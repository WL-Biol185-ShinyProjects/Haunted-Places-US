#Render plot code that draws the plot
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggpie)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(bslib)


#Access the data set
haunted_places <- read.csv("haunted_places.csv")


#Create centuries
by_century <- haunted_places %>%
  filter(!is.na(year), year >= 1700) %>%
  mutate(century = paste0(floor(year/100) * 100, "s"))


# Bar chart of Haunted Places by Entity Type
function(input, output, session){
  output$hauntedBarChart <- renderPlot({
    
    haunted_places %>%
      filter(
        entity_type != "unspecified",
        input$state == "All" | state == input$state
      ) %>%
      mutate(entity_type = fct_infreq(entity_type)) %>%
      ggplot(aes(x = entity_type)) +
      geom_bar(fill = "darkred") +
      labs(
        title = "Haunted Places by Entity Type",
        x = "Entity Type (If Specified)",
        y = "Number of Places"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
        ),
        panel.background = element_blank()
      )
    
  })
  
  
  # Bar chart of Phenomena Type by Division
  output$divisionBarChart1 <- renderPlotly({
    
    # Filter data based on selected division
    if (input$finder_division == "All Divisions") {
      filtered_data <- haunted_places
    } else {
      filtered_data <- haunted_places %>%
        filter(division == input$finder_division)
    }
    
    # Create bar chart
    p <- ggplot(
      filtered_data,
      aes(
        x = phenomena_type,
        fill = division,
        text = paste(
          "Phenomenon Type:", phenomena_type,
          "<br>Division:", division
        )
      )
    ) +
      geom_bar(position = "stack") +
      labs(
        title = "Haunted Places by Phenomenon Type and Division",
        x = "Phenomenon Type",
        y = "Total Number of Hauntings",
        fill = "Census Division"
      ) +
      scale_fill_manual(
        values = c(
          "New England" = "#6A3D9A",
          "Middle Atlantic" = "#1F78B4",
          "East North Central" = "#33A02C",
          "West North Central" = "#B2DF8A",
          "South Atlantic" = "#E31A1C",
          "East South Central" = "#FF7F00",
          "West South Central" = "#FDBF6F",
          "Mountain" = "#A6CEE3",
          "Pacific" = "#B15928"
        )
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 15
        ),
        axis.text.x = element_text(
          angle = 30,
          hjust = 1
        )
      )
    
    # Convert ggplot to interactive Plotly chart
    ggplotly(p, tooltip = "text")
    
  })
  
  # Information about selected bar
  
  output$bar_information <- renderText({
    
    if (input$finder_division == "All Divisions") {
      
      paste(
        "Showing all Census divisions. Hover over the bars to see the phenomenon type and division."
        )
      
    } else {
      
      paste(
        "Currently displaying haunted places in the",
        input$finder_division,
        "Census division. Hover over the bars to explore the phenomenon types."
        
      )
      
    }
    
  })
  
#Pie chart of setting vs state
  
  output$pie <- renderPlotly({
    
    filtered_data <- haunted_places %>%
      filter(
        entity_type != "unspecified",
        input$statepie == "All" | state == input$statepie
      ) %>%
      count(setting)
    
    plot_ly(
      data = filtered_data,
      labels = ~setting,
      values = ~n,
      type = "pie",
      textinfo = "percent",
      hovertemplate = paste(
        "<b>%{label}</b>",
        "<br>Count: %{value}",
        "<br>Percentage: %{percent}",
        "<extra></extra>"
      )
    )
  })
  
  
  output$century_bar <- renderPlot({
    if(input$century == "All Centuries"){
      ggplot(
        by_century %>% filter(input$century == "All Centuries" | century == input$century), 
        aes(century)) + geom_bar() + theme(panel.background = element_blank())
   } else{
      ggplot(by_century %>% filter(century == input$century), 
             aes(fct_infreq(setting))
      ) + geom_bar() + 
       theme(
         panel.background = element_blank()
       )}
  })
  



#leaflet map
  #marker map
  output$hauntingMap <- renderLeaflet({
  
    if(input$map_type == "markers") {
  leaflet(data = haunted_places) %>% 
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addTiles() %>% 
      addMarkers(popup = ~paste0("Location: ", location, "<br/>", "<br/>", "Description:", "<br/>", description), clusterOptions = markerClusterOptions())
    }
    
    else {

  #heat map
    
    leaflet(data = haunted_places) %>% 
      addTiles() %>% 
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addHeatmap(
        data = haunted_places %>%
          filter(!is.na(longitude), !is.na(latitude)),
        lng = ~longitude,
        lat = ~latitude,
        blur = 10,      # how smooth/spread out the heat is
        max = .05,     # intensity ceiling
        radius = 10,    # size of each point's influence
        intensity = 10,
        scaleIntensity = TRUE
        )
  }
})
  
  # Dropdown selection
  observeEvent(input$finder_state, {
    req(input$finder_state)
    
    cities <- sort(unique(
      haunted_places$city[
        haunted_places$state == input$finder_state
      ]
    ))
    
    updateSelectInput(
      session,
      "finder_city",
      choices = c("Select a city" = "", cities),
      selected = ""
    )
  })
  
  
  # Generate city-specific haunted history
  output$finder_results <- renderUI({
    
    req(input$finder_state, input$finder_city)
    
    d <- haunted_places[
      haunted_places$state == input$finder_state &
        haunted_places$city == input$finder_city,
    ]
    
    n_places <- nrow(d)
    
    tagList(
      
      # City introduction
      h2(paste(input$finder_city, input$finder_state, sep = ", ")),
      
      h4("A History That Refuses to Stay Buried"),
      
      p(paste(
        "Our records identify", n_places,
        ifelse(n_places == 1, "haunted location", "haunted locations"),
        "in", input$finder_city, "."
      )),
      
      hr(),
      
      # Summary statistics
      h3("Inside the City's Haunted History"),
      
      fluidRow(
        column(
          width = 6,
          valueBox(
            value = n_places,
            subtitle = "Documented Haunted Places",
            icon = icon("home"),
            color = "purple"
          )
        ),
        column(
          width = 6,
          valueBox(
            value = length(unique(d$location)),
            subtitle = "Unique Locations",
            icon = icon("map-marker"),
            color = "black"
          )
        )
      ),
      
      hr(),
      
      # Individual haunted place stories
      h3("The Stories Behind the Places"),
      
      p("Explore the locations and reported paranormal activity associated with this city."),
      
      lapply(seq_len(n_places), function(i) {
        
        tags$div(
          style = paste(
            "background-color: white;",
            "padding: 15px;",
            "margin-bottom: 15px;",
            "border-left: 4px solid #398DB8;",
            "border-radius: 5px;"
          ),
          
          h4(d$location[i]),
          
          if (!is.na(d$year[i])) {
            tags$p(
              tags$em(paste("Year:", d$year[i]))
            )
          },
          
          tags$p(d$description[i])
        )
      })
    )
  })
  
  
  # Division dropdown selection and bar click feature with corresponding text
  output$divisionBarChart2 <- renderPlot({

    
    req(input$finder_division)
    
    d <- haunted_places %>%
      filter(input$finder_division == "All Divisions" | division == input$finder_division)
    
    ggplot(d, aes(x = phenomena_type)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = paste("Phenomena Types in", input$finder_division),
        x = "Phenomena Type",
        y = "Number of Haunted Places"
      ) +
      theme_minimal()
  })
  
  output$clickDetail <- renderText({
    req(input$plotClick)
    types = sort(unique(haunted_places$phenomena_type))
    i = round(input$plotClick$x)
    type = types[i]
    n <- sum(haunted_places$phenomena_type == type, na.rm = TRUE)
    paste(type, ": ", n, " haunted places in ", input$finder_division)
  })
  

  
  #chloropleth map
  output$chloropleth <- renderLeaflet({
    states_sf <- sf::read_sf("https://rstudio.github.io/leaflet/json/us-states.geojson")
    
    state_count <- haunted_places %>%
      group_by(state) %>%
      summarize(n_rows = n())
    
    map_data <- states_sf %>%
      left_join(state_count, by = c("name" = "state")) %>%
      mutate(n_rows = coalesce(n_rows, 0L))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%d haunted places",
      map_data$name, map_data$n_rows
    ) %>% lapply(htmltools::HTML)
    
    bins <- c(0, 100, 200, 300, 500, 700, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = map_data$n_rows, bins = bins)
    
    leaflet(map_data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(n_rows),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~n_rows, opacity = 0.7, title = NULL,
                position = "bottomright")
  
  })
  
 
}


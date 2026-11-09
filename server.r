#Render plot code that draws the plot
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggpie)
library(leaflet)
library(leaflet.extras)


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
          hjust = 1
        )
      )
    
  })
  
  
# Bar chart of Phenomena Type by Setting
    
    
    output$divisionBarChart <- renderPlot({
      
      ggplot(
        haunted_places,
        aes(
          x = phenomena_type,
          fill = division
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
    })
    
  
#Pie chart of setting vs state
  
  output$pie <- renderPlot({
    ggpie(haunted_places %>%
            filter(
              entity_type != "unspecified",
              input$state == "All" | state == input$state), 
          group_key = "setting",
          count_type  = "full",
          label_info  = "ratio",
          label_type  = "horizon",
          label_pos   = "out",
          label_split = NULL,
          label_size  = 4,
          border_color = "white",
          border_size  = 1
    )  
  })
  
  
  output$century_bar <- renderPlot({
    if(input$century == "All Centuries"){
      ggplot(
        by_century %>% filter(input$century == "All Centuries" | century == input$century), 
        aes(century)) + geom_bar()
   } else{
      ggplot(by_century %>% filter(century == input$century), 
             aes(fct_infreq(setting))
      ) + geom_bar()}
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
  
# dropdown selection
  observeEvent(input$finder_state, {
    req(input$finder_state)
    cities <- sort(unique(haunted_places$city[haunted_places$state == input$finder_state]))
    updateSelectInput(session, "finder_city", choices = cities)
  })
  
  output$finder_results <- renderUI({
    req(input$finder_state, input$finder_city)
    d <- haunted_places[haunted_places$state == input$finder_state &
                        haunted_places$city == input$finder_city,  ]
    tagList(
      h3(nrow(d), ifelse(nrow(d) == 1, "haunted place in", "haunted places in"), input$finder_city),
      tags$ul(lapply(seq_len(nrow(d)), function(i) {
        tags$li(
          tags$b(d$location[i]),
          if (!is.na(d$year[i])) paste0(" (", d$year[i], ")"),
          tags$br(),
          d$description[i]
        )
      }))
    )
  })
  
  
  # Division dropdown selection and bar click feature with corresponding text
  output$divisionBarChart <- renderPlot({

    
    req(input$finder_division)
    
    d <- haunted_places %>%
      filter(division == input$finder_division)
    
    ggplot(d, aes(x = `phenomena_type`)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = paste("Phenomena Types in", input$finder_division),
        x = "Phenomena Type",
        y = "Number of Haunted Places"
      ) +
      theme_minimal()
  })
  output$clickDetail <- renderText({
    input$plotClick
    row=nearPoints(haunted_places,input$plotClick)
    "Hey here is the text"
  })
}
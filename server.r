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
  output$hauntingMap <- renderLeaflet({
  
  leaflet(data = haunted_places) %>% 
      addTiles() %>% 
      addMarkers(popup = ~paste0("Location: ", location, "<br/>", "<br/>", "Description:", "<br/>", description), clusterOptions = markerClusterOptions())
})

  
  #heat map
  output$heatmap <- renderLeaflet({
    
    leaflet(data = haunted_places) %>% 
      addTiles() %>% 
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addHeatmap(
        lng = ~longitude,
        lat = ~latitude,
        blur = 25,      # how smooth/spread out the heat is
        max = 0.05,     # intensity ceiling
        radius = 17    # size of each point's influence
      )
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
      h3(nrow(d), "haunted places in", input$finder_city),
      tags$ul(lapply(d$description, tags$li))
    )
  })
  
  
}  
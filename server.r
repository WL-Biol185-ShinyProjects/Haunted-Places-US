#Render plot code that draws the plot

library(shiny)
library(ggplot2)
library(tidyverse)
install.packages("remotes")
remotes::install_github("showteeth/ggpie")
#^^^this runs every single time the app is created, should probably put this somewhere else
library(ggpie)

haunted_places <- read.csv("haunted_places_clean.csv")

by_century <- haunted_places %>%
  filter(!is.na(year), year >= 1700) %>%
  mutate(century = paste0(floor(year/100) * 100, "s"))


# Plot of Haunted Places by Entity Type
function(input, output){
  output$hauntedBarChart <- renderPlot({
    
    haunted_places_clean %>%
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
  
  
  # Plot of Phenomena Type by Setting
    
    
    output$hauntedBarChart <- renderPlot({
      
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
        scale_fill_brewer(palette = "Set2") +
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
    
  
  
  
  #setting vs state pie chart
  
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
  
}
    
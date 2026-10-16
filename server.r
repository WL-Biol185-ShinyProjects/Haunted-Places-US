#Render plot code that draws the plot

library(shiny)
library(ggplot2)
library(tidyverse)

haunted_places <- haunted_places_clean

# Plot of Haunted Places by State  
function(input, output) {
  
  output$hauntedBarChart <- renderPlot({
    
    haunted_places_clean %>%
      ggplot(aes(x = state)) +
      geom_bar(fill = "darkred") +
      labs(
        title = "Haunted Places by State",
        x = "state",
        y = "setting"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
}

# Plot of Haunted Places by Entity Type
function(input, output) {  
  output$hauntedBarChart <- renderPlot({
    
    haunted_places_clean %>%
      filter(entity_type != "unspecified") %>%
      mutate(entity_type = fct_infreq(entity_type)) %>%
      ggplot(aes(x = entity_type)) +
      geom_bar(fill = "darkred") +
      labs(
        title = "Haunted Places by Entity Type",
        x = "Entity Type (If Specified)",
        y = "Number of Places"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}


#setting vs state pie chart

function(input, output) {  
  output$pie <- renderPlot({
ggpie(haunted_places %>%
      filter(state == input$state), 
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
}
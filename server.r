#Render plot code that draws the plot

library(shiny)
library(ggplot2)
library(tidyverse)

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
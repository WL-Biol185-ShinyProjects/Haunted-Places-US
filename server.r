#Render plot code that draws the plot

library(shiny)
library(ggplot2)
library(tidyverse)

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
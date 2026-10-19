#reserves spot on website page for the elements

library(shiny)
library(ggplot2)

haunted_places <- read.csv("haunted_places_clean.csv")


    fluidPage(
    titlePanel("Haunted Places by State"),
    sidebarLayout(
      sidebarPanel(
        h3("State Comparison"),
        p("Compare the number of haunted places recorded in each state.")
      ),
      mainPanel(
        plotOutput("hauntedBarChart")
      )
    ),
  
#pie chart
    titlePanel("Haunted Places by State"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "state",
          label = "State:",
          choices = c("All States", sort(unique(haunted_places$state))),
          selected = "All States"
        )
      ),
        mainPanel(
          plotOutput("pie")
          )
        )
      )

#reserves spot on website page for the elements

library(shiny)
library(ggplot2)

title = "hauntedBarAnalysis"
shinyUI(
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
    )
  )
)

#pie chart

shinyUI(
  fluidPage(
    titlePanel("Haunted Places by State"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "state",
          label = "State:",
          choices = sort(unique(haunted_places$state)),
          selected = "Alabama"
        )
      ),
        mainPanel(
          plotOutput("pie")
          )
        )
      )
    )
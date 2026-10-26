library(shiny)
library(ggplot2)

haunted_places <- read.csv("haunted_places_clean.csv")

ui <- navbarPage(
  title = "Haunted Places Explorer",
  
  # Page 1: Introduction
  tabPanel(
    "Introduction",
    h1("Welcome to the Haunted Places Explorer"),
    p("Explore haunted places across the United States."),
    p("This interactive dashboard lets you investigate haunted locations, compare states, and discover regional patterns.")
  ),
  
  # Page 2: Data Overview
  tabPanel(
    "Data Overview",
    h1("Haunted Places by State"),
    p("Compare the number of haunted places recorded in each state."),
    plotOutput("hauntedBarChart")
    # plotOutput("pie")
  ),
  
  # Page 3: Haunting Finder
  tabPanel(
    "Haunting Finder",
    h1("Find Haunted Places by U.S. City"),
    p("Filter to a state and type in your city to find haunted places close by!")
  ),
  
  # Page 4: Regions
  tabPanel(
    "Regions",
    h1("Explore Haunted History by Region"),
    p("Compare haunted history across different regions of the United States."),
    titlePanel("Setting and Phenomena Type"),
    sidebarLayout(
      sidebarPanel(
        h3("Setting Percentage Comparison"),
        p("Compare the % of each phenomena type in each setting.")
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
    ),

titlePanel("Count by Century"),
sidebarLayout(
  sidebarPanel(
    selectInput(
      inputId = "century",
      label = "Century:",
      choices = c("All Centuries", sort(unique(by_century$century))),
      selected = "All Centuries"
    )
  ),
  mainPanel(
    plotOutput("century_bar")
  )
)

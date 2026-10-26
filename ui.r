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
    
    selectInput(
      inputId = "state",
      label = "Filter by State:",
      choices = c(
        "All States" = "All",
        sort(unique(haunted_places_clean$state))
      ),
      selected = "All"
    ),
    plotOutput("hauntedBarChart"),
    h2("Haunted Places by Entity Type"),
    p("See the distribution of haunted places by entity type."),
    plotOutput("pie")
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
        plotOutput("phenomenatype_setting")
      )
    )
  )
)
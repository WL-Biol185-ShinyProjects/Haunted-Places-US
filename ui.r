library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

haunted_places <- read.csv("haunted_places.csv")

by_century <- haunted_places %>%
  filter(!is.na(year), year >= 1700) %>%
  mutate(century = paste0(floor(year/100) * 100, "s"))

ui <- navbarPage(
  title = "Haunted Places Explorer",
  
# Page 1: Introduction
  tabPanel(
    "Where is America Haunted?",
    h1("Welcome to the Haunted Places Explorer"),
    p("Explore haunted places across the United States."),
    p("This interactive dashboard lets you investigate haunted locations, compare states, and discover regional patterns."),
 
       
    h2("Map of Hauntings"),
    selectInput(
      inputId = "map_type",
      label = "Select Map Style",
      choices = c("Heat Map" ="heat", 
                  "Individual Markers" = "markers"),
      selected = "heat"
    ),
    leafletOutput("hauntingMap"), 
    
    ),

 
  
  
# Page 2: Data Overview
  tabPanel(
    "What Kinds of Places are Haunted?",
      h1("Haunted Places by State"),
      p("Compare the number of haunted places recorded in each state."),
    
      selectInput(
        inputId = "state",
        label = "Filter by State:",
        choices = c(
          "All States" = "All",
          sort(unique(haunted_places$state))
        ),
        selected = "All"
      ),
    plotOutput("hauntedBarChart"),
    
    h2("Haunted Places by Entity Type"),
    p("See the distribution of haunted places by entity type."),
    plotOutput("pie"),
    
    h3("Count By Century"),
    p("See the concentration by century and drop down into types of hauntings by century"),
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
  ),
  
# Page 3: Haunting Finder
  tabPanel(
    "Haunting Finder",
    h1("Find Haunted Places by U.S. City"),
    p("Filter to a state and type in your city to find haunted places close by!"),
    selectInput("finder_state", "State:",
                choices = c("Select a state" = "", sort(unique(haunted_places$state)))),
    selectInput("finder_city", "City:", choices = NULL),
    uiOutput("finder_results")
  ),
  
# Page 4: Division
  tabPanel(
    "Places by Region",
    h1("Explore Haunted History by Region"),
    p("Compare the four phenomenon types across U.S. Census regions"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Phenomenon by Region"),
        p("Each bar represents a phenomenon type, with colored segments showing the number of haunted places in each Census region")
      ),
      
      mainPanel(
        plotOutput("divisionBarChart", height = "500px")
      )
    )
  )

)


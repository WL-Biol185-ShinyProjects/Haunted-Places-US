library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(shinydashboard)

haunted_places <- read.csv("haunted_places.csv")


by_century <- haunted_places %>%
  filter(!is.na(year), year >= 1700) %>%
  mutate(century = paste0(floor(year/100) * 100, "s"))


ui <- dashboardPage(
  dashboardHeader(title = "Haunted Places in the US"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Where is America Haunted?", tabName = "IntroPage"),
                menuItem("What kinds of Places are Haunted?", tabName = "TotalbySetting"),
                menuItem("Do Regions have Concentrations?", tabName = "DivisionBarChart"),
                menuItem("Types of Phenomena by Region", tabName = "GroupbyRegion"),
                menuItem("What Kind of Haunting are you Likely to Experience?", tabName = "StateCityDropdown"),
                menuItem("About our Team", tabName= "MeetourTeam")
    )
  ),

dashboardBody(
  tabItems(
  
    
    #Page 1: Where is America Haunted?
    tabItem(tabName = "IntroPage",
            h1("Welcome to the Haunted Places Explorer"),
            p("Explore haunted places across the United States."),
            p("This interactive dashboard lets you investigate haunted locations, compare states, and discover regional patterns."),
            fluidRow(
              box(
                title = "Map of Hauntings", width = 12, solidHeader = TRUE,
                selectInput(
                  inputId = "map_type",
                  label = "Select Map Style",
                  choices = c("Heat Map" = "heat",
                              "Individual Markers" = "markers"),
                  selected = "heat"
                ),
                leafletOutput("hauntingMap")
              )
            )

    ),
  
  #Page 2: What kinds of places are haunted?
  tabItem(tabName = "TotalbySetting",
          h1("Haunted Places by State"),
          p("Compare the number of haunted places recorded in each state."),
          fluidRow(
            box(
              width = 12,
              selectInput(
                inputId = "state",
                label = "Filter by State:",
                choices = c("All States" = "All",
                            sort(unique(haunted_places$state))),
                selected = "All"
              ),
              plotOutput("hauntedBarChart")
            )
          ),
          fluidRow(
            box(
              title = "Haunted Places by Entity Type", width = 12, solidHeader = TRUE,
              p("See the distribution of haunted places by entity type."),
              plotOutput("pie")
            )
          ),
          fluidRow(
            box(
              title = "Count By Century", width = 4, solidHeader = TRUE,
              p("See the concentration by century and drop down into types of hauntings by century."),
              selectInput(
                inputId = "century",
                label = "Century:",
                choices = c("All Centuries", sort(unique(by_century$century))),
                selected = "All Centuries"
              )
            ),
            box(
              width = 8,
              plotOutput("century_bar")
            )
          )
  ),
  
  
  #Page 3: Do Divisions have concentrations?
  tabItem(tabName = "DivisionBarChart",
          h1("Explore Haunted History by Region"),
          p("Compare the four phenomenon types across U.S. Census regions."),
          fluidRow(
            box(
              title = "Phenomenon by Region", width = 4, solidHeader = TRUE,
              p("Each bar represents a phenomenon type, with colored segments showing the number of haunted places in each Census region.")
            ),
            box(
              width = 8,
              plotOutput("divisionBarChart", height = "500px")
            ),
            
          #setting by division
          #choropleth map of the divisions, colored by count of hauntings 
          fluidRow(
            box(
          h1("Concentration Map"),
          leafletOutput("chloropleth")
          )
          )
          )
  ),
  
  
  #Page 4: Types of phenomena by division
  tabItem(tabName = "GroupbyRegion",
          h1("Explore Haunted History by Division"),
          p("Compare the four phenomena types across U.S. Divisions"),
          sidebarLayout(
            sidebarPanel(
              h3("Phenomenon by Division"),
              p("Each bar represents a phenomenon type, with colored segments showing the number of haunted places in each Census division")),
              
          mainPanel(
          fluidRow(
            box(
              title = "Count By Century", width = 4, solidHeader = TRUE,
              p("See the concentration by century and drop down into types of hauntings by century."),
              selectInput(
                inputId = "finder_division",
                label = "Select a Division",
                choices = c("All Divisions", sort(unique(haunted_places$division))),
                selected = "All Divisions"
              )
            )
          ),
            box(
              height = "500px",
              plotOutput("divisionBarChart")
          )
          )
          
          ),
          # phenomena by division plot goes here

  )
  ),
  

  #  sidebarLayout(
  #    sidebarPanel(
  #      h3("Phenomenon by Division"),
  #      p("Each bar represents a phenomenon type, with colored segments showing the number of haunted places in each Census division")
  #    ),
  
  
  
  
  
  #Page 5: How haunted is your area?
  tabItem(tabName = "StateCityDropdown",
          h1("Find Haunted Places by U.S. City"),
          p("Filter to a state and type in your city to find haunted places close by!"),
          fluidRow(
            box(
              width = 4,
              selectInput("finder_state", "State:",
                          choices = c("Select a state" = "",
                                      sort(unique(haunted_places$state)))),
              selectInput("finder_city", "City:", choices = NULL)
            ),
            box(
              width = 8,
              uiOutput("finder_results")
            )
          )
  )
),


  #Page 6: About the Team 
  tabItem(tabName = "MeetourTeam",
  h1("Meet the Team"),
  sidebarLayout(
    sidebarPanel(
      h3("Meet the Team"),
      p("Team information goes here.")
    ),
    
    mainPanel(
      h3("About Our Project"),
      p("Project description goes here.")
    )
  )
)
          
          
          
)




library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(bslib)

haunted_places <- read.csv("haunted_places.csv")


by_century <- haunted_places %>%
  filter(!is.na(year), year >= 1700) %>%
  mutate(century = paste0(floor(year/100) * 100, "s"))


ui <- dashboardPage(
  dashboardHeader(title = NULL),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Where is America Haunted?", tabName = "IntroPage"),
                menuItem("What Kinds of Places are Haunted?", tabName = "TotalbySetting"),
                menuItem("Do Regions have Concentrations?", tabName = "DivisionBarChartTab"),
                menuItem("Types of Phenomena by Division", tabName = "GroupbyRegion"),
                menuItem("Hauntings Near You", tabName = "StateCityDropdown"),
                menuItem("About our Team", tabName= "MeetourTeam")
    )
  ),

dashboardBody(
  tags$head(
    tags$style(HTML("
    /* Hide the original title area */
    .main-header .logo {
      display: none;
    }

    /* Expand the navbar */
    .main-header .navbar {
      margin-left: 0 !important;
      background-color: #398DB8;
    }

    /* Keep the hamburger menu on the left */
    .main-header .navbar .sidebar-toggle {
      float: left;
      color: white;
    }

    /* Add the title to the right of the hamburger */
    .main-header .navbar:after {
      content: 'Haunted Places in the United States';
      position: absolute;
      left: 60px;
      top: 0;
      line-height: 50px;
      color: white;
      font-size: 18px;
      font-weight: 400;
    }
  "))
  ),
  tabItems(
  
    
    # Page 1: Where is America Haunted?
    tabItem(tabName = "IntroPage",
            
            h1("Welcome to the Haunted Places Explorer"),
            
            p("Explore haunted places across the United States."),
            
            p("This interactive dashboard lets you investigate haunted locations, compare states, and discover regional patterns."),
            
            # Text Cards
            fluidRow(
              
              valueBox(
                value = "50",
                subtitle = "States to Explore",
                icon = icon("map-marked-alt"),
                color = "light-blue",
                width = 4
              ),
              
              valueBox(
                value = "Haunted Locations",
                subtitle = "Discover America's spooky history",
                icon = icon("ghost"),
                color = "light-blue",
                width = 4
              ),
              
              valueBox(
                value = "Explore",
                subtitle = "Find haunted places near you",
                icon = icon("search"),
                color = "light-blue",
                width = 4
              )
              
            ),
            
            # Map
            fluidRow(
              box(
                title = "Map of Hauntings",
                width = 12,
                solidHeader = TRUE,
                
                selectInput(
                  inputId = "map_type",
                  label = "Select Map Style",
                  choices = c(
                    "Heat Map" = "heat",
                    "Individual Markers" = "markers"
                  ),
                  selected = "heat"
                ),
                
                leafletOutput("hauntingMap")
              )
            ),
            
        
            # Homepage Text Cards
            fluidRow(
              
              box(
                title = "Data Deep Dive",
                width = 4,
                solidHeader = TRUE,
                status = "danger",
                
                p("Explore haunted locations across the United States:"),
                
                tags$ul(
                  tags$li("Identify patterns in haunted locations"),
                  tags$li("Compare haunted locations across states"),
                  tags$li("Discover regional concentrations of hauntings")
                )
              ),
              
              box(
                title = "Why Should You Care?",
                width = 4,
                solidHeader = TRUE,
                status = "danger",
                
                p("Haunted places offer a fascinating look into American history:"),
                
                tags$ul(
                  tags$li("Explore historical events and local legends"),
                  tags$li("Understand the cultural significance of haunted places"),
                  tags$li("Discover how stories shape our communities")
                )
              ),
              
              box(
                title = "Explore the Unknown",
                width = 4,
                solidHeader = TRUE,
                status = "danger",
                
                p("Discover the stories behind America's most infamous locations:"),
                
                tags$ul(
                  tags$li("Explore mysterious hotels and historic landmarks"),
                  tags$li("Investigate haunted locations across the country"),
                  tags$li("Discover which states have the most reported hauntings")
                )
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
            column(12,
              sidebarLayout(
                sidebarPanel(
                  title = "Haunted Places by Entity Type",
                  p("See the distribution of haunted places by entity type."),
              selectInput(
                inputId = "statepie",
                label = "Filter by State:",
                choices = c("All States" = "All",
                            sort(unique(haunted_places$state))),
                selected = "All"
                )
                ),
              
              mainPanel(
              plotlyOutput("pie")
              )
            )
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
  tabItem(tabName = "DivisionBarChartTab",
          h1("Explore Haunted History by Region"),
          p("Compare the four phenomenon types across U.S. Census regions."),
          fluidRow(
            box(
              title = "Phenomenon by Region", width = 4, solidHeader = TRUE,
              p("Each bar represents a phenomenon type, with colored segments showing the number of haunted places in each Census region.")
            ),
            box(
              width = 8,
              plotlyOutput("divisionBarChart1", height = "500px")
            ),
            
          #setting by division
          #choropleth map of the divisions, colored by count of hauntings. need to fix api key thing
          fluidRow(
            box(
              width = 12,
          h1("Concentration Map"),
          p("Explore concentrations on a state by state basis."),
          leafletOutput("chloropleth")
          )
          )
          )
  ),
  
  
  
  # Page 4: Types of phenomena by division
  
  tabItem(
    tabName = "GroupbyRegion",
    
    h1("Explore Haunted History by Division"),
    
    p("Compare the four phenomena types across U.S. Divisions."),
    
    sidebarLayout(
      sidebarPanel(
        h3("Phenomenon by Division"),
        p("Each bar represents a phenomenon type, with colored segments showing the number of haunted places in each Census division."),
        
        selectInput(
          inputId = "finder_division",
          label = "Select a Division",
          choices = c("All Divisions",
                      sort(unique(haunted_places$division))),
          selected = "All Divisions"
        )
      ),
      
      mainPanel(
        box(
          title = "Phenomena by Division",
          width = 12,
          solidHeader = TRUE,
          plotOutput("divisionBarChart2", height = "500px", click = "plotClick"),
          textOutput("clickDetail")
        ),
        
        box(
          title = "Information About Selected Bar",
          width = 12,
          solidHeader = TRUE,
          textOutput("bar_information")
        )
      )
    )
  ),
  
  
  # Page 5: How haunted is your area?
  tabItem(
    tabName = "StateCityDropdown",
    h1("Find Haunted Places by U.S. City"),
    p("Filter to a state and type in your city to find haunted places close by!"),
    
    fluidRow(
      box(
        width = 4,
        selectInput(
          "finder_state",
          "State:",
          choices = c("Select a state" = "",
                      sort(unique(haunted_places$state)))
        ),
        selectInput("finder_city", "City:", choices = NULL)
      ),
      
      box(
        width = 8,
        uiOutput("finder_results")
      )
    )
  ),
  
  # Page 6: About the Team
  tabItem(
    tabName = "MeetourTeam",
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
  
  # Close tabItems
  )
)

# Close dashboardPage
)

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
            #  menuItem("Do Regions have Concentrations?", tabName = "DivisionBarChartTab"),
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
                title = "Quick Facts about the Data",
                style = "font-size: 20px ,margin-bottom: 10px;",
                width = 4,
                solidHeader = TRUE,
                status = "danger",
                
                
                tags$ul(
                  tags$li("The earliest recorded haunting date is", strong("1500"), "with two hauntings this year (New York and South Carolina)"),
                  tags$li(strong("California"), "has the highest number of total hauntings with a total of", strong ("1,070")," across all time periods"),
                  tags$li(strong("Schools"), "are the most common setting for hauntings across the US with a total count of", strong("2,325"))
                )
              ),
              
              box(
                title = "Why Should You Care?",
                width = 4,
                style = "font-size: 20px ,margin-bottom: 10px;",
                solidHeader = TRUE,
                status = "danger",
                
                p("Haunted places offer a fascinating look into American history:"),
                
                tags$ul(
                  tags$li("Uncover significant historical hauntings right near you!"),
                  tags$li("Explore what kinds of hauntings have shaped history across the US"),
                  tags$li("Discover haunting hot-spots and trends across time")
                )
              ),
              
              box(
                title = "Explore the Unknown",
                width = 4,
                style = "font-size: 20px ,margin-bottom: 10px;",
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
          h1("What kinds of places are haunted?"),
          p("Haunted places are found in all kinds of locations across America. From schools and cemeteries to private homes and hotels, reports of hauntings span a wide variety of settings."),
          p("But are some places more commonly associated with hauntings than others? Explore the data below to find out!"),
          #br(), what does this do?
          
          #CHART 1: ENTITY TYPES
          fluidRow(
            box(
              title = "1. What Types of Places are Haunted?",
              width = 12,
              solidHeader = TRUE,
              
              p("Some types of locations appear more frequently in the haunted-place records than others. Use the filter below to explore how these patterns vary by state."),
              
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
          #br(),
          
          #CHART 2: SETTINGS
          fluidRow(
            box(
              title = "2. What Settings are Most Commonly Haunted?",
              width = 12,
              solidHeader = TRUE,
              
              p("Not all haunted places are the same. While cemeteries are often associated with ghost stories, hauntings are also recorded in everyday locations like schools, houses, and hotels."),
              p("Explore the distribution of haunted place settings and hover over individual slices to see their counts and percentages."),
              
              selectInput(
                inputId = "statepie",
                label = "Filter by State:",
                choices = c(
                  "All States" = "All",
                  sort(unique(haunted_places$state))
                ),
                selected = "All"
              ),
              
              plotlyOutput("pie", height = "450px")
            )
          ),
          #br(),
          
          #CHART 3: HISTORICAL DISTRIBUTION
          fluidRow(
            box(
              title = "3. When do These Haunted Places Originate?",
              width = 12, 
              solidHeader = TRUE,
              
              p("Haunted place records span multiple centuries. Looking at their historical distribution helps us understand which time periods are most represented in the dataset."),
              p("Select a century to explore the distribution of haunted paces across historical periods."),
              
              selectInput(
                inputId = "century",
                label = "Filter by Century:",
                choices = c("All Centuries", sort(unique(by_century$century))),
                selected = "All Centuries"
              ),
              
              plotOutput("century_bar", height = "400px")
            )
          ),
          
          #br()
          
          #CONCLUSION
          fluidRow(
            box(
              title = "What Does the Data Tell Us?",
              width = 12,
              solidHeader = TRUE,
              
              p("The distribution of haunted places reveals that reports of hauntings are associated with a wide variety of locations and historical periods."),
              p("By comparing entity types, settings, and centuries, we can ebtter understand the patterns found within America's haunted place records.")
            )
          )
  ),
            
  
  
#Page 3: Do Divisions have concentrations?
#  tabItem(tabName = "DivisionBarChartTab",
#          h1("Explore Haunted History by Region"),
#          p("Compare the four phenomenon types across U.S. Census regions."),
#          fluidRow(
#            box(
#              title = "Phenomenon by Region", width = 4, solidHeader = TRUE,
#              p("Each bar represents a phenomenon type, with colored segments showing the number of haunted places in each Census region.")
#            ),
#            box(
#              width = 8,
#              plotlyOutput("divisionBarChart1", height = "500px")
#            ),
#         
#          )
#  ),
  
  
  
# Page 4: Types of phenomena by division
  tabItem(
    tabName = "GroupbyRegion",
    
    h1("Explore Haunted History by Division"),
    p("From the Pacific Coast to New England, reports of paranormal activity appear ascross the United States. But, are some regions more haunted than others? Explore the geographic patterns and types of phenomena reported throughout the country."),
    br(),
    
    #setting by division
    #choropleth map of the divisions, colored by count of hauntings. need to fix api key thing
    fluidRow(
      box(
        width = 12,
        title = "Where are Haunted Places Concentrated?",
        status = "primary",
        solidHeader = TRUE,
        
        p("Some states have far more documented haunted locations than others. Explore the map to see where reports of paranormal activity are most concentrated."),
        p(em("Darker shades represent a greater number of documented haunted places.")),

        leafletOutput("chloropleth", height = "400px")
      )
    ), 
    
    br(),
    
    #section 2
    h2("Do Different Regions Experience Different Hauntings?"),
    p("Beyond where hauntings occur, the types of paranormal experiences reported may also reveal interesting regional patterns. Compare auditory, physical, tactile, and visual phenomena across the nine U.S. Census divisions."),
    p("Select a region to investigate its paranormal profile, or explore all divisions to see the national picture."),
    
    sidebarLayout(
      sidebarPanel(
        h3("Explore a Division"),
        
        p("Choose a Census division to narrow your exploration to a specific part of the country."),
        
        selectInput(
          inputId = "finder_division",
          label = "Select a Division:",
          choices = c(
            "All Divisions",
            sort(unique(haunted_places$division))
            ),
          selected = "All Divisions"
      ),
      br(),
      
      p(em("Tip: Hover over the bars to explore the data and click to learn more."))
    ),
      
      mainPanel(
        box(
          title = "Types of Paranormal Phenomena",
          width = 12,
          solidHeader = TRUE,
          
          p("Which experiences are reported most frequently? Compare the different categories of paranormal activity in your selected region."),
          plotOutput(
            "divisionBarChart2", 
            height = "450px", 
            click = "plotClick"),
          
          textOutput("clickDetail")
        ),
        
        box(
          title = "Discover the Details",
          width = 12,
          solidHeader = TRUE,
          
          p("Select a bar in the chart to uncover more about the reported paranormal activity in this region."),
          textOutput("bar_information")
        )
      )
    )
    ),
  
  
# Page 5: How haunted is your area?
  tabItem(
    tabName = "StateCityDropdown",
    h1("Find Haunted Places by U.S. City"),
    p("Every city has a history. Some are harder to explain than others. Explore the haunted places and paranormal stories hiding in communities across America."),    
    fluidRow(
      box(
        width = 3,
        title = "Choose Your Desitnation",
        status = "primary",
        solidHeader = TRUE,
        
        p("Where would you like to investigate?"),
        
        selectInput(
          "finder_state",
          "State:",
          choices = c("Select a state" = "",
                      sort(unique(haunted_places$state)))
        ),
        
        selectInput("finder_city", "City:", choices = NULL)
      ),
      
      box(
        width = 9,
        title = "Your City's Haunted History",
        status = "primary",
        solidHeader = TRUE,
        uiOutput("finder_results")
      )
    )
  ),
  

# Page 6: About the Team
tabItem(
  tabName = "MeetourTeam",
  
  h1("Meet the Team"),
  p("Get to know the people behind our project!"),
  
  fluidRow(
    
    # Team Member 1
    box(
      title = "",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      
      div(
        align = "center",
        img(
          src = "Team_member1.PNG",
          height = "180px",
          width = "180px",
          style = "border-radius: 50%; object-fit: cover;"
        )
      ),
      
      h3("Mackenzie Moore"),
      p("Washington and Lee Class of 2027"),
      p("Accounting major | Data Science minor")
    ),
    
    # Team Member 2
    box(
      title = "",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      
      div(
        align = "center",
        img(
          src = "Team_member2.PNG",
          height = "180px",
          width = "180px",
          style = "border-radius: 50%; object-fit: cover;"
        )
      ),
      
      h3("Raleigh Craft"),
      p("Washington and Lee Class of 2027"),
      p("Business Administration major | Data Science minor")
    ),
    
    # Team Member 3
    box(
      title = "",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      
      div(
        align = "center",
        img(
          src = "Team_member3.PNG",
          height = "180px",
          width = "180px",
          style = "border-radius: 50%; object-fit: cover;"
        )
      ),
      
      h3("Kelcie Comisac"),
      p("Washington and Lee Class of 2027"),
      p("Economics major | Data Science, Art History minors")
    )
    
  )
)

  # Close tabItems
  )
)


# Close dashboardPage
)

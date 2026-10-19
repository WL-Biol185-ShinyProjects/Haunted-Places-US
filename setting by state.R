library(tidyverse)
library(ggplot2)
library(ggpie)



# output$pie <- renderPlot()

  
ggplot(reactive({
  haunted_places %>%
    filter(state == input$state) %>%
    mutate(setting = fct_lump_n(setting, n = input$slices,
                                other_level = "other settings")),
  
})
)

# table pipe into filter by state into groupby and summarize, then pipe into the pie chart

library(tidyverse)
library(ggplot2)
install.packages("remotes")
remotes::install_github("showteeth/ggpie")
library(ggpie)

haunted_places <- read.csv("haunted_places_clean.csv")


ggpie(reactive({haunted_places %>%
      filter(state == input$state)}), 
  group_key = "setting",
  count_type  = "full",
  label_info  = "ratio",
  label_type  = "horizon",
  label_pos   = "out",
  label_split = NULL,
  label_size  = 4,
  border_color = "white",
  border_size  = 1
  )  
  




by_century <- haunted_places %>%
  filter(!is.na(year), year >= 1700) %>%
  mutate(century = paste0(floor(year/100) * 100, "s"))

ggplot(by_century, aes(century)) + geom_bar()

#ui

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
    plotOutput("XYZ")
  )
)

#server

output$century_bar <- renderPlot({
  if(input$century == "All Centuries"){
  ggplot(
  by_century %>% filter(input$century == "All Centuries" | century == input$century), 
  aes(century)) + geom_bar()
    } else{
      ggplot(by_century %>% filter(century == input$century), 
             aes(fct_infreq("setting"))
             ) + geom_bar()
    
})
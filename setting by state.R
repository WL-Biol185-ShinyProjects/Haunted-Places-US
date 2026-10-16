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

haunted_places <- haunted_places_clean


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
  
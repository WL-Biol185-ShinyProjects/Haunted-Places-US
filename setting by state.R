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



ggplot(
  haunted_places %>%
    filter(state == "Alabama"), 
  aes("setting", "Alabama")  
  ) + geom_bar()
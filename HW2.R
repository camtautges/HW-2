
#title: "HW 2"
#author: "camryn tautges"
#date: "2023-03-09"


library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
library(knitr)

wine = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTqKLqrVrRqzRC-Hb3z-THqlvA7KVAhYk3plSnpT-pE-z4OEklcXt4Ed5lr-Xr_t6nIZUURMDJrmIMC/pub?gid=63819209&single=true&output=csv")

#data already grouped by year
wine = wine %>%
  #get rid of columns i dont want
  select(-CommodityCode, -CropName, -Unit) %>%
  #rename production to something easier to understand
  rename(tonsProduced = Production)



#create the scatter function to use in app
scatterplot <- function(result) {
  x =
    ggplot(mapping = aes(x = Year, y = tonsProduced, col = County)) +
    geom_point(data = result) +
    labs(x = "Year", y = "Tons Produced", title = "Year vs. Tons Produced") +
    #choose color palette for legend
    scale_color_brewer(palette="Dark2")
}

# create the app
ui <- fluidPage(
  #app title
  titlePanel("California Wine Production 1980-2020"),
  #counties wanted
  selectizeInput("county", "Choose the county", unique(wine$County), multiple = TRUE), 
  #years wanted
  sliderInput("year", "Year", min(wine$Year), max(wine$Year), c(1980, 2020), sep = "", step = 1),
  #plot output
  plotlyOutput("scatter"),
  dataTableOutput("dt")
)

#create server
server <- function(input, output) {
  #update df reactively
  result = reactive({ 
    wine  %>% 
      #check for years
      filter(between(Year, input$year[1], input$year[2])) %>%
      #check for counties
      filter(County %in% input$county)
  })
  
  #plots data chosen
  output$scatter <- renderPlotly({
    scatterplot(result())
  })
  #produces data table of chosen data
  output$dt = renderDataTable(result())
}

shinyApp(ui, server)

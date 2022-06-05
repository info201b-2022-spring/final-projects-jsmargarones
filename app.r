library('shiny')
library("ggplot2")
library("dplyr")
library('tidyverse')
library('reshape2')
library('shinyWidgets')


automaker_stock <- read.csv("/Users/josiemargarones/Downloads/automakers_stocks.csv")

automaker_stock <- automaker_stock %>%
  mutate(avg_stock = (High + Low)/2)
  #filter(cummax(Date == 01/01/20))

ui <- fluidPage(
  titlePanel("Car Companies Average Stock"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "cars", 
                 label = "Select Car Brand:",
                 choices = unique(automaker_stock$Symbol),
                 # default is Tesla because thats the stock we're most interested in
                 selected = "TSLA",
                 multiple = TRUE)
  ),
  mainPanel(
    plotOutput("line")
  )
  )
)

server <- function(input, output) {
  output$line <- renderPlot( {
    ggplot(automaker_stock %>% filter(Symbol == input$cars),
    aes(x = Date, y = avg_stock, group = Symbol)) +
      geom_line(aes(color = Symbol)) +
      ggtitle("Comparison of Tesla, Lucid Motor, and Volkswagen Stock") +
      ylab("Average Stock Price") +
      xlab("Date")
  })
}
  
shinyApp(ui = ui, server = server)
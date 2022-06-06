library(shiny)
library(ggplot2)
library(dplyr)

tesla <- read.csv("/Users/josiemargarones/Downloads/tesla.csv")

tesla <- tesla %>%
  mutate(difference_daily = High - Low)


ui <- fluidPage(
  dateRangeInput("daterange", "Select the date Range(2020/09/18 - 2021/08/23)",
                 start = min(tesla$Date),
                 end = max(tesla$Date),
                 min = min(tesla$Date),
                 max = max(tesla$Date),
                 separator = " - ", format = "dd/mm/yy"),
  plotOutput("barplot"))


server <- function(input, output) {
  dateRangeInput<-reactive({
    dataset <- subset(tesla, Date >= input$daterange[1] & Date <= input$daterange[2]) 
  })
    
  output$barplot <-renderPlot({
    ggplot(data = tesla, aes(x = Date, y = difference_daily), )  +
      geom_bar(stat="identity") +
      labs(title="Tesla stock high vs low on given date", x ="Date") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$x.pos <- renderText(paste0("x = ",input$plot_hover$x))
  output$y.pos <- renderText(paste0("y = ",input$plot_hover$y))
}
shinyApp (ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

rank <- read.csv("/Users/ziqiangwei/Downloads/Largest automakers by market capitalization.csv")
tesla <- read.csv("/Users/ziqiangwei/Downloads/tesla.csv")
tsla <- read.csv("/Users/ziqiangwei/Downloads/TSLA.csv")
lucid <- read.csv("/Users/ziqiangwei/Downloads/lucid motors.csv")
volkswagen <- read.csv("/Users/ziqiangwei/Downloads/Volkswagen.csv")


# finds earliest lucid date
lucid_earliest <- lucid %>%
  filter(Date == min(Date, na.rm = TRUE)) %>%
  pull(Date)

tesla <- tesla %>%
  # making it so tesla and lucid start at the same date
  filter(cummax(Date == lucid_earliest) > 0)  %>%
  # adding col with the average price of the day for Tesla stock
  mutate(avg_price_tesla = (High + Low)/2)

#add average price for lucid stock each day
lucid <- lucid %>%
  mutate(avg_price_lucid = (High + Low)/2)

volkswagen <- volkswagen %>%
  # making it so tesla and lucid start at the same date
  filter(cummax(Date == lucid_earliest) > 0)  %>%
  # adding col with the average price of the day for Tesla stock
  mutate(avg_price_vollkswagen = (High + Low)/2)

# merges all the data into one table
all_data <- merge(tesla, lucid, by = 'Date')
all_data <- merge(all_data, volkswagen, by = 'Date')
filtered_date <- filter(all_data, Date >= "2021-08-01")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Comparison of stock prices of Tesla and Vollkswagen"),
  sidebarLayout(
    sidebarPanel(
      h5("Controls"),
      sliderInput(
        inputId = "provety",
        label = "Filter by max poverty percentage:",
        min = 0,
        max = 16,
        value = 16
      )
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$histogram <- renderPlot({
    ggplot(data = filtered_date) +
      geom_bar(aes(x = Date, y = avg_price_tesla), stat = "identity", fill = "skyblue")+
      geom_bar(aes(x = Date, y = avg_price_vollkswagen), stat = "identity", fill = "yellow")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

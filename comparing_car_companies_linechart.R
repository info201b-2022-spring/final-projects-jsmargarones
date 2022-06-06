# This Line Chart compares Tesla’s stocks to different companies that are in direct competition with Tesla. 
# Volkswagen is a leading, mainly gas car company while Lucid Motors is a rising electric car company
# the colors are chosen because Tesla's symbol is red, Volkswagen's is blue, and green contrasts with those 
# bc Lucid Motors main color is black

library("ggplot2")
library("dplyr")
library('tidyverse')
library('shiny')


lucid <- read.csv("/Users/quinnrosenberg/Downloads/lucid motors.csv")
volkswagen <- read.csv("/Users/quinnrosenberg/Downloads/Volkswagen.csv")

tesla <- read.csv("/Users/quinnrosenberg/Downloads/tesla.csv")
lucid <- read.csv("/Users/quinnrosenberg/Downloads/lucid motors.csv")
volkswagen <- read.csv("/Users/quinnrosenberg/Downloads/Volkswagen.csv")


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

# colors declared before ggplot so the legend corresponds
colors <- c("Tesla" = "red", "Volkswagen" = "blue", "Lucid Motors" = "green")
# line chart
line_chart <- ggplot(data = all_data, aes(x = Date, group = 1)) +
  geom_line(aes(y = avg_price_tesla, color = "Tesla"), size = 1.5) +
  geom_line(aes(y = avg_price_vollkswagen, color = "Volkswagen"), size = 1.5) +
  geom_line(aes(y = avg_price_lucid, color = "Lucid Motors"), size = 1.5) +
  ggtitle("Comparison of Tesla, Lucid Motor, and Volkswagen Stock") +
  ylab("Stock Price") +
  xlab("Date")

#for shinyapp


library('shiny')
library("ggplot2")
library("dplyr")
library('tidyverse')
library('reshape2')
library('shinyWidgets')


automaker_stock <- read.csv("/Users/quinnrosenberg/Downloads/automakers\ stocks\ 2010-2022.csv")

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


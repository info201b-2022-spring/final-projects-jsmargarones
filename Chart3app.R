library(ggplot2)
library(maps)
library(dplyr)
library(viridis)
library(plotly)
library(stringr)
library(shiny)
library(naniar)

#load the dataset
auto_df <- read.csv("/Users/quinnrosenberg/downloads/Largest automakers by market capitalization.csv")

mapp <- maps::map("world",col="lightgrey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80))

#load the map
world_map <- map_data("world")
world_map <- mutate(world_map, region = str_replace_all(world_map$region, "USA", "United States"))

#find the total market cap for each country in billions of dollars
by_country <- auto_df %>% 
  select(price..USD., country) %>% 
  group_by(country) %>% 
  summarize(mean_price = mean(price..USD.)) %>%
  arrange(-mean_price)

#merge map and data
mean_price_map <- merge(world_map, by_country, by.x = "region", by.y = "country", all = TRUE)

#reorder data
mean_price_map <- mean_price_map %>%
  arrange(group, order) %>%
  mutate(mytext = paste(
    "Country: ", region, "\n", 
    "Average Stock Price: $", mean_price, sep="")
  )

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Filters"),
      sliderInput(
        inputId = "price",
        label = "Filter by mean stock price",
        min = 0,
        max = 102,
        value = 0
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "map")
    )
  )
)

#server
server <- function(input, output){
  
  output$map <- renderPlotly({
    filter_map <- mean_price_map %>% mutate(mean_price = if_else(is.na(mean_price), 0, mean_price)) %>%  filter(mean_price >= input$price)
    average_stock__map <- ggplot() +
      geom_polygon(data = filter_map, aes(fill = mean_price, x = long, y = lat, group = group, text = mytext)) +
      scale_fill_viridis(option = "inferno") +
      theme(legend.position = "left") 
    ggplotly(average_stock__map, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)



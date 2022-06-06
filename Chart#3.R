library(ggplot2)
library(maps)
library(dplyr)
library(viridis)
library(plotly)

auto_df <- read.csv("/Users/quinnrosenberg/downloads/Largest automakers by market capitalization.csv")

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

#create bubble map plot
#filter_map <- mean_price_map %>% mutate(mean_price = if_else(is.na(mean_price), 0, mean_price)) %>%  filter(mean_price >= input$price)
#average_stock__map <- ggplot() +
  #geom_polygon(data = filter_map, aes(fill = mean_price, x = long, y = lat, group = group, text = mytext)) +
  #scale_fill_viridis(option = "inferno") +
  #theme(legend.position = "left") 

#stock_map <- ggplotly(average_stock_map, tooltip = "text")




library(ggplot2)
library(maps)
library(dplyr)
library(viridis)
library(plotly)

#load the dataset
auto_df <- read.csv("/Users/quinnrosenberg/downloads/Largest automakers by market capitalization.csv")

#load the map
world_map <- map('world',col="darkgrey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )

#find the total market cap for each country in billions of dollars
by_country <- auto_df %>% 
  select(price..USD., country) %>% 
  group_by(country) %>% 
  summarize(mean_price = mean(price..USD.)) %>%
  arrange(-mean_price)

#download dataset with latitude and longitude of each country
lat_and_long <- read.csv("/Users/quinnrosenberg/Downloads/world_country_and_usa_states_latitude_and_longitude_values.csv")
lat_and_long <- select(lat_and_long, country, latitude, longitude)

#merge the datasets by country
by_country <- merge(x=by_country, y=lat_and_long, by="country")

#reorder data
by_country <- by_country %>%
  arrange(-mean_price) %>%
  mutate(mytext = paste(
  "Country: ", country, "\n", 
  "Average Stock Price: $", mean_price, sep="")
  ) %>%
  mutate(mean_price = round(mean_price, digits = 2))

#create bubble map plot
average_stock_price_by_country <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), alpha = 0.03) +
  geom_point(data = by_country, aes(x = longitude, y = latitude, size = mean_price, color = mean_price, text = mytext, alpha = mean_price)) +
  scale_size_continuous(range = c(1,14)) +
  scale_color_viridis(option = "inferno", trans ="log") +
  scale_alpha_continuous(trans="log") +
  theme_void() +
  theme(legend.position = "left") +
  title("Average Stock Price by Country")

#Get interactive bubble map(must look in 'Viewer' not 'Plots')
stock_plot <- ggplotly(average_stock_price_by_country, tooltip = "text")
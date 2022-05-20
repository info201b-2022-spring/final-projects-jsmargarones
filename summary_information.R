library(dplyr)
library(stringr)

tesla_stock_df <- read.csv("/Users/quinnrosenberg/downloads/TSLA.csv")
  
summary_info <- list()

# A function that takes in a dataset and returns a list of info about it
summary_info$num_observations <- nrow(tesla_stock_df)
summary_info$num_variables <- ncol(tesla_stock_df)

#On what date was the maximum price the highest?
summary_info$max_date <- tesla_stock_df %>%
  filter(High == max(High, na.rm = T)) %>%
  select(Date, High)

#How many total units have been traded?
summary_info$total_units_sold <- tesla_stock_df %>%
  select(Volume) %>%
  summarise(total_units_sold = sum(Volume))

#What is the average (mean) number of units sold in a day?
summary_info$mean_volume <- tesla_stock_df %>%
  select(Volume) %>%
  summarise(mean_volume = mean(Volume))

#What is the largest price jump in a day?
summary_info$max_price_jump <- tesla_stock_df %>%
  mutate(price_change = Close - Open) %>%
  filter(price_change == max(price_change, na.rm = T)) %>%
  select(Date, price_change, Open, Close)

#What was the largest price drop in a day?
summary_info$max_price_drop <- tesla_stock_df %>%
  mutate(price_change = Close - Open) %>%
  filter(price_change == min(price_change, na.rm = T)) %>%
  select(Date, price_change, Open, Close)

#In what year did the price change the most?
summary_info$max_change_year <- tesla_stock_df %>%
  mutate(Year = str_sub(Date, 1, 4)) %>%
  group_by(Year) %>%
  filter(Date == max(Date) | Date == min(Date)) %>% 
  summarise(.groups = "keep", price_change = (max(High) - min(Low)), Year) %>%
  distinct(price_change) %>%
  ungroup() %>%
  filter(price_change == max(price_change))

#In what year did the price change the least?
summary_info$min_change_year <- tesla_stock_df %>%
  mutate(Year = str_sub(Date, 1, 4)) %>%
  group_by(Year) %>%
  filter(Date == max(Date) | Date == min(Date)) %>% 
  summarise(.groups = "keep", price_change = (max(High) - min(Low)), Year) %>%
  distinct(price_change) %>%
  ungroup() %>%
  filter(price_change == min(price_change))

#In what year were the most units sold?
summary_info$max_volume_year <- tesla_stock_df %>%
  mutate(Year = str_sub(Date, 1, 4)) %>%
  group_by(Year) %>%
  summarise(Volume = sum(Volume), Year) %>%
  distinct(Volume) %>%
  ungroup() %>%
  filter(Volume == max(Volume))

#In what year were the least units sold?
summary_info$min_volume_year <- tesla_stock_df %>%
  mutate(Year = str_sub(Date, 1, 4)) %>%
  group_by(Year) %>%
  summarise(Volume = sum(Volume), Year) %>%
  distinct(Volume) %>%
  ungroup() %>%
  filter(Volume == min(Volume))


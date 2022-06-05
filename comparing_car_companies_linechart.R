# This Line Chart compares Tesla’s stocks to different companies that are in direct competition with Tesla. 
# Volkswagen is a leading, mainly gas car company while Lucid Motors is a rising electric car company
# the colors are chosen because Tesla's symbol is red, Volkswagen's is blue, and green contrasts with those 
# bc Lucid Motors main color is black

library("ggplot2")
library("dplyr")
library('tidyverse')
library('shiny')

 tesla <- read.csv("/Users/josiemargarones/Downloads/tesla.csv")
lucid <- read.csv("/Users/josiemargarones/Downloads/lucid motors.csv")
volkswagen <- read.csv("/Users/josiemargarones/Downloads/Volkswagen.csv")

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

# merge with dates, avg_price_tesla, and avg_price_lucid
all_data <- merge(tesla, lucid, by = 'Date')
all_data <- merge(all_data, volkswagen, by = 'Date')

colors <- c("Tesla" = "red", "Volkswagen" = "blue", "Lucid Motors" = "green")
# line chart
line_chart <- ggplot(data = all_data, aes(x = Date, group = 1)) +
  geom_line(aes(y = avg_price_tesla, color = "Tesla"), size = 1.5) +
  geom_line(aes(y = avg_price_vollkswagen, color = "Volkswagen"), size = 1.5) +
  geom_line(aes(y = avg_price_lucid, color = "Lucid Motors"), size = 1.5) +
  ggtitle("Comparison of Tesla, Lucid Motor, and Volkswagen Stock") +
  ylab("Stock Price") +
  xlab("Date")


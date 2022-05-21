# The histogram illustrates how much Tesla stock fluctuates on the daily
# There is no legend because the histogram only deals with one data type

library("ggplot2")
library("dplyr")
library(tidyverse)


tesla <- read.csv("/Users/josiemargarones/Downloads/tesla.csv")

# adds a column to the Tesla table that finds the difference between stock high and low daily
tesla <- tesla %>%
  mutate(difference_daily = High - Low)

# histogram that illustrates the difference in Tesla stock prices on a certain date
histogram_tesla <- ggplot(data = tesla, aes(x = Date, y = difference_daily, color = difference_daily)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  ggtitle("High vs Low Weekly of Tesla Stock") +
  xlab('Date') +
  ylab('Difference between stock high and low') +
  theme(
    plot.title = element_text(face = "bold")
    ) 


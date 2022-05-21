# The histogram illustrates how much Tesla stock fluctuates on the daily
# There is no legend because the histogram only deals with one data type

library("ggplot2")
library("dplyr")
library(tidyverse)


tesla <- read.csv("/Users/josiemargarones/Downloads/tesla.csv")

tesla <- tesla %>%
  mutate(difference_daily = High - Low)


histogram_tesla <- ggplot(data = tesla, aes(x = Date, y = difference_daily, color = difference_daily)) +
  # binwidth is 7 so it's organized by weeks
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  ggtitle("High vs Low Weekly of Tesla Stock") +
  xlab('Date') +
  ylab('Difference between stock high and low') +
  theme(
    plot.title = element_text(face = "bold")
    ) 


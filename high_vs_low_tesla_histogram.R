# The histogram illustrates how much Tesla stock fluctuates on the daily
# There is no legend because the histogram only deals with one data type

library("ggplot2")
library("dplyr")
library(tidyverse)


tesla <- read.csv("/Users/quinnrosenberg/Downloads/tesla.csv")

# adds a column to the Tesla table that finds the difference between stock high and low daily
tesla2 <- tesla %>%
  mutate(difference_daily = High - Low) %>%
  mutate(mytext.2 = paste("Difference between Tesla's high and low price: $", difference_daily, "\n",
         "Date: ", Date, sep = ""))



bar_tesla <- ggplot(data = tesla, aes(x = Date, y = difference_daily, color = difference_daily))
  # binwidth is 7 so it's organized by weeks

# histogram that illustrates the difference in Tesla stock prices on a certain date
histogram_tesla <- ggplot(data = tesla2, aes(x = Date, y = difference_daily, color = difference_daily, text = mytext.2)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  ggtitle("High vs Low of Tesla Stock Daily") +
  xlab('Date') +
  ylab('Difference between stock high and low') +
  theme(plot.title = element_text(face = "bold")) 
interactive_histogram_tesla <- ggplotly(histogram_tesla, tooltip = "text")

#shinyApp(ui = ui, server = server)







library(ggplot2)
library(maps)
library(dplyr)
library(viridis)
library(plotly)
library(stringr)
library(shiny)
library(rsconnect)
source("/Users/quinnrosenberg/final-projects-jsmargarones/high_vs_low_tesla_histogram.R")
source("/Users/quinnrosenberg/final-projects-jsmargarones/Chart#3.R")
source("/Users/quinnrosenberg/final-projects-jsmargarones/comparing_car_companies_linechart.R")

intro_page <- tabPanel("Introduction",
  titlePanel("Tesla Stock and How It Compares With Other Top Car Brands"),
  mainPanel(
    textOutput(outputId = "intro"),
    tags$img(src = "Tesla_logo.png", width = 300, height = 300, deleteFile = FALSE)
    )
  )  


chart_1 <- tabPanel("Tesla vs. Volkswagen Line Chart",
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

chart_2 <- tabPanel("Tesla Daily Price Difference Barplot",
  titlePanel("Comparison of High and Low Stock Prices of Tesla"),
  dateRangeInput("daterange", "Select the date Range(2020/09/18 - 2021/08/23)",
                 start = min(tesla2$Date),
                 end = max(tesla2$Date),
                 min = min(tesla2$Date),
                 max = max(tesla2$Date),
                 separator = " - ", format = "dd/mm/yy"),
    mainPanel(
      plotlyOutput(outputId = "barplot")
    )
  )

chart_3 <- tabPanel("Average Stock Price by Country Map",
  titlePanel("Countries With The Highest Average Automobile Stock Price"),
  sidebarLayout(
    sidebarPanel(
      h1("Filters"),
      sliderInput(
        inputId = "average",
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

takeaways_page <- tabPanel("Summary Takeaways",
  titlePanel("3 Takeaways"),
  verticalLayout(
    h5("Takeaway 1"),
    textOutput(outputId = "summary1"),
    h5("Takeaway 2"),
    textOutput(outputId = "summary2"),
    h5("Takeaway 3"),
    textOutput(outputId = "summary3")
  )
)

ui <- fluidPage(
  navbarPage(
    title = "Tesla's Influence in the Automobile Stock Market",
    intro_page,
    chart_1,
    chart_2,
    chart_3,
    takeaways_page,
  )
)
    

#server
server <- function(input, output){
  
  dateRangeInput<-reactive({
    dataset <- subset(tesla2, Date >= input$daterange[1] & Date <= input$daterange[2])
  })
  
  output$intro <- renderText({
    paste("In this current century, we are seeing a huge rise in the popularity of electric cars over the good old gas-powered engine. Does this mean people should start investing in electric car manufacturers and move away from gas-powered manufacturers? Elon Musk is a name that almost everyone around the world has heard of, owner of SpaceX, creator of Paypal, the new owner of Twitter, but more importantly, is the CEO of the 16.9 billion company Tesla. In this project, our group is doing a deep analysis on the stock market of electric car manufacturers over gas-powered automakers. For this analysis, we will refer to three datasets acquired through the website Kaggle. Our group chose this topic to help young and current investors see if electric car manufacturers are worth future investments or should they stick with gas-making automakers. The first dataset we used shows the data on the top 48 automakers' stock prices from 2010-to 2022. This dataset both includes manufacturers that produce electric cars and gas-powered cars. The next dataset strictly looks at Tesla stock by itself. This dataset provides important information like opening stock price, closing stock price, stock high for the day, stock low for the day, number of units traded in a day, and more. Our last dataset compares the opening and closing stock prices of car manufacturers that produce gas cars and electric cars like Rolls Royce and Audi. For our first interactive graph, we will be comparing the stock prices of the top electric car manufacturer (Tesla) and the top gas-producing manufacturer (Volkswagen). With this interactive graph, we will allow the user to see the stock prices of these two manufacturers at any time during the past ten years. Another graph that will show the fluctuations of Tesla stock prices throughout the past years. Then our last graph will show the market capitalization of the top 48 automakers and group them by country to show users which countries have the highest average stock price for automakers. Our group's primary goal in this presentation is to help investors decide whether or not electric manufacturers are the future of investments or are they overhyped. Knowing important information like stock market trends throughout past years of multiple electric car manufacturers and gas-powered automobiles is important in assessing if electric automobiles are worth investing your money in. Are electric automakers like Lucid Motors and Tesla the future of investments, or should investors still dump their money into the good old gas engine automakers? That is the main question our group is trying to answer in this project.")
  })
    
  
  output$line <- renderPlot({
    ggplot(automaker_stock %>% filter(Symbol == input$cars),
           aes(x = Date, y = avg_stock, group = Symbol)) +
      geom_line(aes(color = Symbol)) +
      ggtitle("Comparison of Tesla, Lucid Motor, and Volkswagen Stock") +
      ylab("Average Stock Price") +
      xlab("Date")
  })
  
  output$barplot <-renderPlotly({
    dataset <- subset(tesla2, Date >= input$daterange[1] & Date <= input$daterange[2])
    date_barplot <- ggplot(data = dataset, aes(x = Date, y = difference_daily, text = mytext.2))  +
      geom_bar(stat="identity") +
      labs(x ="Date") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(date_barplot, tooltip = "text")
  })
    
  output$map <- renderPlotly({
    filter_map <- mean_price_map %>% mutate(mean_price = if_else(is.na(mean_price), 0, mean_price)) %>%  filter(mean_price >= input$average)
    average_stock__map <- ggplot() +
      geom_polygon(data = filter_map, aes(fill = mean_price, x = long, y = lat, group = group, text = mytext)) +
      scale_fill_viridis(option = "inferno") +
      labs(x = "lat", y = "long") +
      theme(legend.position = "left")
    ggplotly(average_stock__map, tooltip = "text")
  })
  
  output$summary1 <- renderText({
    paste("We can gather many key takeaways from this chart; first of all, we can see how much Tesla shares are worth compared to Volkswagen. The stock prices of each of these car manufacturers were recorded every day from 2016-to 2021. According to the line chart, we see that the high price per share for Tesla was back on January 25, 2021, when Tesla shares skyrocketed to 900 dollars per share. Compared to Volkswagen, which is currently the top gas manufacturer in the world, it couldn’t surpass 250 dollars per share back on March 18, 2021. Why is Tesla worth so much more than companies like Volkswagen, a manufacturer that has been around for years? There are many contributors to why Tesla is significantly more successful, things like Elon Musk being a pioneer in the electric car industry, growing information that gas vehicles hurt our world and aren’t eco-friendly compared to electric cars, and Elon Musk, in general, being a well-loved figure in our world for his past achievements. You may also see that after 2021 we see a decline in all the stocks for the manufacturers; this is because of world events like COVID-19, which has a heavy influence on the fluctuation of the stock market. Based on this line chart, we can see that Tesla is at the top of the car industry, but how long will they be at the top? I believe that gas manufacturers like Volkwagen will start to dissipate the production of gas cars and convert to eco-friendly electric cars to compete with companies like Tesla and stay in the car market. All and all, I think investing in companies like Volkswagen isn’t a bad idea right now, considering that a share is significantly cheaper than Tesla. It won’t be long until big manufacturers like Volkswagen start producing electric vehicles and will become heavy competition for Tesla.")
  })
  
  output$summary2 <- renderText({
    paste("In this Tesla Histogram, we can see the difference in Tesla's high and low stock for that day. The timeframe of data collected for this dataset started in 2010 and continues to this day. We can clearly see that between 2019-2020 we see the biggest improvement in Teslas, high and low. One day, we see a difference of 75 dollars per share, which is absurd in the stock market. The key takeaway from this histogram is that Tesla is more than capable of still making the price per share for a Tesla stock increase. As I mentioned above, the reason Tesla stock went on a decline was because of world problems like COVID-19, where the whole world was practically shut down for a significant period of time. Knowing the revolutionary man Elon Musk is and how he tackles company problems investing in Tesla now isn't a bad idea. You just need to be patient to buy at the right price, so a profit is manageable.")
  })
  
  output$summary3 <- renderText({
    paste("In our last interactive graph, we show a Bubble Map of the world showing the average stock price by country. What is interesting to notice about this bubble graph is that even though Tesla has the highest stock price for a car manufacturer, it isn’t the highest stock in the United States; Italy actually has the highest average stock price of 225 dollars per share. Knowing this it tells us that Tesla dominates other automakers in the US market. This is important to know because it tells us that Tesla is not only prominent in the United States but is big in other countries like Italy and Germany, and this can significantly influence Tesla stocks over time. I believe that with the expansion Tesla is making by making their product available in other countries besides the United States will eventually increase the stock for Tesla even past the insane price it is at now.")
  })
}


shinyApp(ui = ui, server = server)






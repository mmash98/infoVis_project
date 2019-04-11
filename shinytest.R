install.packages('shiny')
install.packages('shinydashboard')
install.packages('wesanderson')
install.packages('plotly')

library('shiny')
library('shinydashboard')
library('ggplot2')
library('caret')
library('plotly')

#colors 
library(wesanderson)

df <- read.csv('gg_clean.csv')

####
#inds <- createDataPartition(df$userId , p = 0.4, list=FALSE)
#df <- df[inds,]



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    
      titlePanel("Filters"), 
      sliderInput("Date", label = h3("Date Range"), min = 0, 
                  max = 100, value = c(40, 60)),
      hr(),
      radioButtons("waitTimeSwitch", label = '',
                   choices = list("Driver" = 1, "Passenger" = 2), 
                   selected = 1, inline=TRUE)
      
    
  ),
  
  dashboardBody( # Boxes need to be put in a row (or column)
    
  
    fluidPage(
      titlePanel("GG taxi dashboard"),
      box(plotOutput("plot1", height = 250)),
      box(plotOutput('plot2', height = 250)),
      hr(),
      box(plotOutput('plot3', height = 250)),
      hr(),
      box(plotOutput('plot4', height = 250)),
      hr(),
      box(plotOutput('plot5', height = 250))
    ))
)

server <- function(input, output) {
  #waittimes
  output$plot1 <- renderPlot({
    if(input$waitTimeSwitch == 1)
    {
      ggplot(df[df$driver_wait_time < 20,], aes(x=driver_wait_time, fill = "green")) + geom_histogram(bins = 20)
    }
    else
    {  
      ggplot(df[df$passenger_wait_time < 20,], aes(x=passenger_wait_time, fill = "green")) + geom_histogram(bins = 20)
    }
  })
  ########
  
  #ordertimes
  output$plot2 <- renderPlot({
    ggplot(df, aes(x=order_hour))+ 
      geom_histogram(bins = 24, fill = wes_palette("Zissou1", 24, type = "continuous"))+ 
      coord_polar(theta = "y")
    
  })
  ########
  
  #ridedurations
  df_agg_duration <- aggregate(ride_duration~order_hour, df, FUN = 'mean')
  output$plot3 <- renderPlot({
    ggplot(df_agg_duration, aes(x=order_hour, y=ride_duration, fill='red')) +
      geom_area()+
      geom_point()
  })
  #############
  
  #map TODO
  ############
  
  #prices
  df_agg_price <- aggregate(fare~order_hour, df, FUN = 'mean')
  output$plot4 <- renderPlot({
    ggplot(df_agg_price, aes(x=order_hour, y=fare)) + geom_point() + geom_line()
   
  })
  ######
  
  #cancelations
  df$canceled <- as.numeric(df$canceled)
  df_agg_canceled <- aggregate(canceled~order_hour, df, FUN = 'sum')
  output$plot5 <- renderPlot({
    ggplot(df_agg_canceled, aes(x=order_hour, y=canceled)) +
      geom_point() +
      geom_line() + 
      coord_polar()
    
  })
  ######
  
  ##filters 
  output$range <- renderPrint({ input$slider2 })
  
  
}


shinyApp(ui, server)

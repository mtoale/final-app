library(shiny)
library(fpp3)
library(readr)
library(ggplot2)
library(seasonal)
library(plotly)
library(zoo)
library(shinythemes)

data <- aus_livestock %>%
  filter(State == "Western Australia") 

decomp <- data %>%
  model(classical_decomposition(Count)) %>%
  components()

ui <- fluidPage(
  theme = shinytheme("united"),
  
  h1("Livestock Count for Western Australia"),
  h2("By Megan Toale"),
  h3("Visualizations with choices of Calves, Lambs, Pigs, Sheep, and Cattle (excl. calves)"),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               textOutput("intro")),
      tabPanel("Time Series",
               
               checkboxGroupInput(inputId = "select_ts",
                                  label = "Select an Animal:",
                                  choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                                  selected = "Sheep"),
               
               checkboxInput(inputId = "trendline",
                             label = "Would you like to add a trend line?",
                             value = TRUE),
               
               plotlyOutput("ts_plot"),
               textOutput("ts_explain")),
      
      tabPanel("Seasonality", 
               
               radioButtons(inputId = "select_season",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotlyOutput("season_plot"), 
               textOutput("season_explain")),
      
      tabPanel("AutoCorrelation",
               
               radioButtons(inputId = "select_auto",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotOutput("auto_plot"), 
               textOutput("auto_explain")),
      
      tabPanel("Decompostion", 
               
               plotlyOutput("decomp_plot"), 
               textOutput("decomp_explain")),
      
      tabPanel("Subseries",
               
               radioButtons(inputId = "select_ss",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotlyOutput("ss_plot"), 
               textOutput("ss_explain")),
      
      tabPanel("Naive Forecasting Model",
               
               radioButtons(inputId = "select_naive",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotOutput("naive_plot")),
      
      tabPanel("Mean Forecasting Model",
               
               radioButtons(inputId = "select_mean",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               plotOutput("mean_plot")),
      
      tabPanel("Seasonal Naive Forecasting Model",
               
               radioButtons(inputId = "select_sn",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotOutput("sn_plot")),
      
      tabPanel("Drift Forecasting Model",
               
               radioButtons(inputId = "select_drift",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotOutput("drift_plot")),
      
      tabPanel("Holts Model",
               
               radioButtons(inputId = "select_holt",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotOutput("holt_plot")),
      
      tabPanel("ARIMA Model",
               
               radioButtons(inputId = "select_arima",
                            label = "Select an Animal:",
                            choices = c("Calves", "Lambs", "Pigs", "Sheep", "Cattle (excl. calves)"),
                            selected = "Sheep"),
               
               plotOutput("arima_plot"))
    )
  ),
)

server <- function(input, output){
  
  output$intro <- renderText({
    "Welcome to my final time series app! Please explore while viewing in full screen to be able to view each plot. Each tab, displayed above, can be selected and will display a different plot or model. In each tab you will have the option to select one or sometimes multiple animals from the aus_livestock data. The data has been filtered to only include Western Australia as the state to make the results more readable. The animals that are availible to be selected are Calves, Lambs, Pigs, Sheep, and Cattle (excl. calves). This app was made to study the different animals from Western Australia along with their total count in a time series analysis."
  })
  
  output$ts_plot <- renderPlotly({
    p1 <- data %>%
      filter(Animal == input$select_ts) %>%
      ggplot(aes(x = Month, y = Count, color = factor(Animal))) +
      geom_line()
    
    if(input$trendline == TRUE) {
      p1 <- p1 + geom_smooth(method = lm)
    }
    p1
  })
  
  output$ts_explain <- renderText({
    "Calves has the lowest count when looking at the plot, which makes it almost impossible to view when looking at every animal. They have a a bit of a spike around July 1976, both other than that calves have a downward trend. Cattle (excl. calves) and pigs have similar counts, however the main difference is that pigs is trending while Cattle (excl. calves) has a downward trend. Both don’t have constant seasonality. Similar to Cattle and Pigs, when viewing sheep and lambs they have very similar counts but opposite trends for negative and positive."
  })
  
  output$season_plot <- renderPlotly({
    data %>% 
      filter(Animal == input$select_season) %>%
      gg_season(Count) +
      labs(title = "Animal Counts in Western Australia Seasonality Plot")
  }) 
  
  output$season_explain <- renderText({
    "Lambs, Sheeps, and Pigs all shoe a lot of seasonality that is rather constant. Where as Cattle and Calves show seasonality with a lot of variation and not a defined path or trend over time."
  })
  
  output$auto_plot <- renderPlot({
    data %>%
      filter(Animal == input$select_auto) %>%
      ACF(Count) %>%
      autoplot() +
      labs(title = "Autocorrelation Plot of Animal Counts in Western Australia")
  })
  
  output$auto_explain <- renderText({
    "The autocorrelation plots for sheep and lambs indicate that seasonality exists in the data and is rather consistent. While the plots for calves, pigs, and cattle have some variation it’s not a true indication of seasonality."
  })
  
  output$decomp_plot <- renderPlotly({
    autoplot(decomp) +
      labs(title =  "Animal Counts in Western Australia Decomposition Plot")
  })
  
  output$decomp_explain <- renderText({
    "Lambs is the most noticeable line on this chart as is has a very defined upward trend with a lot of seasonality. While the other animals have less noticeable trends and seasonality over time."
  })
  
  output$ss_plot <- renderPlotly({
    data %>%
      filter(Animal == input$select_ss) %>%
      gg_subseries() +
      labs(title = "Subseries Plot of Animal Counts in Western Australia")
  })
  
  output$ss_explain <- renderText({
    "Sheep"
  })
  
  output$naive_plot <- renderPlot({
    naive_data <- data %>%
      filter(Animal == input$select_naive)
    
    train_naive <- naive_data %>%
      filter_index("2010 Jan" ~ "2018 Dec")
    
    naive_fit <- naive_data %>%
      model(NAIVE(Count)) %>%
      forecast(h = 12)
    
    naive_fit %>%
      autoplot(train_naive, level = NULL) + 
      autolayer(
        filter_index(naive_data, "2018 Aug" ~ .),
        colour = "orange") +
      labs(title = "Naive Forecasting Model")
  })
  
  output$mean_plot <- renderPlot({
    
    mean_data <- data %>%
      filter(Animal == input$select_mean)
    
    train_mean <- mean_data %>%
      filter_index("2010 Jan" ~ "2018 Dec")
    
    mean_fit <- mean_data %>%
      model(MEAN(Count)) %>%
      forecast(h = 12)
    
    mean_fit %>%
      autoplot(train_mean, level = NULL) + 
      autolayer(
        filter_index(mean_data, "2018 Aug" ~ .),
        colour = "orange") +
      labs(title = "Mean Forecasting Model")
  })
  
  output$sn_plot <- renderPlot({
    
    sn_data <- data %>%
      filter(Animal == input$select_sn)
    
    train_sn <- sn_data %>%
      filter_index("2010 Jan" ~ "2018 Dec")
    
    sn_fit <- sn_data %>%
      model(SNAIVE(Animal ~lag("year"))) %>%
      forecast(h = 12)
    
    sn_fit %>%
      autoplot(train_sn, level = NULL) + 
      autolayer(
        filter_index(sn_data, "2018 Aug" ~ .),
        colour = "orange") +
      labs(title = "Seasonal Naive Forecasting Model")
  })
  
  output$drift_plot <- renderPlot({
    
    drift_data <- data %>%
      filter(Animal == input$select_drift)
    
    train_drift <- drift_data %>%
      filter_index("2010 Jan" ~ "2018 Dec")
    
    drift_fit <- drift_data %>%
      model(RW(Count ~ drift())) %>%
      forecast(h = 12)
    
    drift_fit %>%
      autoplot(train_drift, level = NULL) + 
      autolayer(
        filter_index(drift_data, "2018 Aug" ~ .),
        colour = "orange") +
      labs(title = "Drift Forecasting Model")
  })
  
  output$holt_plot <- renderPlot({
    holt <- data %>%
      model(ETS(input$select_holt))
  })
  
  output$arima_plot <- renderPlot({
    arima <- data %>% 
      model(ARIMA(input$select_arima, stepwise = FALSE))
  })
}

shinyApp(ui = ui, server = server)

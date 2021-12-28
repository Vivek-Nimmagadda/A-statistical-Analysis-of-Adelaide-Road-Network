library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(scales)
library(zoo)
library(DT)

server <- function(input, output, session) {
  
  sales <- read.delim("Daily_Data.txt", header = TRUE, sep = "\t")
  
  test <- read.delim("Test_Data.txt", header = TRUE, sep = "\t")
  
  #Summarize Data and then Plot
  
  #df <- reactive({
  # req(input$sel_Probes)
  #df <- sales %>% filter(Probe_ID %in% input$sel_SalesRep) # %>% group_by(Month) %>% summarise(Sales = sum(Probe_Count))
  #})
  
  # Back-end Function for Plotting the actual data
  main_model <- reactive({
    req(input$sel_Probes)
    df <- sales %>% filter(Probe_ID %in% input$sel_Probes) # %>% group_by(Month) %>% summarise(Sales = sum(Probe_Count))
    
    
    
    # Finding missing "date" fields in the data and imputing NA's
    library(padr)
    df$Date <- as.Date(df$Date)
    df <- pad(df)
    
    # Finding the day of the week from the date
    df$Week_No <- df$Date
    df$Week_No <- strftime(df$Week_No, format = "%V")
    df$day <- weekdays(df$Date)
    
    # Replacing missing Site_ID values with the Actual Site_ID
    df$Probe_ID[is.na(df$Probe_ID)] <- mean(df$Probe_ID, na.rm = TRUE)
    
    # Finding the Day-wise Mean of the data
    Mean_Values_By_Weekday <- aggregate(Probe_Count ~ day, df, mean, na.rm = TRUE)
    Mean_Values_By_Weekday$day <- factor(Mean_Values_By_Weekday$day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    # Re-ordering the data from Monday to Sunday
    Mean_Values_By_Weekday <- Mean_Values_By_Weekday[order(Mean_Values_By_Weekday$day), ]
    
    # Replace NA values in "Probe_Count" field with 0's
    df$Probe_Count[is.na(df$Probe_Count)] <- 0
    
    # Loop to find and store all values less than the 1st Quantile
    n = 0
    m = 0
    x = data.frame()
    for(i in 1:length(df$Probe_Count)){
      if(df$Probe_Count[i] < quantile(df$Probe_Count, 0.25)){
        m = m + 1
        n = df$Probe_Count[i]
        x = rbind(x, n)
      }
    }
    
    colnames(x) <- "Probe_Count"
    
    # Loop to replace all missing values and impure values with the mean of the "weekdays"
    # This is the safest possible way to replace the wrong values
    for(i in 1:length(df$Probe_Count)){
      if((df$Probe_Count[i] < quantile(x$Probe_Count, 0.25)) && (df$day[i] == "Monday")){
        df$Probe_Count[i] <- Mean_Values_By_Weekday$Probe_Count[1]
      }
      else if((df$Probe_Count[i] < quantile(x$Probe_Count, 0.25)) && (df$day[i] == "Tuesday")){
        df$Probe_Count[i] <- Mean_Values_By_Weekday$Probe_Count[2]
      }
      else if((df$Probe_Count[i] < quantile(x$Probe_Count, 0.25)) && (df$day[i] == "Wednesday")){
        df$Probe_Count[i] <- Mean_Values_By_Weekday$Probe_Count[3]
      }
      else if((df$Probe_Count[i] < quantile(x$Probe_Count, 0.25)) && (df$day[i] == "Thursday")){
        df$Probe_Count[i] <- Mean_Values_By_Weekday$Probe_Count[4]
      }
      else if((df$Probe_Count[i] < quantile(x$Probe_Count, 0.25)) && (df$day[i] == "Friday")){
        df$Probe_Count[i] <- Mean_Values_By_Weekday$Probe_Count[5]
      }
      else if((df$Probe_Count[i] < quantile(x$Probe_Count, 0.25)) && (df$day[i] == "Saturday")){
        df$Probe_Count[i] <- Mean_Values_By_Weekday$Probe_Count[6]
      }
      else if((df$Probe_Count[i] < quantile(x$Probe_Count, 0.25)) && (df$day[i] == "Sunday")){
        df$Probe_Count[i] <- Mean_Values_By_Weekday$Probe_Count[7]
      }
    }
    
    # Plotting the data
    Probe_Count_by_date <- df[,c(3,2)]
    Probe_Count_by_date$Date <- as.Date(Probe_Count_by_date$Date)
    
    # Converting the data to a time series and fitting the ARIMA model
    Probe_Count_df <- ts(df$Probe_Count, start = c(2021, 4), frequency = 365)
    arima.model <<- auto.arima(Probe_Count_df)
    
    auto_correlation <<- acf(auto.arima(Probe_Count_df)$resid, lag.max = 50)
    partial_auto_correlation <<- pacf(auto.arima(Probe_Count_df)$resid, lag.max = 50)
    
    # forecasting the predictions for the next 2 weeks
    forecast.arima <<- forecast(auto.arima(Probe_Count_df), h = 14)
    
    Probe_Count_by_date
  })
  
  # Function for displaying the ARIMA Model
  ar <- reactive({
    req(input$sel_Probes)
    arima.model
  })
  
  # Function for displaying the ACF plot
  correlogram <- reactive({
    req(input$sel_Probes)
    auto_correlation
  })
  
  # Function for displaying the PACF plot
  partial_correlogram <- reactive({
    req(input$sel_Probes)
    partial_auto_correlation
  })
  
  # Function for displaying the Forecasted Values along with the Actual Values
  prediction <- reactive({
    req(input$sel_Probes)
    forecast.arima
  })
  
  # Function for displaying the 80% and 95% Confidence-Intervals of Predictions as a Data Table
  prediction_confidence <- reactive({
    req(input$sel_Probes)
    predictions <- data.frame(forecast.arima)
    predictions
    forecast_arima_df <- forecast.arima$mean
    forecast_arima_df <- data.frame(forecast_arima_df)
    colnames(forecast_arima_df) <- "Forecasted_Probe_Counts"
    forecast_arima_df$Date <- c("2021-10-04", "2021-10-05", "2021-10-06", "2021-10-07", "2021-10-08", "2021-10-09", "2021-10-10", 
                                "2021-10-11", "2021-10-12", "2021-10-13", "2021-10-14", "2021-10-15", "2021-10-16", "2021-10-17")
    forecast_arima_df$Forecasted_Probe_Counts <- round(forecast_arima_df$Forecasted_Probe_Counts)
    forecast_arima_df <<- forecast_arima_df
    forecast_arima_df
    rownames(predictions) <- forecast_arima_df$Date
    predictions <- round(predictions)
    predictions
  })
  
  # Function for displaying the actual and Forecasted Values
  prediction_testing <- reactive({
    req(input$sel_Probes)
    
    df1 <- test %>% filter(Probe_ID %in% input$sel_Probes) # %>% group_by(Month) %>% summarise(Sales = sum(Probe_Count))

    forecast_arima_df$Actual_Probe_Counts <- df1$Probe_Count
    rownames(forecast_arima_df) <- forecast_arima_df$Date
    forecast_arima_df <- forecast_arima_df[,c(3, 1)]
    forecast_arima_df
  })
  
  # The Code below runs multiple times depending on user's interaction with the dashboard
  #Update SelectInput Dynamically
  observe({
    updateSelectInput(session, "sel_Probes", choices = sales$Probe_ID)
  })
  
  #Plot 
  
  #output$plot <- renderPlot({
  # g <- ggplot(df(), aes(y = Probe_Count, x = Year))
  #g + geom_bar(stat = "sum")
  #})
  
  output$raw_data <- renderPlot({
    g1 <- ggplot(main_model(), aes(y = Probe_Count, x = Date))
    g1 + geom_line() + ggtitle("Devices Tracked in 2021 Between 4th January - 3rd October") + theme(
      plot.title = element_text(color="black", size=18, face="bold.italic"),
      axis.title.x = element_text(color="blue", size=14, face="bold"),
      axis.title.y = element_text(color="blue", size=14, face="bold") 
    )
  })
  
  output$probe <- renderText({
    paste("You have selected the Probe: ", input$sel_Probes)
  })
  
  output$ar <- renderText({
    paste("The Fitted ARIMA Model is:", ar())
  })
  
  output$auto_correlation <- renderPlot({
    plot(correlogram(), cex.lab = 1.3, col.lab = "blue", main = "Auto Correlogram of the Fitted ARIMA Model")
  })
  
  output$partial_auto_correlation <- renderPlot({
    plot(partial_correlogram(), cex.lab = 1.3, col.lab = "blue", main = "Partial Auto Correlogram of the Fitted ARIMA Model")
  })
  
  output$forecast <- renderPlot({
    options(scipen = 999)
    plot(prediction(), xlab = 'Date', ylab = 'Devices Tracked', cex.lab = 1.3, col.lab = "blue")
  })
  
  output$confidence <- DT::renderDataTable({
    prediction_confidence()
  })
  output$testing <- DT::renderDataTable({
    prediction_testing()
  })
}

# This function plots the actual outputs in the form of Tables and Images
ui <- basicPage(
  tags$head(tags$style("#ar{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 text-align: center;
                                 }",
                       "#probe{color: black;
                                 font-size: 15px;
                                 }"
  )
  ),
  h3("Analyzing Traffic Patterns Associated with Different Probes"),
  selectInput(inputId = "sel_Probes",
              label = "Choose a Probe ID",
              "Names"),
  #plotOutput("plot"),
  textOutput("probe"),
  plotOutput("raw_data"),
  #h4(paste("The Final ARIMA Model for this data is: ", y, x)),
  mainPanel(
    verbatimTextOutput("ar")
  ),
  plotOutput("auto_correlation"),
  plotOutput("partial_auto_correlation"),
  plotOutput("forecast"),
  DT::dataTableOutput("confidence"),
  DT::dataTableOutput("testing")
)

shinyApp(ui = ui, server = server)
# Importing the Dataset
sales <- read.delim("Daily_Data.txt", header = TRUE, sep = "\t")

# Testing the data with only one Site ID
df <- sales %>% filter(Probe_ID == 3088)
#df <- sales

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
library(ggplot2)
df1 <- df[,c(3,2)]
df1$Date <- as.Date(df1$Date)
ggplot(df1) + aes(Date, Probe_Count) + geom_line()

# Converting the data to a time series
x <- ts(df$Probe_Count, start = c(2021, 4), frequency = 365)
auto.arima(x)

# forecasting the predictions for the next 2 weeks
y <- forecast(auto.arima(x), h = 14)
plot(y)

# Comparing the predictions with the actual values


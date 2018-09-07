library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(zoo)
library(forecast)
library(fma)

rm(list=ls())
# import well data
setwd("C:/Users/Jerry/Documents/MSA18/Time_Series/HW/HW_Team")
well <- read_excel("../PB-1680_T.xlsx", sheet = 3)

# change time data into character and make the hourly time frame
well$date <- as.character(well$date)
well$time <- as.character(well$time)
well$hour <- paste(well$date, str_sub(well$time, 12, 13))

# change the time data into UTC time zone
well$hour <- paste0(well$hour, ":00:00", " ", well$tz_cd)
well$hour <- as.POSIXct(well$hour)
attributes(well$hour)$tzone <- "UTC"

# aggregate the data on hour basis
well_agg <- well %>%
  group_by(hour) %>%
  summarise(Well_ft_mean = mean(Well_ft, na.rm = TRUE),
            Corrected_mean = mean(Corrected, na.rm = TRUE)) 

# arrange the time data to create monthly basis
month_agg <- well_agg
month_agg$hour <- as.POSIXct(well_agg$hour)
month_agg <- cbind(month = paste(as.character(year(well_agg$hour)), as.character(month(well_agg$hour))),well_agg)
month_agg$hour <- NULL
month_agg[,1] <- paste(month_agg[,1], "01")
month_agg[,1] <- as.Date(month_agg[,1], "%Y %m %d")

# aggregate the data on monthly basis
month_agg <- month_agg %>%
  group_by(month_agg[,1]) %>%
  summarise(Well_ft_mean = mean(Well_ft_mean, na.rm = TRUE), Corrected_mean = mean(Corrected_mean, na.rm = TRUE))
colnames(month_agg)[1] <- "month"

# order the data by month
month_agg <- month_agg[order(month_agg$month),]

# create the time frame by hour
month_seq <- seq(
  from = as.Date("2007-10-01", tz = "UTC"),
  to = as.Date("2018-06-13", tz = "UTC"),
  by = "month"
)

df_month_seq <- data.frame(month = month_seq)

# merge well data into time frame
month_agg <- month_agg %>%
  right_join(df_month_seq, by = "month")

# interpolate missing values
month_agg_intp <- month_agg
month_agg_intp$Well_ft_mean <- na.approx(month_agg$Well_ft_mean)
month_agg_intp$Corrected_mean <- na.approx(month_agg$Corrected_mean)

# use well_ft_mean as time series data because all the values are positive
well_ts <- ts(month_agg$Well_ft_mean, start = c(2007, 10), end = c(2018, 6), frequency = 12)
well_ts <- na.approx(well_ts)

# Use a holdout data set
training <- subset(well_ts, end = length(well_ts) - 6)
test <- subset(well_ts, start = length(well_ts) - 5)

# generate prediction by using different model
HW_M_Train <- hw(training, seasonal = "multiplicative", initial='optimal', h = 6)
HW_A_Train <- hw(training, seasonal = "additive", initial='optimal', h = 6)
SES_Train <- ses(training, initial = "optimal", h=6)
HOLT_Train <- holt(training, initial='optimal', h = 6)
DAMPED_Train <- holt(training, initial='optimal', damped = TRUE, h = 6)

# store the different model results in a list
ls_Model <- list(HW_M = HW_M_Train, HW_A = HW_A_Train, SES = SES_Train, HOLT = HOLT_Train, DAMPED = DAMPED_Train)

# calculate MAPE
for(i in 1:length(ls_Model)){
  error = test - ls_Model[[i]]$mean
  ls_Model[[i]]$MAPE = mean(abs(error)/abs(test))
}

# look for the least MAPE
for(i in 1:length(ls_Model)){
  print(ls_Model[[i]]$MAPE)
}


# check the plots (if aggregating by month changes the seasonality)
ggplot(month_agg_intp, aes(month, Well_ft_mean)) + geom_line() +
  xlab("time") + ylab("well_ft_mean")

# decomposition
model <- stl(well_ts, s.window = 7)
plot(model)

HW_stl <- stl(ls_Model$HW_M$fitted, s.window = 7)
plot(HW_stl)

df_HW_stl <- data.frame(HW_stl$time.series)
df_HW_stl <- cbind(df_HW_stl, month = month_agg_intp$month[1:(nrow(month_agg) - 6)])
# change the model time series into data frame
df_ts <- data.frame(model$time.series)
df_ts <- cbind(df_ts, month = month_agg$month)
df_ts <- df_ts[1:(nrow(df_ts) - 6),]
df_ts <- cbind(df_ts, HW_seasonal = df_HW_stl$seasonal, HW_trend = df_HW_stl$trend, HW_remainder = df_HW_stl$remainder)

# create the plot for the trend
ggplot(df_ts, aes(month)) + 
  geom_line(aes(y = training, colour = "Actual")) + 
  geom_line(aes(y = HW_trend, colour = "Prediction")) +
  xlab("Date") +
  ylab("Well ft") +
  labs(title = "Actual vs. Model Trend Decomposition", size = 15) +
  theme(legend.title = element_blank())

# create the plot for the seasonality
ggplot(df_ts, aes(month)) + 
  geom_line(aes(y = seasonal, colour = "Actual")) + 
  geom_line(aes(y = HW_seasonal, colour = "Prediction")) +
  scale_x_date(limits = c(as.Date('2014-01-01'), as.Date('2014-05-01')))
  xlab("Date") +
  ylab("Well ft") +
  labs(title = "Actual vs. Model Seasonality Decomposition", size = 15) +
  theme(legend.title = element_blank())

df_prediction <- data.frame(cbind(month = month_agg_intp$month[(nrow(month_agg)-5):nrow(month_agg)], test = test, HW_M = ls_Model$HW_M$mean))
df_prediction$month <- as.Date(df_prediction$month)

# create the plot for the prediction
ggplot(df_prediction, aes(month)) + 
  geom_line(aes(y = test, colour = "Actual")) +
  geom_line(aes(y = HW_M, colour = "Prediction")) +
  xlab("Date") +
  ylab("Well ft") +
  labs(title = "Well ft Actual vs. Prediction", size = 15) +
  theme(legend.title = element_blank())

# ESM forecast 6 month out
autoplot(ls_Model$HW_M) +
  autolayer(fitted(ls_Model$HW_M), series = "Fitted") +
  ylab("Well ft") +
  xlab("Date") +
  labs(title = "Well ft ESM Forecast for 6 months", size = 15) +
  theme(legend.title = element_blank())




i = 2007
as.Date(paste0(as.character(i), '-01-01'))
# seasonality test
for (i in 2008:2016) {
  print(as.Date(paste0(as.character(i), '-01-01')))
ggplot(df_ts, aes(month)) + 
  geom_line(aes(y = seasonal, colour = "Actual")) + 
  geom_line(aes(y = HW_seasonal, colour = "Prediction")) +
  scale_x_date(limits = c(as.Date(paste0(as.character(i), '-01-01')), as.Date(paste0(as.character(i), '-06-01'))))+
  labs(title = i, size = 15) +
  theme(legend.title = element_blank())
}

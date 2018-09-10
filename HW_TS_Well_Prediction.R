library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(zoo)
library(forecast)
library(fma)
library(imputeTS)
library(tseries)

# Clear workspace for consistency reasons
rm(list=ls())


# import well data, make sure to set your working directory to the source file location
well <- read_excel("PB-1680_T.xlsx", sheet = 3)

# change time data into character and make the hourly time frame
well$date <- as.character(well$date)
well$time <- as.character(well$time)
well$hour <- paste(well$date, str_sub(well$time, 12, 13))

# change the time data into UTC time zone
well$hour <- paste0(well$hour, ":00:00", " ", well$tz_cd)
well$hour <- as.POSIXct(well$hour)
attributes(well$hour)$tzone <- "UTC"

# aggregate the data on an hourly basis
well_agg <- well %>%
  group_by(hour) %>%
  summarise(Well_ft_mean = mean(Well_ft, na.rm = TRUE),
            Corrected_mean = mean(Corrected, na.rm = TRUE)) 

# Prepare the data to be aggregated by Year/Month 
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

# In order to identify missing values, create a sequence of months from 2007-10 to 2018-06
month_seq <- seq(
  from = as.Date("2007-10-01", tz = "UTC"),
  to = as.Date("2018-06-13", tz = "UTC"),
  by = "month"
)

# Merge the sequence into our data. Months not in the data will have a missing value
df_month_seq <- data.frame(month = month_seq)
month_agg <- month_agg %>%
  right_join(df_month_seq, by = "month")

# interpolate missing values
month_agg_intp <- month_agg
month_agg_intp$Well_ft_mean <- na.interpolation(month_agg$Well_ft_mean)
month_agg_intp$Corrected_mean <- na.interpolation(month_agg$Corrected_mean)

# subset data starting from June, 2011  
month_agg_intp <- month_agg_intp[45:nrow(month_agg_intp),]

# use well_ft_mean as time series data because all the values are positive
ts_wellft <- ts(month_agg_intp$Well_ft_mean, start = c(2011, 6), end = c(2018, 6), frequency = 12)

# Use a holdout data set of 6 months
training <- subset(ts_wellft, end = length(ts_wellft) - 6)
test <- subset(ts_wellft, start = length(ts_wellft) - 5)

# generate prediction by using different models
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
  print(
    paste(
      ls_Model[[i]]$method,": ",ls_Model[[i]]$MAPE
      ))
}

# check the plots (if aggregating by month changes the seasonality)
ggplot(month_agg_intp, aes(month, Well_ft_mean)) + geom_line() +
  xlab("time") + ylab("well_ft_mean")

# decomposition
wellft_stl <- stl(ts_wellft, s.window = 7)
HW_stl <- stl(ls_Model$HW_M$fitted, s.window = 7)
training_stl <- stl(training, s.window = 7)

# convert HW_stl time series into data frame 
df_HW_stl <- data.frame(HW_stl$time.series)
# add month column
df_HW_stl <- cbind(df_HW_stl, month = month_agg_intp$month[1:(nrow(month_agg_intp) - 6)])

# change the training_stl time series into data frame
df_training_stl <- data.frame(training_stl$time.series)

# combine df_HW_stl and df_training_stl
df_training_stl <- cbind(month = month_agg_intp$month[1:(nrow(month_agg_intp) - 6)], df_training_stl, 
                         HW_seasonal = df_HW_stl$seasonal, HW_trend = df_HW_stl$trend, HW_remainder = df_HW_stl$remainder)

# add a column that calculates seasonal adjusted well ft
df_training_stl$seasonal_adjusted <- training - df_training_stl$seasonal

# calculate the difference between well_ft and corrected
diff_wc <- unique(well$Well_ft - well$Corrected)[1]



######### The plots we need ###########
# set the min and max for the plots
min = min(training, na.rm = TRUE) - diff_wc - 0.5
max = max(training, na.rm = TRUE) - diff_wc + 0.5

# create the plot for the trend
ggplot(df_training_stl, aes(month)) + 
  geom_line(aes(y = (training - diff_wc), colour = "Actual")) + 
  geom_line(aes(y = (trend - diff_wc), colour = "Trend")) +
  ylab("Corrected Water Level (Ft)") +
  xlab("Date") +
  ylim(c(min, max)) +
  labs(title = "PB-1680 Water Level with Trend", size = 15,
       subtitle="STL Decomposition of Training Data - Oct 2011 to Dec 2017") +
  theme(legend.title = element_blank())

#Actual vs training seasonality adjusted
ggplot(df_training_stl, aes(month)) +
  geom_line(aes(y = training - diff_wc, colour = "Actual")) +
  geom_line(aes(y = seasonal_adjusted - diff_wc, colour = "Seasonality Adjusted")) +
  xlab("Date") +
  ylab("Corrected Water Level (Ft)") +
  ylim(c(min, max)) +
  labs(title = "PB-1680 Monthly Water Level with Seasonality Adjustment", size = 15,
       subtitle = "STL Decomposition of Training Data - Oct 2011 to Dec 2017") +
  theme(legend.title = element_blank())

# convert the prediction data into dataframe so can plot
df_prediction <- data.frame(cbind(month = month_agg_intp$month[(nrow(month_agg_intp)-5):nrow(month_agg_intp)],
                                  test = test,
                                  HW_M = ls_Model$HW_M$mean))
df_prediction$month <- as.Date(df_prediction$month)


######### how to display Jan 2018, Feb 2018, ....
library(scales)

# create the plot for the prediction
ggplot(df_prediction, aes(as.Date(month))) + 
  geom_line(aes(y = test - diff_wc, colour = "Actual")) +
  geom_line(aes(y = HW_M - diff_wc, colour = "Prediction")) +
 # scale_x_date(labels =  function(x) format(x, "%b")) +
  xlab("Month") +
  ylab("Corrected Water Level (Ft)") +
  labs(title = "PB-1680 Model Evaluation on Test Data",
       subtitle="Actual vs. Predicted Water Level: January-June 2018", size = 15) +
  ylim(c(min, max)) + 
  theme(legend.title = element_blank())

########## can we use ggplot???????  and we need to subtract diff_wc
## Don't believe we need this plot

  # ESM forecast 6 month out
  autoplot(ls_Model$HW_M) +
    autolayer(fitted(ls_Model$HW_M), series = "Fitted") +
    ylab("Well ft") +
    xlab("Date") +
    ylim(c(min, max+2)) +
    labs(title = "Well ft ESM Forecast for 6 months", size = 15) +
    theme(legend.title = element_blank())





# seasonality check over time
#for (i in 2008:2016) {
#  print(ggplot(df_ts, aes(month)) + 
#    geom_line(aes(y = seasonal, colour = "Actual")) + 
#    geom_line(aes(y = HW_seasonal, colour = "Prediction")) +
#    scale_x_date(limits = c(as.Date(paste0(as.character(i), '-07-01')), as.Date(paste0(as.character(i), '-12-01'))))+
#    labs(title = i, size = 15) +
#    theme(legend.title = element_blank()))
#}
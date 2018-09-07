library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(zoo)

# import well data
well <- read_excel("/Users/jesseball/Documents/Time Series/PB-1680_T.xlsx", sheet = 3)

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
  summarise(Well_ft_mean = mean(Well_ft), Corrected_mean = mean(Corrected)) 

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
  summarise(Well_ft_mean = mean(Well_ft_mean), Corrected_mean = mean(Corrected_mean))
colnames(month_agg)[1] <- "month"

# order the data by month
month_agg <- month_agg[order(month_agg$month),]

# use well_ft_mean as time series data because all the values are positive
well_ts <- ts(month_agg[,-c(1,3)], start = 2007, frequency = 12)


# Use a holdout data set
training <- subset(well_ts,end =length(well_ts)-6)
test <- subset(well_ts,start=length(well_ts)-5)

# generate prediction by using different model
HW_M_Train <- hw(training, seasonal = "multiplicative",initial='optimal', h = 6)
HW_A_Train <- hw(training, seasonal = "additive",initial='optimal', h = 6)
SES_Train <- ses(training, initial = "optimal", h=6)
HOLT_Train <- holt(training,initial='optimal', h = 6)

# store the different model results in a list
ls_Model <- list(HW_M = HW_M_Train, HW_A = HW_A_Train, SES = SES_Train, HOLT = HOLT_Train)

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
ggplot(month_agg, aes(month, Well_ft_mean)) + geom_line() +
  xlab("time") + ylab("well_ft_mean")

well_ft <- ts(well_agg$Well_ft_mean, start = 2007, frequency = 8760)
model <- stl(well_ft, s.window = 7, na.action = na.approx)
plot(model)

well_ft <- ts(month_agg$Well_ft_mean, start = 2007, frequency = 12)
model <- stl(well_ft, s.window = 7, na.action = na.approx)
plot(model)









plot(ls_Model$HW_M, main = "Well Ft with HW ADDiditve", xlab = "Date", ylab = "Well Ft in KNots")
abline(v = 2007, col = "red", lty = "dashed")
round(accuracy(HWES.well.train.add),2)
library(ggplot2)
autoplot(ls_Model$HW_M)+
  autolayer(fitted(ls_Model$HW_M),series="Fitted")+ylab("Well Ft with HW Addidinve")



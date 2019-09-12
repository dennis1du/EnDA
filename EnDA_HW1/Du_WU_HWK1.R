Du_WU_HWK1 <- function(month, day, year, station_id) {
  
  #packages loaded
  library(lubridate)
  
  #data collecting
  WU_URL <- paste("https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=",station_id, "&month=", month, "&day=", day, "&year=", year, "&format=1", sep = "")#create url from inputs
  WU_data <- read.csv(WU_URL, row.names = NULL, stringsAsFactors = FALSE)#import csv file to data
  
  #data cleaning
  WU_data <- WU_data[WU_data$row.names != '<br>',]
  names(WU_data) <- names(WU_data)[2:length(names(WU_data))]
  
  #data processing
  WU_data <- subset(WU_data, select = c(Time, TemperatureF, DewpointF, WindSpeedMPH, Humidity))
  names(WU_data) <- c('time', 'tempF', 'DewpointF', 'wnd.spd.mph', 'RH')
  WU_data$tempC <- (as.numeric(WU_data$tempF) - 32)*(5/9)
  WU_data$DP_tempC <- (as.numeric(WU_data$DewpointF) - 32)*(5/9)
  WU_data$wnd.spd.ms <- as.integer(WU_data$wnd.spd.mph)*0.44704
  WU_data$RH <- as.numeric(WU_data$RH)*1
  WU_data <- WU_data[c(1,5,6,7,8)]
  WU_data$time <- as.POSIXct(WU_data$time)
  WU_data$time2 <- ceiling_date(WU_data$time, "hour")
  WU_data <- aggregate(WU_data[c(2:5)], by = list(WU_data$time2), FUN = mean)
  names(WU_data) <- c('time', 'RH', 'tempC', 'DP_tempC', 'wnd.spd.ms')
  WU_data$wetbulb_tempC <- (-5.806+0.672*WU_data$tempC-0.006*WU_data$tempC*WU_data$tempC+(0.061+0.004*WU_data$tempC+0.000099*WU_data$tempC*WU_data$tempC)*WU_data$RH+(-0.000033-0.000005*WU_data$tempC-0.0000001*WU_data$tempC*WU_data$tempC)*WU_data$RH*WU_data$RH)
  return(WU_data)
  
}

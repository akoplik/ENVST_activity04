library(ggplot2)
library(dplyr)
library(lubridate)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")

metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")

sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")

weather$dateF <- mdy_hm(weather$Date)
weather$doy <- yday(weather$dateF)
weather$year <- year(weather$dateF)

# add a column to weather:
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation

weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

weather <- weather %>% filter(!is.na(AirTemp))

# x is a vector of dates
intervalFlag <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  rows_to_flag <- as.integer(interval_times != 900)
  return(rows_to_flag)
}
flagged_rows <- c(1,intervalFlag(weather$dateF))
weather$flagged_rows <- flagged_rows

weather_cleaned <- weather %>% filter(flagged_rows == 0)
weather_cleaned$rolling_average <- 0

january_22_weather <- weather_cleaned %>% filter(year == 2022 & doy <= 31)

for(i in 1:length(january_22_weather$Date))
{
  if(i <= 8){
    january_22_weather$rolling_average[i] = mean(january_22_weather$AirTemp[1:i])
  }
  else{
    january_22_weather$rolling_average[i] = mean(january_22_weather$AirTemp[(i-7):i])
  }
}
# Prompt 1

ggplot(january_22_weather, aes(x = dateF)) +
  geom_point(aes(y = AirTemp), alpha = 0.5, size = 1, color = "black") +
  geom_line(aes(y = rolling_average), color = "blue") + 
  labs(title = "Air Temperature Data at Hamilton College (Jan. 2022)",
       x = "Date",
       y = "Air Temperature (C°)") +
  theme_classic() +
  annotate("text", x = january_22_weather$dateF[2975], y = 10, label = "15 Minute Air Temp", color = "black", size = 3, hjust = 1)+
annotate("text", x = january_22_weather$dateF[2975], y = 8, label = "2 Hour Rolling Average Air Temp", color = "blue", hjust =1, size = 3)

# Prompt 2

may_june_21 <- weather_cleaned %>% filter(weather_cleaned$doy >= 121 & weather_cleaned$doy <= 181 & weather_cleaned$year == 2021)
ggplot(may_june_21, aes(x = dateF, y = SolRad)) + geom_line()

# Certainly some inconsistencies, but generally the data makes sense (every night there's little radiation, some days are simply cloud)

# Prompt 3

# 1. UTC is the wrong time zone
# During daylight savings, the date doesn't adjust and looks at data as if it is going back in time, confusing the data



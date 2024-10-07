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

#Question 1
  #As the weather station data manager, you been asked to share precipitation 
  #data with the village of Clinton. You want to ensure that there are no 
  #issues with the bird excrement or frozen precipitation. You want to exclude 
  #any precipitation that occurs when the air temperature is below zero. You 
  #also want to check that no precipitation measurements are used if the X and 
  #Y level observations are more than 2 degrees.

  #Indicate how many missing precipitation values are in your data. 
  #Do you think there might be any additional issues with the precipitation 
  #data to consider before sending the data to the village. Generally describe 
  #(do not code) what you might need to do for further data cleaning.

weather$Precip.Cleaned <- ifelse((abs(weather$XLevel) > 2 | abs(weather$YLevel) > 2 | weather$AirTemp < 0),
                                             NA, weather$Precip)

sum(as.integer(is.na(weather$Precip.Cleaned)))

# 14,096 missing preceipitation values

#Question 2
  #Create a data flag that warns a user if the battery voltage falls below 
  #8.5 Volts. Explain how you set up the flag.

weather$BatFlag <- ifelse(weather$BatVolt < 8500, 1, 0)

#Question 3
  #You should also create a function that checks for observations that are in 
  #unrealistic data ranges in air temperature and solar radiation. Explain 
  #how your function works.

check_outliers <- function(x){
  mean_x <- mean(x, na.rm = T)
  sd_x <- sd(x, na.rm = T)
  z_score <- (x - mean_x) / sd_x
  outliers <- ifelse(abs(z_score) > 3, 1, 0)
}

weather$AirTempOutliers = check_outliers(weather$AirTemp)
weather$SolarOutliers = check_outliers(weather$SolRad)

#Question 4
  #Make a plot of winter air temperatures in Jan - Mar of 2021. Check for 
  #persistence issues that might indicate snow accumulation on the sensor. 
  #Describe whether you think this might be an issue.
weather$month <- month(weather$dateF)
weather_jan_mar_21 <- weather %>% filter(year == 2021 & month <= 3)
ggplot(weather_jan_mar_21, aes(x = dateF, y = AirTemp)) + geom_line()

#Question 5
  #You are asked for total daily precipitation in March and April of 2021. 
  #Use a for loop to exclude (convert to NA) any days that include temperatures 
  #less than 35 degrees F on that day or the day prior to ensure that 
  #measurements are not likely to be affected by snow accumulation on the sensor. 
  #How many daily observations have precipitation observations (not a NA) in 
  #your final data table?

weather_mar_apr_21 <- weather %>% filter(year == 2021 & month %in% c(3,4))
weather_mar_apr_21 <- weather_mar_apr_21 %>%
  mutate(AirTempF = (AirTemp*(9/5)) + 32)

for (i in (2: length(weather_mar_apr_21$AirTemp))) {
  weather_mar_apr_21$Precip[i] <- ifelse(weather_mar_apr_21$AirTempF[i] < 35 | weather_mar_apr_21$AirTempF[i-1] < 35, NA, weather_mar_apr_21$Precip[i])
}

sum(as.integer(!is.na(weather_mar_apr_21$Precip)))

# 3,858 observations still remain
  
#Question 6
  #Copy the URL to your R script here.
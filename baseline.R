rm(list = ls())

library(lubridate)  
library(dplyr)
library(tidyverse)
library(ggpubr)
library(openair)
library(openairmaps)
library(ggplot2)
library(worldmet)
library(zoo)
library(viridis)

######### 
# SCRIPT 2 
######### 
# This script performs the baseline correction (first part of correction)
# It takes the hourly 5th percentile PM measurement at each site for each hour,
# finds the median, calculates each sensor's offset from the median, then 
# subtracts the offset from each measurement. We're currently experimenting with
# new/different methods of calculating the baseline — stay tuned!
# Always change path names to reflect your setup.
#########

load("data/all_sensors_2023.RData")

# Create hourly dataframe
All_Sensors_hourly <- All_Sensors_23 %>%
  mutate(datetime = ymd_hms(datetime),
         dateHourly = floor_date(datetime, unit = "hours"))

# Find 5th percentile for each sensor by hour
All_Sensors_sensorbaseline <- All_Sensors_hourly %>%
  group_by(node_id,dateHourly) %>%
  summarise(baselinesensor5pct = quantile(pm2_5, probs = c(0.05), na.rm = TRUE))

# Calculate median of all the 5th percentiles
All_Sensors_networkbaseline <- All_Sensors_sensorbaseline %>%
  group_by(dateHourly) %>%
  summarise(networkmedian = median(baselinesensor5pct, na.rm = TRUE))

All_Sensors_merge <- merge(All_Sensors_hourly, All_Sensors_sensorbaseline,
                           by = c("node_id","dateHourly"), all = TRUE)

All_Sensors_merge <- merge(All_Sensors_merge, All_Sensors_networkbaseline, 
                           by.x = "dateHourly", by.y = "dateHourly", all.x = TRUE)

All_Sensors_merge$offset <- All_Sensors_merge$baselinesensor5pct - All_Sensors_merge$networkmedian

All_Sensors_merge$pm2_5corrected <- All_Sensors_merge$pm2_5 - All_Sensors_merge$offset


# Adding sensor names
sensor_mapping <- data.frame(
  node_id = c(250, 254, 258, 
              261, 264, 267, 
              270, 274, 276, 
              251, 252, 255, 
              257,259, 262,
              263, 266, 269, 
              272, 253, 256, 
              260, 265, 268, 
              271),
  sensor = c("Sensor01", "Sensor02", "Sensor03", 
             "Sensor04", "Sensor05","Sensor06", 
             "Sensor07", "Sensor08", "Sensor09", 
             "Sensor10", "Sensor11", "Sensor12", 
             "Sensor13","Sensor14", "Sensor15",
             "Sensor16", "Sensor17", "Sensor18", 
             "Sensor19", "Sensor20", "Sensor21", # THERE WAS AN ERROR IN THIS LINE: MAY NEED TO CORRECT SETS AGAIN
             "Sensor22", "Sensor23", "Sensor24",
             "Sensor25")
)


All_Sensors_merge <- merge(All_Sensors_merge, sensor_mapping, by = "node_id", all.x = TRUE)

# Save baseline-corrected data
save(All_Sensors_merge, file = "data/all_sensors_baseline_corrected.RData")

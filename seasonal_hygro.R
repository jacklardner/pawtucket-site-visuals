rm(list = ls())

library(lubridate)  
library(dplyr)
library(tidyverse)
library(ggpubr)
library(openair)
library(openairmaps)
library(ggplot2)
library(plotly)
library(worldmet)
library(Metrics)
library(lvmisc)
library(zoo)

######### 
# SCRIPT 3 
######### 
# This script performs the hygroscopic correction, using baseline-corrected sensor01
# to calculate m and k. M and k are fit seasonally by meteorological season and year
# and are applied to all sensors. 
# Winter is handled differently because it spans multiple calendar years.
#########

#################################################################
# Importing and preparing the data

# Import baseline corrected Rdata file
load("data/all_sensors_baseline_corrected.RData")

# Sort by date and sensor
All_Sensors_merge_sorted.df <- All_Sensors_merge %>% 
  arrange(sensor,datetime)

# Make hourly averages
All_Sensors_merge_sorted.df$hour <- hour(All_Sensors_merge_sorted.df$dateHourly)
All_Sensors_merge_sorted.df$day <- day(All_Sensors_merge_sorted.df$dateHourly)
All_Sensors_merge_sorted.df$month <- month(All_Sensors_merge_sorted.df$dateHourly)
All_Sensors_merge_sorted.df$year <- year(All_Sensors_merge_sorted.df$dateHourly)

All_Sensors_baseline_hourly <- All_Sensors_merge_sorted.df %>%
  group_by(sensor, day, hour, month, year) %>%
  summarise(
    avg_rh = mean(rh, na.rm = TRUE),
    avg_pm2_5raw = mean(pm2_5, na.rm = TRUE),
    avg_pm2_5corrected = mean(pm2_5corrected, na.rm = TRUE)
  )

All_Sensors_baseline_hourly <- All_Sensors_baseline_hourly %>%
  mutate(dateHourly = make_datetime(year, month, day, hour))

All_Sensors_baseline_hourly <- All_Sensors_baseline_hourly %>%
  ungroup()
All_Sensors_baseline_hourly <- select(All_Sensors_baseline_hourly, -day, -hour)

save(All_Sensors_baseline_hourly, file = "data/all_sensors_baseline_hourly.RData")

#################################################################


# Import DEM data
DEM_23 <- read_csv("data/EP_PM2.5_AQS_2023.csv")

# Clean up columns
col_names <- c("date_utc", "time_utc", "pm_ref")

# Bind years together
DEM_ep <- bind_rows(DEM_23)

# Clean up columns and date
datetime_str <- paste(DEM_ep$date_utc, DEM_ep$time_utc)
DEM_ep$datetime <- as.POSIXct(datetime_str, tz = "GMT", format = "%m/%d/%y %H:%M") # change this format depending on what it is
DEM_ep$dateHourly <- DEM_ep$datetime
DEM_ep$dateHourly <- format(DEM_ep$dateHourly, tz="America/New_York",usetz=FALSE)
DEM_ep$dateHourly <- as.POSIXct(DEM_ep$dateHourly)
DEM_ep <- select(DEM_ep, "dateHourly","pm_ref")

# Plot and examine reference data
ggplot(DEM_ep) + geom_line(aes(x=dateHourly,y=pm_ref))

# Isolate sensor 1
sensor01 <- filter(All_Sensors_baseline_hourly, sensor == "Sensor01")

# Merge Sensor 1 and DEM data into new hourly dataframes
EP_merge <- merge(sensor01, DEM_ep, by = "dateHourly")

# Plot correlation of baseline corrected and DEM at EP
ggplot(data=EP_merge,aes(x=pm_ref, y=avg_pm2_5corrected)) + geom_point()+
  geom_smooth(method="lm")+ stat_regline_equation(label.x=8, label.y=0) +
  stat_cor(aes(label=..rr.label..), label.x=8, label.y=-2) +
  ggtitle("Sensor 1 (hourly average, baseline corrected) vs EP DEM")

#################################################################
# Seasons

# create a season column
EP_merge$season <- ifelse(EP_merge$month %in% c(12, 1, 2), "Winter",
                          ifelse(EP_merge$month %in% c(3, 4, 5), "Spring",
                                 ifelse(EP_merge$month %in% c(6, 7, 8), "Summer",
                                        ifelse(EP_merge$month %in% c(9, 10, 11), "Fall", NA))))



# concatenate the year to the season, handling the winter months separately
EP_merge$season_year <- ifelse(EP_merge$month == 12, 
                               paste("Winter", EP_merge$year, EP_merge$year + 1, sep = "_"),
                               ifelse(EP_merge$month %in% c(1, 2), 
                                      paste("Winter", EP_merge$year - 1, EP_merge$year, sep = "_"),
                                      paste(EP_merge$season, EP_merge$year, sep = "_")))

#################################################################
# Developing the model and applying the coefficients

# defining the empirical formula
hygro.eq <- function(x, RH, m, k) {
  x * m / (1 + k / ((100 / RH) - 1))
}

szns_ep <- unique(EP_merge$season_year)

# Create an empty dataframe to store m, k and season
fit_results_ep <- data.frame()

# Run model for EP
for (szn in szns_ep) { 
  data <- EP_merge %>% filter(season_year == szn) 
  x <- data$avg_pm2_5corrected  # baseline corrected LCS measurement, hourly 
  y <- data$pm_ref  # reference data, hourly 
  RH <- data$avg_rh  # LCS RH, hourly  
  df <- data.frame(y, x, RH) 
  df <- df %>% drop_na() 
  fit <- nls(y ~ hygro.eq(x, RH, m, k), data = df, start = list(m = 0.5, k = 1)) 
  fit_coefs <- coef(fit) 
  m <- as.numeric(fit_coefs[1]) 
  k <- as.numeric(fit_coefs[2]) 
  
  # Add the results to the dataframe
  fit_results_ep <- rbind(fit_results_ep, data.frame(season_year = szn, m = m, k = k)) 
}


#################################################################
# Apply fits to all sensors

# Merge all sensors with DEM data to prepare for applying fit
sensor_fit_merge_ep <- merge(DEM_ep, All_Sensors_baseline_hourly, by = "dateHourly")

# Add m and k values to sensor_fit_merge based on season
# Create a season column in sensor_fit_merge
# create a season column
sensor_fit_merge_ep$season <- ifelse(sensor_fit_merge_ep$month %in% c(12, 1, 2), "Winter",
                                     ifelse(sensor_fit_merge_ep$month %in% c(3, 4, 5), "Spring",
                                            ifelse(sensor_fit_merge_ep$month %in% c(6, 7, 8), "Summer",
                                                   ifelse(sensor_fit_merge_ep$month %in% c(9, 10, 11), "Fall", NA))))


# concatenate the year to the season, handling the winter months separately
sensor_fit_merge_ep$season_year <- ifelse(sensor_fit_merge_ep$month == 12, 
                                          paste("Winter", sensor_fit_merge_ep$year, sensor_fit_merge_ep$year + 1, sep = "_"),
                                          ifelse(sensor_fit_merge_ep$month %in% c(1, 2), 
                                                 paste("Winter", sensor_fit_merge_ep$year - 1, sensor_fit_merge_ep$year, sep = "_"),
                                                 paste(sensor_fit_merge_ep$season, sensor_fit_merge_ep$year, sep = "_")))


sensor_fit_merge_ep <- merge(sensor_fit_merge_ep, fit_results_ep, by = "season_year")


sensor_fit_merge_ep$pm2_5_dry <- (sensor_fit_merge_ep$avg_pm2_5corrected)*(sensor_fit_merge_ep$m/(1+sensor_fit_merge_ep$k/((100/sensor_fit_merge_ep$avg_rh)-1)))
all_sensors_full_correction.df <- bind_rows(sensor_fit_merge_ep)

#################################################################

# Clean up columns
all_sensors_full_correction.df <- select(all_sensors_full_correction.df, -month, -year, -season, -m, -k)
colnames(all_sensors_full_correction.df) <- c("season_year", "date","pm_ref",
                                              "sensor","rh","pm_raw","pm_baseline",
                                              "pm_baseline_hygro")

save(all_sensors_full_correction.df, file = "data/all_sensors_full_correction_seasonal.RData")
write_csv(all_sensors_full_correction.df, file = "data/all_sensors_full_correction_seasonal.csv")

#################################################################
# Plotting 

load("data/all_sensors_full_correction_seasonal.RData")


#Plots
corrected_plot <- ggplot(all_sensors_full_correction.df, aes(x=date,y=pm_baseline_hygro,color=sensor)) + geom_line()
ggplotly(corrected_plot)


all_sensors_full_correction_roll24.df <- all_sensors_full_correction.df %>%
  arrange(date)%>%
  arrange(sensor)%>%
  group_by(sensor) %>%
  mutate(roll24 = rollmean(pm_baseline_hygro, 24, fill = NA, align = "left")) %>%
  mutate(roll15 = rollmean(pm_baseline_hygro, 15, fill = NA, align = "left")) %>%
  ungroup()


ggplot(filter(all_sensors_full_correction_roll24.df, date >= "2023-06-03 12:00:00" & date <= "2023-06-10 00:00:00"), aes(x=date,y=roll24,color=sensor)) + geom_line()

timeVariation(all_sensors_full_correction.df, pollutant = "pm_baseline_hygro", key.columns = 4)

colnames(all_sensors_full_correction.df)[4] <- "site"

all_sensors_full_correction_hourly.df <- all_sensors_full_correction.df %>%
  group_by(site, date) %>%
  summarise(pm_baseline_hygro_hourly = mean(pm_baseline_hygro, na.rm = TRUE))

summaryPlot(all_sensors_full_correction_hourly.df, pollutant = "pm_baseline_hygro_hourly")

april.df <- filter(all_sensors_full_correction_hourly.df, date >= "2023-04-15 00:00:00" & date <= "2024-04-15 00:00:00")

summaryPlot(april.df, pollutant = "pm_baseline_hygro_hourly")

# count by sensor
april_count.df <- april.df %>%
  group_by(site) %>%
  summarise(
    count = n()
  )

# add expected hours for 1/1/24 - 12/1/24
april_count.df$expected <- 8760

# calculate completeness
april_count.df$completeness <- april_count.df$count / april_count.df$expected


sensor1_full_correction.df <- filter(all_sensors_full_correction.df, sensor == "Sensor01")

ggplot(data=sensor1_full_correction.df,aes(x=pm_ref, y=pm_baseline_hygro)) + geom_point()+
  geom_smooth(method="lm")+ stat_regline_equation(label.x=20, label.y=5.5) +
  stat_cor(aes(label=..rr.label..), label.x=15, label.y=2) +
  ggtitle("Sensor 1 (baseline and hygro corrected, hourly) vs DEM")

#ggplot(All_Sensors_baseline_hourly) + geom_line(aes(x=dateHourly,y=avg_pm2_5raw,color=sensor))

sensor14_full_correction.df <- filter(all_sensors_full_correction.df, sensor == "Sensor14")

ggplot(data=sensor14_full_correction.df,aes(x=date, y=pm_baseline_hygro)) + geom_line()

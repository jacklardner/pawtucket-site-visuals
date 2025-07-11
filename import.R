rm(list = ls())

library(lubridate)  
library(dplyr)
library(tidyverse)
library(ggpubr)
library(openair)
library(openairmaps)
library(ggplot2)
library(worldmet)
library(httr)

######### 
# SCRIPT 1 
######### 
# This script loads raw sensor data at minute resolution and combines it into a df
# It's written so you perform the process for each year and merge together at the end
# Pulling data for more than a year at once tends to time out the process
# Make sure to change the year everywhere in the script 
# Always change path names to reflect your setup
# NOTE: there are two download methods here — the first one tends to time out more,
# so try the second one if it's not working.
#########

###############################################################################
# Load raw sensor data

#Establish study period
Start_Date <- "2023-01-01" #Format:  YYYY-MM-DD
Start_Time <- "00:00:00"   #Format:  hh:mm:ss I think this time is PST
End_Date <- "2024-01-01"  #Format: YYYY-MM_DD
End_Time <- "12:00:00"    #Format: hh:mm:ss I think this time is PST

#List
Sensor_Names <- c("Sensor01","Sensor02","Sensor03", 
                  "Sensor04","Sensor05","Sensor06",
                  "Sensor07","Sensor08","Sensor09",
                  "Sensor10","Sensor11","Sensor12", 
                  "Sensor13","Sensor14","Sensor15",
                  "Sensor16","Sensor17","Sensor18",
                  "Sensor19","Sensor20","Sensor21",
                  "Sensor22","Sensor23","Sensor24",
                  "Sensor25")

#Nodes
Sensor01_Node <- 250
Sensor02_Node <- 254
Sensor03_Node <- 258
Sensor04_Node <- 261
Sensor05_Node <- 264
Sensor06_Node <- 267
Sensor07_Node <- 270
Sensor08_Node <- 274
Sensor09_Node <- 276
Sensor10_Node <- 251
Sensor11_Node <- 252
Sensor12_Node <- 255
Sensor13_Node <- 257
Sensor14_Node <- 259
Sensor15_Node <- 262
Sensor16_Node <- 263
Sensor17_Node <- 266
Sensor18_Node <- 269
Sensor19_Node <- 272
Sensor20_Node <- 253
Sensor21_Node <- 256
Sensor22_Node <- 260
Sensor23_Node <- 265
Sensor24_Node <- 268
Sensor25_Node <- 271

Sensor_Nodes <- c(Sensor01_Node, Sensor02_Node, Sensor03_Node,  
                  Sensor04_Node, Sensor05_Node, Sensor06_Node,
                  Sensor07_Node, Sensor08_Node, Sensor09_Node, 
                  Sensor10_Node, Sensor11_Node, Sensor12_Node, 
                  Sensor13_Node, Sensor14_Node, Sensor15_Node, 
                  Sensor16_Node, Sensor17_Node, Sensor18_Node,
                  Sensor19_Node, Sensor20_Node, Sensor21_Node,
                  Sensor22_Node, Sensor23_Node, Sensor24_Node,
                  Sensor25_Node)

# Create a list to store the URLs
sensor_urls <- list()

# Loop to generate the URLs 
for (i in 1:length(Sensor_Names)) {
  url <- paste("http://", "128.32.208.8/node/", Sensor_Nodes[i], "/measurements_all/csv?name=", 
               noquote(Sensor_Names[i]), "&interval=1&variables=pm2_5,rh", 
               "&start=", noquote(Start_Date), "%20", noquote(Start_Time), 
               "&end=", noquote(End_Date), "%20", noquote(End_Time), "&chart_type=measurement", sep = "")
  sensor_urls[[i]] <- url
}

###############################################################################
# METHOD 1: download data and read it using read.csv for each sensor

#Download Sensor Data to Working Directory
Sensor01_Data_23 <- read.csv(sensor_urls[[1]])
Sensor02_Data_23 <- read.csv(sensor_urls[[2]])
Sensor03_Data_23 <- read.csv(sensor_urls[[3]])
Sensor04_Data_23 <- read.csv(sensor_urls[[4]])
Sensor05_Data_23 <- read.csv(sensor_urls[[5]])
Sensor06_Data_23 <- read.csv(sensor_urls[[6]])
Sensor07_Data_23 <- read.csv(sensor_urls[[7]])
Sensor08_Data_23 <- read.csv(sensor_urls[[8]])
Sensor09_Data_23 <- read.csv(sensor_urls[[9]])
Sensor10_Data_23 <- read.csv(sensor_urls[[10]])
Sensor11_Data_23 <- read.csv(sensor_urls[[11]])
Sensor12_Data_23 <- read.csv(sensor_urls[[12]])
Sensor13_Data_23 <- read.csv(sensor_urls[[13]])
Sensor14_Data_23 <- read.csv(sensor_urls[[14]])
Sensor15_Data_23 <- read.csv(sensor_urls[[15]])
Sensor16_Data_23 <- read.csv(sensor_urls[[16]])
Sensor17_Data_23 <- read.csv(sensor_urls[[17]])
Sensor18_Data_23 <- read.csv(sensor_urls[[18]])
Sensor19_Data_23 <- read.csv(sensor_urls[[19]])
Sensor20_Data_23 <- read.csv(sensor_urls[[20]])
Sensor21_Data_23 <- read.csv(sensor_urls[[21]])
Sensor22_Data_23 <- read.csv(sensor_urls[[22]])
Sensor23_Data_23 <- read.csv(sensor_urls[[23]])
Sensor24_Data_23 <- read.csv(sensor_urls[[24]])
Sensor25_Data_23 <- read.csv(sensor_urls[[25]])

All_Sensors_23 <- bind_rows(Sensor01_Data_23, Sensor02_Data_23, Sensor03_Data_23,  
                            Sensor04_Data_23, 
                            Sensor05_Data_23, 
                            Sensor06_Data_23, Sensor07_Data_23, Sensor08_Data_23, 
                            Sensor09_Data_23, 
                            Sensor10_Data_23, Sensor11_Data_23, 
                            Sensor12_Data_23, 
                            Sensor13_Data_23, Sensor14_Data_23, 
                            Sensor15_Data_23, 
                            Sensor16_Data_23, Sensor17_Data_23, 
                            Sensor18_Data_23,
                            Sensor19_Data_23, 
                            Sensor20_Data_23, 
                            Sensor21_Data_23,
                            Sensor22_Data_23, 
                            Sensor23_Data_23, Sensor24_Data_23, 
                            Sensor25_Data_23)

# Save each year to bind together later
save(All_Sensors_23, file = "data/all_sensors_2023.RData")


###############################################################################
# METHOD 2: download data with download.data (less likely to time out), GET, and libcurl
# these functions will retry downloads if the initial one fails/is incomplete
# it will create a directory in your project folder to store the downloaded csvs before binding
# ChatGPT helped with this one, so I'm less able to explain any bugs

# Create directory
dir.create("sensor_data", showWarnings = FALSE)  

download_with_retries <- function(url, file_path, max_retries = 3) {
  attempt <- 1
  success <- FALSE
  
  while (attempt <= max_retries && !success) {
    # Try with httr::GET() and timeout
    response <- tryCatch({
      GET(url, write_disk(file_path, overwrite = TRUE), timeout(120))
    }, error = function(e) {
      return(NULL)
    })
    
    # If GET fails, fallback to download.file()
    if (is.null(response) || http_status(response)$category != "Success" || file.info(file_path)$size < 1000) {
      message(paste("Attempt", attempt, "failed. Retrying with download.file..."))
      tryCatch({
        download.file(url, file_path, method = "libcurl", timeout = 200, quiet = TRUE)
      }, error = function(e) {
        message(paste("Download failed on attempt", attempt))
      })
    }
    
    # Check if file exists and is not empty
    if (file.exists(file_path) && file.info(file_path)$size > 1000) {
      success <- TRUE
      message(paste("Downloaded:", file_path))
    } else {
      attempt <- attempt + 1
      Sys.sleep(2)  # Short pause before retrying
    }
  }
  
  if (!success) {
    message(paste("Failed to download:", file_path))
  }
}

# Loop through sensors and download data
for (i in 1:length(Sensor_Names)) {
  file_path <- paste0("sensor_data/", Sensor_Names[i], ".csv")
  download_with_retries(sensor_urls[[i]], file_path)
}

# Read csvs into a List
sensor_data_list <- lapply(Sensor_Names, function(sensor) {
  file_path <- paste0("sensor_data/", sensor, ".csv")
  if (file.exists(file_path) && file.info(file_path)$size > 1000) {
    read.csv(file_path)
  } else {
    message(paste("Skipping incomplete file:", file_path))
    NULL
  }
})

# Combine all data into one df
All_Sensors_23 <- bind_rows(sensor_data_list, .id = "Sensor")

# Save each year to bind together later
save(All_Sensors_23, file = "data/all_sensors_2023.RData")


###############################################################################
# Bind all years together

# load("unfiltered_uncorrected_lcs/all_sensors_2022.RData")
load("unfiltered_uncorrected_lcs/all_sensors_2023.RData")
# load("unfiltered_uncorrected_lcs/all_sensors_2024.RData")

all_sensors <- bind_rows(All_Sensors_23)

###############################################################################
# Clean up formatting and convertings to eastern time

all_sensors$dateGMT <- as.POSIXct(all_sensors$datetime, format = "%Y-%m-%d %H:%M:%S",tz="GMT")
all_sensors$dateEST <- format(all_sensors$dateGMT,tz = "America/New_York",usetz = FALSE)
all_sensors$dateEST <- as.POSIXct(all_sensors$dateEST,format = "%Y-%m-%d %H:%M:%S")

all_sensors <- select(all_sensors, node_id, pm2_5, rh, temp, dateEST)


###############################################################################
# Filtering for only installed + functioning periods for PM
# (slightly diff than for gases, which need day removed when was unplugged at all)

#1
all_sensors <- all_sensors %>%
  filter(!(node_id == 250 & dateEST <= "2022-07-12 00:00:00")) %>%
  filter(!(node_id == 250 & dateEST >= "2024-04-16 00:00:00" & dateEST <= "2024-04-23 23:00:00"))
#2
all_sensors <- all_sensors %>%
  filter(!(node_id == 254 & dateEST <= "2022-12-01 23:00:00"))
#3
all_sensors <- all_sensors %>%
  filter(!(node_id == 258 & dateEST <= "2022-11-17 23:00:00"))
#4
all_sensors <- all_sensors %>%
  filter(!(node_id == 261 & dateEST <= "2022-11-17 23:00:00"))
#5
all_sensors <- all_sensors %>%
  filter(!(node_id == 264 & dateEST <= "2022-11-17 23:00:00")) %>%
  filter(!(node_id == 264 & dateEST >= "2024-07-04 00:00:00" & dateEST <= "2024-08-09 23:00:00"))
#6
all_sensors <- all_sensors %>%
  filter(!(node_id == 267 & dateEST <= "2022-08-25 23:00:00")) %>%
  filter(!(node_id == 267 & dateEST >= "2023-01-19 00:00:00" & dateEST <= "2023-01-25 00:00:00"))
#7
all_sensors <- all_sensors %>%
  filter(!(node_id == 270 & dateEST <= "2022-08-25 23:00:00"))
#8
all_sensors <- all_sensors %>%
  filter(!(node_id == 274 & dateEST <= "2022-07-28 23:00:00")) %>%
  filter(!(node_id == 274 & dateEST >= "2024-01-10 00:00:00" & dateEST <= "2024-01-18 23:00:00"))
#9
all_sensors <- all_sensors %>%
  filter(!(node_id == 276 & dateEST <= "2022-08-09 23:00:00"))
#10
all_sensors <- all_sensors %>%
  filter(!(node_id == 251 & dateEST <= "2022-10-04 23:00:00")) %>%
  filter(!(node_id == 251 & dateEST >= "2024-9-30 00:00:00"))
#11
all_sensors <- all_sensors %>%
  filter(!(node_id == 252 & dateEST <= "2022-09-02 23:00:00")) %>% 
  filter(!(node_id == 252 & dateEST >= "2024-02-07 00:00:00" & dateEST <= "2024-02-15 23:00:00"))
#12
all_sensors <- all_sensors %>%
  filter(!(node_id == 255 & dateEST <= "2022-11-19 23:00:00"))
#13
all_sensors <- all_sensors %>%
  filter(!(node_id == 257 & dateEST <= "2022-08-09 23:00:00"))
#14
all_sensors <- all_sensors %>%
  filter(!(node_id == 259 & dateEST <= "2022-10-27 23:00:00")) %>%
  filter(!(node_id == 259 & dateEST >= "2023-04-26 00:00:00" & dateEST <= "2023-08-07 23:00:00"))
#15
all_sensors <- all_sensors %>%
  filter(!(node_id == 262 & dateEST <= "2022-11-11 23:00:00"))
#16
all_sensors <- all_sensors %>%
  filter(!(node_id == 263 & dateEST <= "2022-08-25 23:00:00"))
#17
all_sensors <- all_sensors %>%
  filter(!(node_id == 266 & dateEST <= "2023-03-07 23:00:00")) %>%
  filter(!(node_id == 266 & dateEST >= "2024-02-07 00:00:00" & dateEST <= "2024-02-09 23:00:00"))
#18
all_sensors <- all_sensors %>%
  filter(!(node_id == 269 & dateEST <= "2023-02-02 23:00:00"))
#19
all_sensors <- all_sensors %>%
  filter(!(node_id == 272 & dateEST <= "2022-12-15 23:00:00"))
#20 - many problems with the Rock sensor, may just use data from 5/9/24 onward
all_sensors <- all_sensors %>%
  filter(!(node_id == 253 & dateEST <= "2024-05-09 23:00:00"))
#21
all_sensors <- all_sensors %>%
  filter(!(node_id == 256 & dateEST <= "2023-05-25 23:00:00"))
#22
all_sensors <- all_sensors %>%
  filter(!(node_id == 260 & dateEST <= "2023-09-01 23:00:00"))
#23
all_sensors <- all_sensors %>%
  filter(!(node_id == 265 & dateEST <= "2023-03-16 23:00:00")) %>%
  filter(!(node_id == 265 & dateEST >= "2023-07-07 00:00:00" & dateEST <= "2023-09-01 23:00:00")) %>%
  filter(!(node_id == 265 & dateEST >= "2024-11-14 00:00:00" & dateEST <= "2024-11-20 23:00:00")) 
#24
all_sensors <- all_sensors %>%
  filter(!(node_id == 268 & dateEST <= "2023-03-16 23:00:00")) 
#25
all_sensors <- all_sensors %>%
  filter(!(node_id == 271 & dateEST <= "2024-08-23 23:00:00")) 

# Filter for under 500 ug/m3
all_sensors <- filter(all_sensors, pm2_5 < 500)


# Save filtered and date-corrected data
save(all_sensors, file = "data/filtered_datesgood_uncorrected.RData")


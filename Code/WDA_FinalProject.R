#Research questions:
#1. How long is flood duration at each gage?
#2. How is the water height change at each gage? (max - min)/avg?
#3. How long is the lag of flood peak moving along the river? 
#4. How to visualize these?


#Set up#####################
#Check working directory
getwd()

#Load packages
library(tidyverse)
library(dplyr)
library(ggpubr)
library(dataRetrieval)
library(zoo)
library(sf)
library(nhdplusTools)
library(viridis)

#Create a ggplot theme
MyTheme <- theme_bw(base_size = 14) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0))

#Data retrieval and exploratory analysis ################

#Gage 1: USGS 02080500 ROANOKE RIVER AT ROANOKE RAPIDS, NC===========
Gage1_info <- readNWISsite("02080500")[,c(1:3, 7:8)]

#Obtain the data
Gage1 <- readNWISdv(siteNumbers = "02080500",
           parameterCd = "00065", # Water height (feet)
           startDate = "2019-02-01",
           endDate = "2019-04-05")
names(Gage1)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage1 <- Gage1 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage1_plot <- ggplot(Gage1, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 1 - 02080500",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                 "5-day average" = "blue")) +
  MyTheme

#Gage 2: USGS 0208062765 ROANOKE RIVER AT HALIFAX, NC====================
Gage2_info <- readNWISsite("0208062765")[,c(1:3, 7:8)]

#Obtain the data
Gage2 <- readNWISdv(siteNumbers = "0208062765",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-01",
                    endDate = "2019-04-05")
names(Gage2)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage2 <- Gage2 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage2_plot <- ggplot(Gage2, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 2 - 0208062765",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                "5-day average" = "blue")) +
  MyTheme
Gage2_plot

#Gage 3: USGS 02081000 ROANOKE RIVER NEAR SCOTLAND NECK, NC======================
Gage3_info <- readNWISsite("02081000")[,c(1:3, 7:8)]

#Obtain the data
Gage3 <- readNWISdv(siteNumbers = "02081000",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-01",
                    endDate = "2019-04-05")
names(Gage3)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage3 <- Gage3 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage3_plot <- ggplot(Gage3, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 3 - 02081000",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                "5-day average" = "blue")) +
  MyTheme
Gage3_plot

#Gage 4: USGS 02081022 ROANOKE RIVER NEAR OAK CITY, NC===========================
Gage4_info <- readNWISsite("02081022")[,c(1:3, 7:8)]

#Obtain the data
Gage4 <- readNWISdv(siteNumbers = "02081022",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-01",
                    endDate = "2019-04-05")
names(Gage4)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage4 <- Gage4 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage4_plot <- ggplot(Gage4, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 4 - 02081022",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                "5-day average" = "blue")) +
  MyTheme
Gage4_plot


#Gage 5: USGS 02081028 ROANOKE RIVER AT HAMILTON, NC===============================
Gage5_info <- readNWISsite("02081028")[,c(1:3, 7:8)]

#Obtain the data
Gage5 <- readNWISdv(siteNumbers = "02081028",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-01",
                    endDate = "2019-04-05")
names(Gage5)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage5 <- Gage5 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage5_plot <- ggplot(Gage5, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 5 - 02081028",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                "5-day average" = "blue")) +
  MyTheme
Gage5_plot


#Gage 6: USGS 02081054 ROANOKE RIVER AT WILLIAMSTON, NC=====================
Gage6_info <- readNWISsite("02081054")[,c(1:3, 7:8)]

#Obtain the data
Gage6 <- readNWISdv(siteNumbers = "02081054",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-01",
                    endDate = "2019-04-05")
names(Gage6)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage6 <- Gage6 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage6_plot <- ggplot(Gage6, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 6 - 02081054",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                "5-day average" = "blue")) +
  MyTheme
Gage6_plot


#Gage 7: USGS 02081094 ROANOKE RIVER AT JAMESVILLE, NC========================
Gage7_info <- readNWISsite("02081094")[,c(1:3, 7:8)]

#Obtain the data
Gage7 <- readNWISdv(siteNumbers = "02081094",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-01",
                    endDate = "2019-04-05")
names(Gage7)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage7 <- Gage7 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage7_plot <- ggplot(Gage7, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 7 - 02081094",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                "5-day average" = "blue")) +
  MyTheme
Gage7_plot


#Gage 8: USGS 0208114150 ROANOKE RIVER AT NC 45 NR WESTOVER, NC=========================
Gage8_info <- readNWISsite("0208114150")[,c(1:3, 7:8)]

#Obtain the data
Gage8 <- readNWISdv(siteNumbers = "0208114150",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-01",
                    endDate = "2019-04-05")
names(Gage8)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage8 <- Gage8 %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

#Preliminary visualization
Gage8_plot <- ggplot(Gage8, aes(x = Date)) +
  geom_line(aes(y = Water_Height, col = "Original")) +
  geom_line(aes(y = avg_5d, col = "5-day average"), size = 1) +
  labs(title = "Gage 8 - 0208114150",
       y = "Water height (ft)") + 
  scale_x_date(date_labels = "%m/%d") +
  scale_color_manual(name = " ", 
                     values = c("Original" = "black", 
                                "5-day average" = "blue")) +
  MyTheme
Gage8_plot


#Combine the plots===========================
ggarrange(Gage1_plot, Gage2_plot, Gage3_plot, Gage4_plot, 
          Gage5_plot, Gage6_plot, Gage7_plot, Gage8_plot,
          ncol = 4, nrow = 2)
  

#Analysis#################################

#Gage 1 ==================================
#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage1_startdate <- Gage1$Date[min(which(diff(Gage1$avg_5d) > 0.5, FALSE))]

#Remove the days before the start date to find the end date
Gage1_subset <- subset(Gage1, Date >= Gage1_startdate)

#Find the end date - the last day of the pulse 
Gage1_enddate <- Gage1_subset$Date[max(which(Gage1_subset$diff < 0))]

#flood duration = end date - start date
Gage1_duration <- as.numeric(Gage1_enddate - Gage1_startdate)

#peak = the date with the max water height 
Gage1_peak <- Gage1_subset$Date[which.max(Gage1_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage1_flood <- Gage1 %>%
  subset(Date >= Gage1_startdate & Date <= Gage1_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage1_flood_rate <- (max(Gage1_flood$avg_5d) - min(Gage1_flood$avg_5d))/min(Gage1_flood$avg_5d)

#Create a list for the flood information
Gage1_summary <- list(Gage1_startdate, Gage1_enddate, Gage1_peak, Gage1_duration, Gage1_flood_rate)

#Gage 2===============================
#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage2_startdate <- Gage2$Date[min(which(diff(Gage2$avg_5d) > 1, FALSE))]

#Remove the days before the start date to find the end date
Gage2_subset <- subset(Gage2, Date >= Gage2_startdate)

#Find the end date - the last day of the pulse 
Gage2_enddate <- Gage2_subset$Date[max(which(Gage2_subset$diff < 0))]

#flood duration = end date - start date
Gage2_duration <- as.numeric(Gage2_enddate - Gage2_startdate)

#peak = the date with the max water height 
Gage2_peak <- Gage2_subset$Date[which.max(Gage2_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage2_flood <- Gage2 %>%
  subset(Date >= Gage2_startdate & Date <= Gage2_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage2_flood_rate <- (max(Gage2_flood$avg_5d) - min(Gage2_flood$avg_5d))/min(Gage2_flood$avg_5d)

#Create a list for the flood information
Gage2_summary <- list(Gage2_startdate, Gage2_enddate, Gage2_peak, Gage2_duration, Gage2_flood_rate)


#Gage3 ==================================
#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage3_startdate <- Gage3$Date[min(which(diff(Gage3$avg_5d) > 1, FALSE))]

#Remove the days before the start date to find the end date
Gage3_subset <- subset(Gage3, Date >= Gage3_startdate)

#Find the end date - the last day of the pulse 
Gage3_enddate <- Gage3_subset$Date[max(which(Gage3_subset$diff < 0))]

#flood duration = end date - start date
Gage3_duration <- as.numeric(Gage3_enddate - Gage3_startdate)

#peak = the date with the max water height 
Gage3_peak <- Gage3_subset$Date[which.max(Gage3_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage3_flood <- Gage3 %>%
  subset(Date >= Gage3_startdate & Date <= Gage3_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage3_flood_rate <- (max(Gage3_flood$avg_5d) - min(Gage3_flood$avg_5d))/min(Gage3_flood$avg_5d)

#Create a list for the flood information
Gage3_summary <- list(Gage3_startdate, Gage3_enddate, Gage3_peak, Gage3_duration, Gage3_flood_rate)


#Gage4 ==================================
#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage4_startdate <- Gage4$Date[min(which(diff(Gage4$avg_5d) > 0.5, FALSE))]

#Remove the days before the start date to find the end date
Gage4_subset <- subset(Gage4, Date >= Gage4_startdate)

#Find the end date - the last day of the pulse 
Gage4_enddate <- Gage4_subset$Date[max(which(Gage4_subset$diff < 0))]

#flood duration = end date - start date
Gage4_duration <- as.numeric(Gage4_enddate - Gage4_startdate)

#peak = the date with the max water height 
Gage4_peak <- Gage4_subset$Date[which.max(Gage4_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage4_flood <- Gage4 %>%
  subset(Date >= Gage4_startdate & Date <= Gage4_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage4_flood_rate <- (max(Gage4_flood$avg_5d) - min(Gage4_flood$avg_5d))/min(Gage4_flood$avg_5d)

#Create a list for the flood information
Gage4_summary <- list(Gage4_startdate, Gage4_enddate, Gage4_peak, Gage4_duration, Gage4_flood_rate)


#Gage5 ==================================
#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage5_startdate <- Gage5$Date[min(which(diff(Gage5$avg_5d) > 0.3, FALSE))]

#Remove the days before the start date to find the end date
Gage5_subset <- subset(Gage5, Date >= Gage5_startdate)

#Find the end date - the last day of the pulse 
Gage5_enddate <- Gage5_subset$Date[max(which(Gage5_subset$diff < 0))]

#flood duration = end date - start date
Gage5_duration <- as.numeric(Gage5_enddate - Gage5_startdate)

#peak = the date with the max water height 
Gage5_peak <- Gage5_subset$Date[which.max(Gage5_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage5_flood <- Gage5 %>%
  subset(Date >= Gage5_startdate & Date <= Gage5_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage5_flood_rate <- (max(Gage5_flood$avg_5d) - min(Gage5_flood$avg_5d))/min(Gage5_flood$avg_5d)

#Create a list for the flood information
Gage5_summary <- list(Gage5_startdate, Gage5_enddate, Gage5_peak, Gage5_duration, Gage5_flood_rate)


#Gage6 ==================================
#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage6_startdate <- Gage6$Date[min(which(diff(Gage6$avg_5d) > 0.1, FALSE))]

#Remove the days before the start date to find the end date
Gage6_subset <- subset(Gage6, Date >= Gage6_startdate)

#Find the end date - the last day of the pulse 
Gage6_enddate <- Gage6_subset$Date[max(which(Gage6_subset$diff < 0))]

#flood duration = end date - start date
Gage6_duration <- as.numeric(Gage6_enddate - Gage6_startdate)

#peak = the date with the max water height 
Gage6_peak <- Gage6_subset$Date[which.max(Gage6_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage6_flood <- Gage6 %>%
  subset(Date >= Gage6_startdate & Date <= Gage6_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage6_flood_rate <- (max(Gage6_flood$avg_5d) - min(Gage6_flood$avg_5d))/min(Gage6_flood$avg_5d)

#Create a list for the flood information
Gage6_summary <- list(Gage6_startdate, Gage6_enddate, Gage6_peak, Gage6_duration, Gage6_flood_rate)


#Gage7 ==================================
#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage7_startdate <- Gage7$Date[min(which(diff(Gage7$avg_5d) > 0.06, FALSE))]

#Remove the days before the start date to find the end date
Gage7_subset <- subset(Gage7, Date >= Gage7_startdate)

#Find the end date - the last day of the pulse 
Gage7_enddate <- Gage7_subset$Date[max(which(Gage7_subset$diff < 0))]

#flood duration = end date - start date
Gage7_duration <- as.numeric(Gage7_enddate - Gage7_startdate)

#peak = the date with the max water height 
Gage7_peak <- Gage7_subset$Date[which.max(Gage7_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage7_flood <- Gage7 %>%
  subset(Date >= Gage7_startdate & Date <= Gage7_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage7_flood_rate <- (max(Gage7_flood$avg_5d) - min(Gage7_flood$avg_5d))/min(Gage7_flood$avg_5d)

#Create a list for the flood information
Gage7_summary <- list(Gage7_startdate, Gage7_enddate, Gage7_peak, Gage7_duration, Gage7_flood_rate)


#Gage8 ==================================
#Gage8 data is funky so I refined the time frame 
#Obtain the data
Gage8_update <- readNWISdv(siteNumbers = "0208114150",
                    parameterCd = "00065", # Water height (feet)
                    startDate = "2019-02-20",
                    endDate = "2019-04-15")
names(Gage8_update)[4:5] <- c("Water_Height", "Approval.Code")

#Calculate the 5-day average water height and daily wather height difference
Gage8_update <- Gage8_update %>%
  mutate(avg_5d = rollmean(Water_Height, k = 5, fill = NA),
         diff = avg_5d - lag(avg_5d))

ggplot(Gage8_update, aes(x = Date, y = avg_5d)) +
  geom_line()

#Find the start date - the first day that the water height is 1 feet higher than the previous day
Gage8_startdate <- Gage8_update$Date[min(which(diff(Gage8_update$avg_5d) > 0.05, FALSE))]

#Remove the days before the start date to find the end date
Gage8_subset <- subset(Gage8_update, Date >= Gage8_startdate)

#Find the end date - the last day of the pulse 
Gage8_enddate <- Gage8_subset$Date[max(which(Gage8_subset$diff < 0))]

#flood duration = end date - start date
Gage8_duration <- as.numeric(Gage8_enddate - Gage8_startdate)

#peak = the date with the max water height 
Gage8_peak <- Gage8_subset$Date[which.max(Gage8_subset$avg_5d)]

#Create a dataframe of just the flood period
Gage8_flood <- Gage8_update %>%
  subset(Date >= Gage8_startdate & Date <= Gage8_enddate)

#Calculate the water height increase rate: (max-min)/min
Gage8_flood_rate <- (max(Gage8_flood$avg_5d) - min(Gage8_flood$avg_5d))/min(Gage8_flood$avg_5d)

#Create a list for the flood information
Gage8_summary <- list(Gage8_startdate, Gage8_enddate, Gage8_peak, Gage8_duration, Gage8_flood_rate)



#Create a data freame to gather all information for all 8 gages================

#Create an empty data frame for flood information
Summary_df <- data.frame(Start = as.Date(as.character()),    
                    End = as.Date(as.character()),
                    Peak = as.Date(as.character()),
                    Duration = numeric(),
                    IncreasedPercent = numeric(),
                    stringsAsFactors = FALSE)

#Add flood information
Summary_df[1,] <- Gage1_summary
Summary_df[2,] <- Gage2_summary
Summary_df[3,] <- Gage3_summary
Summary_df[4,] <- Gage4_summary
Summary_df[5,] <- Gage5_summary
Summary_df[6,] <- Gage6_summary
Summary_df[7,] <- Gage7_summary
Summary_df[8,] <- Gage8_summary

#Combine gage information
Info_df <- rbind(Gage1_info, Gage2_info, Gage3_info, Gage4_info,
                 Gage5_info, Gage6_info, Gage7_info, Gage8_info)

#Combine gage information and flood information
Flood_df <- cbind(Info_df, Summary_df)

#Visualization=================================

#Import the Roanoke River mainstem flowline=====================
#Set Gage1 as the start point
start_point <- st_sfc(st_point(c(Flood_df$dec_long_va[1], 
                                 Flood_df$dec_lat_va[1])), 
                      crs = 4269)
start_comid <- discover_nhdplus_id(start_point)

#Navigate the NLDI network
NLDI <- navigate_nldi(list(featureSource = "comid", featureID = start_comid), 
                      mode = "DM", #downstream mainstem of Gage1
                      distance_km = 1000)

#Extract watershed and flowpath information
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(NLDI$DM$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

#Extract the flowline
flowline <- subset$NHDFlowline_Network

#Plot the flowline
ggplot(flowline) +
  geom_sf()


#Create USGS gage sf ==============================
Flood_sf <- st_as_sf(Flood_df, 
                  coords = c("dec_long_va", "dec_lat_va"),
                  crs = 4269)

#Import Lower Roanoke River Basin shapefile ====================
LRB <- st_read("./Data/Basin_shp/Lower_Roanoke_River_Basin.shp")

#Map study area ===================
ggplot() +
  geom_sf(data = LRB) +
  geom_sf(data = Flood_sf, size = 2) +
  geom_sf(data = flowline) +
  MyTheme


#Plot the flood characteristics==============

#Start date
#A function to transfer date class
trans_date <- function(x){
  format(as.Date(x, origin = '1970-01-01'), format = '%m/%d')
}
#Plot
ggplot() +
  geom_sf(data = LRB, fill = "white") +
  geom_sf(data = flowline) +
  geom_sf(data = Flood_sf, aes(col = as.integer(Start)), size = 4) +
  scale_color_gradient(low = "#48CAE4", high = "#030451",
                       labels = trans_date) +
  labs(color = "Start date") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        legend.key.width = unit(1.5, 'cm'))

#End date
ggplot() +
  geom_sf(data = LRB, fill = "white") +
  geom_sf(data = flowline) +
  geom_sf(data = Flood_sf, aes(col = as.integer(End)), size = 4) +
  scale_color_gradient(low = "#48CAE4", high = "#030451",
                       labels = trans_date) +
  labs(color = "End date") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        legend.key.width = unit(1.5, 'cm'))

#Peak date
ggplot() +
  geom_sf(data = LRB, fill = "white") +
  geom_sf(data = flowline) +
  geom_sf(data = Flood_sf, aes(col = as.integer(Peak)), size = 4) +
  scale_color_gradient(low = "#48CAE4", high = "#030451",
                       labels = trans_date) +
  labs(color = "Peak date") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        legend.key.width = unit(1.5, 'cm'))

#Duration 
ggplot() +
  geom_sf(data = LRB, fill = "white") +
  geom_sf(data = flowline) +
  geom_sf(data = Flood_sf, aes(col = Duration), size = 4) +
  scale_color_gradient(low = "#48CAE4", high = "#030451") +
  labs(color = "Duration (days)") +
  MyTheme

#Increase rate
ggplot() +
  geom_sf(data = LRB, fill = "white") +
  geom_sf(data = flowline) +
  geom_sf(data = Flood_sf, aes(col = IncreasedPercent), size = 4) +
  scale_color_gradient(low = "#48CAE4", high = "#030451") +
  labs(color = "Increase rate") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        legend.key.width = unit(1, 'cm'))




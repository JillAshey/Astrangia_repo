# Title: Daily measurement plotting
# Author: Jill Ashey
# Last updated: 20210124

# This script is plotting the daily measurement data from JA's Astrangia project 
# DailyMeausrements.csv will be updated daily

# load packages
library(tidyverse)

# Set working directory 
setwd("~/Desktop/PutnamLab/Repositories/Astrangia_repo/Astrangia_repo/data")

# Load data 
daily <- read.csv("DailyMeasurements.csv", header = TRUE)
daily <- daily[-c(1:17),] # remove tank 14 because it cracked 
daily$Date <- as.Date(daily$Date, "%m/%d/%Y")
daily$Tank <- as.character(daily$Tank)


## Plots ##

# Temperature 
pdf("Temperature_DailyMeasurement.pdf")
ggplot(daily, aes(x=Date, y=Temperature)) +
  geom_line(aes(color = Tank))+
  ylab("Temperature °C")
dev.off()

# pH
pdf("pH_DailyMeasurement.pdf")
ggplot(daily, aes(x=Date, y=pH)) +
  geom_line(aes(color = Tank))+
  ylab("pH mV")
dev.off()

# Salinity
pdf("Salinity_DailyMeasurement.pdf")
ggplot(daily, aes(x=Date, y=Salinity)) +
  geom_line(aes(color = Tank))+
  ylab("Salinity psu")
dev.off()

# Light
light <- select(daily, c("Date", "Tank", "Light")) # subset light data, as it is not measured daily
light <- na.omit(light)
light$Tank <- as.character(light$Tank)

pdf("Light_DailyMeasurement.pdf")
ggplot(light, aes(x=Date, y=Light)) +
  geom_line(aes(color = Tank))+ 
  ylab("Light")
dev.off()
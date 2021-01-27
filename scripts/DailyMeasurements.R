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
daily$Tank <- as.factor(daily$Tank)

pdf("~/Desktop/PutnamLab/Repositories/Astrangia_repo/Astrangia_repo/Output/Daily_Measurements.pdf")
par(mfrow=c(2,2))
plot(daily$Tank, daily$Temperature, xlab="Tank", ylab="Temperature°C", ylim = c(7.5,9), las = 2)
plot(daily$Tank, daily$Salinity, xlab="Tank", ylab="Salinity psu", ylim = c(30,33.5), las = 2)
plot(daily$Tank, daily$pH, xlab="Tank", ylab="pH (mV)", ylim = c(-45,-55), las = 2)
plot(daily$Tank, daily$Light, xlab="Tank", ylab="Light", ylim=c(80,150),las=2)
dev.off()







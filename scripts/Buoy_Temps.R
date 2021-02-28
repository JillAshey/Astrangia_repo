#Title: Astrangia Field Temps
#Project: Astrangia CGP
#Author: HM Putnam 
#Edited by: HM Putnam
#Date Last Modified: 20210227
#See Readme file for details

library(lubridate)
library(tidyr)
library(plotrix)
library(tidyverse)
library(dplyr)
library(reshape2)
library(gridExtra)
library(FSA)


##### Load Buoy Data #####

Temp_2004 <- read.csv("data/NOAA_temp/nwpr1h2004.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,14)]
Temp_2004$Date <- paste0(Temp_2004$YYYY, sep = "-", Temp_2004$MM, sep = "-", Temp_2004$DD)
Temp_2004$Time <- Temp_2004$hh
Temp_2004$Date.Time <- paste0(Temp_2004$Date, sep=" ", Temp_2004$Time)
#Temp_2004[Temp_2004==999.00] <- NA
Temp_2004$Date.Time <- parse_date_time(Temp_2004$Date.Time, "%Y!-%m!-%d! %H!:%M!" , tz="EST")

Temp_2005 <- read.csv("data/NOAA_temp/nwpr1h2005.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2005$Date <- paste0(Temp_2005$MM, sep = "-", Temp_2005$DD)
Temp_2005$Time <- paste0(Temp_2005$hh, sep = ":", Temp_2005$mm)
Temp_2005$Date.Time <- paste0(Temp_2005$Date, sep=" ", Temp_2005$Time)
#Temp_2005[Temp_2005==999.00] <- NA
Temp_2005$Date.Time <- parse_date_time(Temp_2005$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")

Temp_2006 <- read.csv("data/NOAA_temp/nwpr1h2006.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2006$Date <- paste0(Temp_2006$MM, sep = "-", Temp_2006$DD)
Temp_2006$Time <- paste0(Temp_2006$hh, sep = ":", Temp_2006$mm)
Temp_2006$Date.Time <- paste0(Temp_2006$Date, sep=" ", Temp_2006$Time)
#Temp_2006[Temp_2006==999.00] <- NA
Temp_2006$Date.Time <- parse_date_time(Temp_2006$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")


pdf("Output/Newport_Buoy_Temps.pdf")
plot(Temp_2005$Date.Time, Temp_2005$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(Temp_2006$Date.Time, Temp_2006$WTMP, cex=0.1, col="orange")

points(Temp_2019$Date.Time, Temp_2019$WTMP, cex=0.1, col="black")
legend("topleft", legend=c("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
       col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), lty=1:2, cex=0.8)
dev.off()


Temp_2013 <- Temp_2013[21491:43272,]
Temp_2014 <- Temp_2014[21491:43272,]
Temp_2015 <- Temp_2015[21491:43272,]
Temp_2016 <- Temp_2016[21491:43272,]
Temp_2017 <- Temp_2017[21491:43272,]
Temp_2018 <- Temp_2018[21491:43272,]
Temp_2019 <- Temp_2019[21491:37419,]

pdf("Output/KbayTemps_Apr_Jun.pdf")
plot(Temp_2013$Date.Time, Temp_2013$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(22,31))
points(Temp_2014$Date.Time, Temp_2014$WTMP, cex=0.1, col="orange")
points(Temp_2015$Date.Time, Temp_2015$WTMP, cex=0.1, col="yellow")
points(Temp_2016$Date.Time, Temp_2016$WTMP, cex=0.1, col="green")
points(Temp_2017$Date.Time, Temp_2017$WTMP, cex=0.1, col="blue")
points(Temp_2018$Date.Time, Temp_2018$WTMP, cex=0.1, col="purple")
points(Temp_2019$Date.Time, Temp_2019$WTMP, cex=0.1, col="black")
legend("topleft", legend=c("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
       col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), lty=1:2, cex=0.8)
dev.off()
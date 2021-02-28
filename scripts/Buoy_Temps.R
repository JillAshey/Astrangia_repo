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

# Temp_2004 <- read.csv("data/NOAA_temp/nwpr1h2004.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,14)]
# Temp_2004$Date <- paste0(Temp_2004$YYYY, sep = "-", Temp_2004$MM, sep = "-", Temp_2004$DD)
# Temp_2004$Time <- Temp_2004$hh
# Temp_2004$Date.Time <- paste0(Temp_2004$Date, sep=" ", Temp_2004$Time)
# #Temp_2004[Temp_2004==999.00] <- NA
# Temp_2004$Date.Time <- parse_date_time(Temp_2004$Date.Time, "%Y!-%m!-%d! %H!:%M!" , tz="EST")

Temp_2005 <- read.csv("data/NOAA_temp/nwpr1h2005.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2005$Date <- paste0(Temp_2005$MM, sep = "-", Temp_2005$DD)
Temp_2005$Time <- paste0(Temp_2005$hh, sep = ":", Temp_2005$mm)
Temp_2005$Date.Time <- paste0(Temp_2005$Date, sep=" ", Temp_2005$Time)
#Temp_2005[Temp_2005==999.00] <- NA
Temp_2005$Date.Time <- parse_date_time(Temp_2005$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2005 <-Temp_2005[10200:11395, ]

Temp_2006 <- read.csv("data/NOAA_temp/nwpr1h2006.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2006$Date <- paste0(Temp_2006$MM, sep = "-", Temp_2006$DD)
Temp_2006$Time <- paste0(Temp_2006$hh, sep = ":", Temp_2006$mm)
Temp_2006$Date.Time <- paste0(Temp_2006$Date, sep=" ", Temp_2006$Time)
#Temp_2006[Temp_2006==999.00] <- NA
Temp_2006$Date.Time <- parse_date_time(Temp_2006$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2006 <-Temp_2006[13711:15385, ]

Temp_2007 <- read.csv("data/NOAA_temp/nwpr1h2007.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2007$Date <- paste0(Temp_2007$MM, sep = "-", Temp_2007$DD)
Temp_2007$Time <- paste0(Temp_2007$hh, sep = ":", Temp_2007$mm)
Temp_2007$Date.Time <- paste0(Temp_2007$Date, sep=" ", Temp_2007$Time)
#Temp_2007[Temp_2007==999.00] <- NA
Temp_2007$Date.Time <- parse_date_time(Temp_2007$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2005 <-Temp_2005[10200:11395, ]

Temp_2008 <- read.csv("data/NOAA_temp/nwpr1h2008.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2008$Date <- paste0(Temp_2008$MM, sep = "-", Temp_2008$DD)
Temp_2008$Time <- paste0(Temp_2008$hh, sep = ":", Temp_2008$mm)
Temp_2008$Date.Time <- paste0(Temp_2008$Date, sep=" ", Temp_2008$Time)
#Temp_2008[Temp_2008==999.00] <- NA
Temp_2008$Date.Time <- parse_date_time(Temp_2008$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2005 <-Temp_2005[10200:11395, ]

Temp_2009 <- read.csv("data/NOAA_temp/nwpr1h2009.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2009$Date <- paste0(Temp_2009$MM, sep = "-", Temp_2009$DD)
Temp_2009$Time <- paste0(Temp_2009$hh, sep = ":", Temp_2009$mm)
Temp_2009$Date.Time <- paste0(Temp_2009$Date, sep=" ", Temp_2009$Time)
#Temp_2009[Temp_2009==999.00] <- NA
Temp_2009$Date.Time <- parse_date_time(Temp_2009$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2005 <-Temp_2005[10200:11395, ]

Temp_2010 <- read.csv("data/NOAA_temp/nwpr1h2010.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2010$Date <- paste0(Temp_2010$MM, sep = "-", Temp_2010$DD)
Temp_2010$Time <- paste0(Temp_2010$hh, sep = ":", Temp_2010$mm)
Temp_2010$Date.Time <- paste0(Temp_2010$Date, sep=" ", Temp_2010$Time)
#Temp_2010[Temp_2010==999.00] <- NA
Temp_2010$Date.Time <- parse_date_time(Temp_2010$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2005 <-Temp_2005[10200:11395, ]

pdf("Output/Newport_Buoy_Temps.pdf")
plot(Temp_2005$Date.Time, Temp_2005$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(Temp_2006$Date.Time, Temp_2006$WTMP, cex=0.1, col="orange")
points(Temp_2007$Date.Time, Temp_2007$WTMP, cex=0.1, col="yellow")
points(Temp_2008$Date.Time, Temp_2008$WTMP, cex=0.1, col="green")
points(Temp_2009$Date.Time, Temp_2009$WTMP, cex=0.1, col="lightblue")
points(Temp_2010$Date.Time, Temp_2010$WTMP, cex=0.1, col="purple")
legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010"),
       col=c("red", "orange", "yellow", "green", "lightblue", "purple"), lty=1:2, cex=0.8)
dev.off()


pdf("Output/March_Newport_Buoy_Temps.pdf")
plot(March_Temp_2005$Date.Time, March_Temp_2005$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(0,7))
points(March_Temp_2006$Date.Time, March_Temp_2006$WTMP, cex=0.1, col="orange")
# points(Temp_2007$Date.Time, Temp_2007$WTMP, cex=0.1, col="yellow")
# points(Temp_2008$Date.Time, Temp_2008$WTMP, cex=0.1, col="green")
# points(Temp_2009$Date.Time, Temp_2009$WTMP, cex=0.1, col="lightblue")
# points(Temp_2010$Date.Time, Temp_2010$WTMP, cex=0.1, col="purple")
legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010"),
       col=c("red", "orange", "yellow", "green", "lightblue", "purple"), lty=1:2, cex=0.8)
dev.off()

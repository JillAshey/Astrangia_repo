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
library(naniar)


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
unique(Temp_2005$WTMP)
Temp_2005 <- Temp_2005 %>% naniar::replace_with_na(Temp_2005, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2005$WTMP)
Temp_2005$WTMP <- as.numeric(Temp_2005$WTMP)
range(Temp_2005$WTMP, na.rm=TRUE)
Temp_2005$Date.Time <- parse_date_time(Temp_2005$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2005 <-Temp_2005[10200:11395, ]

Temp_2006 <- read.csv("data/NOAA_temp/nwpr1h2006.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2006$Date <- paste0(Temp_2006$MM, sep = "-", Temp_2006$DD)
Temp_2006$Time <- paste0(Temp_2006$hh, sep = ":", Temp_2006$mm)
Temp_2006$Date.Time <- paste0(Temp_2006$Date, sep=" ", Temp_2006$Time)
unique(Temp_2006$WTMP)
Temp_2006 <- Temp_2006 %>% naniar::replace_with_na(Temp_2006, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2006$WTMP)
Temp_2006$WTMP <- as.numeric(Temp_2006$WTMP)
range(Temp_2006$WTMP, na.rm=TRUE)
Temp_2006$Date.Time <- parse_date_time(Temp_2006$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2006 <-Temp_2006[13711:15385, ]

Temp_2007 <- read.csv("data/NOAA_temp/nwpr1h2007.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2007$Date <- paste0(Temp_2007$MM, sep = "-", Temp_2007$DD)
Temp_2007$Time <- paste0(Temp_2007$hh, sep = ":", Temp_2007$mm)
Temp_2007$Date.Time <- paste0(Temp_2007$Date, sep=" ", Temp_2007$Time)
unique(Temp_2007$WTMP)
Temp_2007 <- Temp_2007 %>% naniar::replace_with_na(Temp_2007, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2007$WTMP)
Temp_2007$WTMP <- as.numeric(Temp_2007$WTMP)
range(Temp_2007$WTMP, na.rm=TRUE)
Temp_2007$Date.Time <- parse_date_time(Temp_2007$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2007 <-Temp_2007[13716:15181, ]

Temp_2008 <- read.csv("data/NOAA_temp/nwpr1h2008.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2008$Date <- paste0(Temp_2008$MM, sep = "-", Temp_2008$DD)
Temp_2008$Time <- paste0(Temp_2008$hh, sep = ":", Temp_2008$mm)
Temp_2008$Date.Time <- paste0(Temp_2008$Date, sep=" ", Temp_2008$Time)
unique(Temp_2008$WTMP)
Temp_2008 <- Temp_2008 %>% naniar::replace_with_na(Temp_2008, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2008$WTMP)
Temp_2008$WTMP <- as.numeric(Temp_2008$WTMP)
range(Temp_2008$WTMP, na.rm=TRUE)
Temp_2008$Date.Time <- parse_date_time(Temp_2008$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2008 <-Temp_2008[12154:13773, ]

Temp_2009 <- read.csv("data/NOAA_temp/nwpr1h2009.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2009$Date <- paste0(Temp_2009$MM, sep = "-", Temp_2009$DD)
Temp_2009$Time <- paste0(Temp_2009$hh, sep = ":", Temp_2009$mm)
Temp_2009$Date.Time <- paste0(Temp_2009$Date, sep=" ", Temp_2009$Time)
unique(Temp_2009$WTMP)
Temp_2009 <- Temp_2009 %>% naniar::replace_with_na(Temp_2009, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2009$WTMP)
Temp_2009$WTMP <- as.numeric(Temp_2009$WTMP)
range(Temp_2009$WTMP, na.rm=TRUE)
Temp_2009$Date.Time <- parse_date_time(Temp_2009$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2009 <-Temp_2009[14151:15829, ]

Temp_2010 <- read.csv("data/NOAA_temp/nwpr1h2010.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2010$Date <- paste0(Temp_2010$MM, sep = "-", Temp_2010$DD)
Temp_2010$Time <- paste0(Temp_2010$hh, sep = ":", Temp_2010$mm)
Temp_2010$Date.Time <- paste0(Temp_2010$Date, sep=" ", Temp_2010$Time)
unique(Temp_2010$WTMP)
Temp_2010 <- Temp_2010 %>% naniar::replace_with_na(Temp_2010, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2010$WTMP)
Temp_2010$WTMP <- as.numeric(Temp_2010$WTMP)
range(Temp_2010$WTMP, na.rm=TRUE)
Temp_2010$Date.Time <- parse_date_time(Temp_2010$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2010 <-Temp_2010[14040:15561, ]

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


pdf("Output/March_Week1_Newport_Buoy_Temps.pdf")
plot(March_Temp_2005$Date.Time, March_Temp_2005$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(0,7))
points(March_Temp_2006$Date.Time, March_Temp_2006$WTMP, cex=0.1, col="orange")
points(March_Temp_2007$Date.Time, March_Temp_2007$WTMP, cex=0.1, col="yellow")
points(March_Temp_2008$Date.Time, March_Temp_2008$WTMP, cex=0.1, col="green")
points(March_Temp_2009$Date.Time, March_Temp_2009$WTMP, cex=0.1, col="lightblue")
points(March_Temp_2010$Date.Time, March_Temp_2010$WTMP, cex=0.1, col="purple")
legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010"),
       col=c("red", "orange", "yellow", "green", "lightblue", "purple"), lty=1:2, cex=0.8)
dev.off()

# Running averages

colnames(Temp_2005)[1] <- "YYYY"
colnames(Temp_2006)[1] <- "YYYY"
colnames(Temp_2007)[1] <- "YYYY"
colnames(Temp_2008)[1] <- "YYYY"
colnames(Temp_2009)[1] <- "YYYY"
colnames(Temp_2010)[1] <- "YYYY"


data <- rbind(Temp_2005,Temp_2006,Temp_2007,Temp_2008,Temp_2009,Temp_2010)
range(data$WTMP, na.rm=TRUE)

data$Date <- as.Date(data$Date, "%m-%d")

daily.means <- data %>%
  group_by(Date) %>%
  summarise(avg = mean(as.numeric(WTMP))) 

plot(daily.means$Date, as.numeric(daily.means$avg), cex=0.1, col="red", xlab="Date", ylab="Temperature °C")




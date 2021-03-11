#Title: Astrangia Field Temps
#Project: Astrangia CGP
#Author: HM Putnam 
#Edited by: HM Putnam
#Date Last Modified: 20210227
#See Readme file for details

library(lubridate)
library(tidyr)
library(ggplot2)
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
weekly_2005 <- aggregate(x = Temp_2005$WTMP,                # Specify data column
                         by = list(Temp_2005$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

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
weekly_2006 <- aggregate(x = Temp_2006$WTMP,                # Specify data column
                         by = list(Temp_2006$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

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
weekly_2007 <- aggregate(x = Temp_2007$WTMP,                # Specify data column
                         by = list(Temp_2007$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

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
weekly_2008 <- aggregate(x = Temp_2008$WTMP,                # Specify data column
                         by = list(Temp_2008$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

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
weekly_2009 <- aggregate(x = Temp_2009$WTMP,                # Specify data column
                         by = list(Temp_2009$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

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
weekly_2010 <- aggregate(x = Temp_2010$WTMP,                # Specify data column
                         by = list(Temp_2010$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2011 <- read.csv("data/NOAA_temp/nwpr1h2011.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2011$Date <- paste0(Temp_2011$MM, sep = "-", Temp_2011$DD)
Temp_2011$Time <- paste0(Temp_2011$hh, sep = ":", Temp_2011$mm)
Temp_2011$Date.Time <- paste0(Temp_2011$Date, sep=" ", Temp_2011$Time)
unique(Temp_2011$WTMP)
Temp_2011 <- Temp_2011 %>% naniar::replace_with_na(Temp_2011, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2011$WTMP)
Temp_2011$WTMP <- as.numeric(Temp_2011$WTMP)
range(Temp_2011$WTMP, na.rm=TRUE)
Temp_2011$Date.Time <- parse_date_time(Temp_2011$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2011 <-Temp_2011[14140:15819, ]
weekly_2011 <- aggregate(x = Temp_2011$WTMP,                # Specify data column
                         by = list(Temp_2011$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2012 <- read.csv("data/NOAA_temp/nwpr1h2012.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2012$Date <- paste0(Temp_2012$MM, sep = "-", Temp_2012$DD)
Temp_2012$Time <- paste0(Temp_2012$hh, sep = ":", Temp_2012$mm)
Temp_2012$Date.Time <- paste0(Temp_2012$Date, sep=" ", Temp_2012$Time)
unique(Temp_2012$WTMP)
Temp_2012 <- Temp_2012 %>% naniar::replace_with_na(Temp_2012, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2012$WTMP)
Temp_2012$WTMP <- as.numeric(Temp_2012$WTMP)
range(Temp_2012$WTMP, na.rm=TRUE)
Temp_2012$Date.Time <- parse_date_time(Temp_2012$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
March_Temp_2012 <-Temp_2012[14363:16045, ]
weekly_2012 <- aggregate(x = Temp_2012$WTMP,                # Specify data column
                         by = list(Temp_2012$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2013 <- read.csv("data/NOAA_temp/nwpr1h2013.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2013$Date <- paste0(Temp_2013$MM, sep = "-", Temp_2013$DD)
Temp_2013$Time <- paste0(Temp_2013$hh, sep = ":", Temp_2013$mm)
Temp_2013$Date.Time <- paste0(Temp_2013$Date, sep=" ", Temp_2013$Time)
unique(Temp_2013$WTMP)
Temp_2013 <- Temp_2013 %>% naniar::replace_with_na(Temp_2013, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2013$WTMP)
Temp_2013$WTMP <- as.numeric(Temp_2013$WTMP)
range(Temp_2013$WTMP, na.rm=TRUE)
Temp_2013$Date.Time <- parse_date_time(Temp_2013$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#March_Temp_2013 <-Temp_2013[14040:15561, ]
weekly_2013 <- aggregate(x = Temp_2013$WTMP,                # Specify data column
                         by = list(Temp_2013$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2014 <- read.csv("data/NOAA_temp/nwpr1h2014.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2014$Date <- paste0(Temp_2014$MM, sep = "-", Temp_2014$DD)
Temp_2014$Time <- paste0(Temp_2014$hh, sep = ":", Temp_2014$mm)
Temp_2014$Date.Time <- paste0(Temp_2014$Date, sep=" ", Temp_2014$Time)
unique(Temp_2014$WTMP)
Temp_2014 <- Temp_2014 %>% naniar::replace_with_na(Temp_2014, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2014$WTMP)
Temp_2014$WTMP <- as.numeric(Temp_2014$WTMP)
range(Temp_2014$WTMP, na.rm=TRUE)
Temp_2014$Date.Time <- parse_date_time(Temp_2014$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#March_Temp_2014 <-Temp_2014[14040:15561, ]
weekly_2014 <- aggregate(x = Temp_2014$WTMP,                # Specify data column
                         by = list(Temp_2014$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2015 <- read.csv("data/NOAA_temp/nwpr1h2015.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2015$Date <- paste0(Temp_2015$MM, sep = "-", Temp_2015$DD)
Temp_2015$Time <- paste0(Temp_2015$hh, sep = ":", Temp_2015$mm)
Temp_2015$Date.Time <- paste0(Temp_2015$Date, sep=" ", Temp_2015$Time)
unique(Temp_2015$WTMP)
Temp_2015 <- Temp_2015 %>% naniar::replace_with_na(Temp_2015, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2015$WTMP)
Temp_2015$WTMP <- as.numeric(Temp_2015$WTMP)
range(Temp_2015$WTMP, na.rm=TRUE)
Temp_2015$Date.Time <- parse_date_time(Temp_2015$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#March_Temp_2015 <-Temp_2015[14040:15561, ]
weekly_2015 <- aggregate(x = Temp_2015$WTMP,                # Specify data column
                         by = list(Temp_2015$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2016 <- read.csv("data/NOAA_temp/nwpr1h2016.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2016$Date <- paste0(Temp_2016$MM, sep = "-", Temp_2016$DD)
Temp_2016$Time <- paste0(Temp_2016$hh, sep = ":", Temp_2016$mm)
Temp_2016$Date.Time <- paste0(Temp_2016$Date, sep=" ", Temp_2016$Time)
unique(Temp_2016$WTMP)
Temp_2016 <- Temp_2016 %>% naniar::replace_with_na(Temp_2016, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2016$WTMP)
Temp_2016$WTMP <- as.numeric(Temp_2016$WTMP)
range(Temp_2016$WTMP, na.rm=TRUE)
Temp_2016$Date.Time <- parse_date_time(Temp_2016$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#March_Temp_2016 <-Temp_2016[14040:15561, ]
weekly_2016 <- aggregate(x = Temp_2016$WTMP,                # Specify data column
                         by = list(Temp_2016$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2017 <- read.csv("data/NOAA_temp/nwpr1h2017.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2017$Date <- paste0(Temp_2017$MM, sep = "-", Temp_2017$DD)
Temp_2017$Time <- paste0(Temp_2017$hh, sep = ":", Temp_2017$mm)
Temp_2017$Date.Time <- paste0(Temp_2017$Date, sep=" ", Temp_2017$Time)
unique(Temp_2017$WTMP)
Temp_2017 <- Temp_2017 %>% naniar::replace_with_na(Temp_2017, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2017$WTMP)
Temp_2017$WTMP <- as.numeric(Temp_2017$WTMP)
range(Temp_2017$WTMP, na.rm=TRUE)
Temp_2017$Date.Time <- parse_date_time(Temp_2017$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#March_Temp_2017 <-Temp_2017[14040:15561, ]
weekly_2017 <- aggregate(x = Temp_2017$WTMP,                # Specify data column
                         by = list(Temp_2017$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2018 <- read.csv("data/NOAA_temp/nwpr1h2018.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2018$Date <- paste0(Temp_2018$MM, sep = "-", Temp_2018$DD)
Temp_2018$Time <- paste0(Temp_2018$hh, sep = ":", Temp_2018$mm)
Temp_2018$Date.Time <- paste0(Temp_2018$Date, sep=" ", Temp_2018$Time)
unique(Temp_2018$WTMP)
Temp_2018 <- Temp_2018 %>% naniar::replace_with_na(Temp_2018, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2018$WTMP)
Temp_2018$WTMP <- as.numeric(Temp_2018$WTMP)
range(Temp_2018$WTMP, na.rm=TRUE)
Temp_2018$Date.Time <- parse_date_time(Temp_2018$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#March_Temp_2018 <-Temp_2018[14040:15561, ]
weekly_2018 <- aggregate(x = Temp_2018$WTMP,                # Specify data column
                         by = list(Temp_2018$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2019 <- read.csv("data/NOAA_temp/nwpr1h2019.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
Temp_2019$Date <- paste0(Temp_2019$MM, sep = "-", Temp_2019$DD)
Temp_2019$Time <- paste0(Temp_2019$hh, sep = ":", Temp_2019$mm)
Temp_2019$Date.Time <- paste0(Temp_2019$Date, sep=" ", Temp_2019$Time)
unique(Temp_2019$WTMP)
Temp_2019 <- Temp_2019 %>% naniar::replace_with_na(Temp_2019, replace = list(WTMP=c("9   9.", ".0 999", "999", "0 999.")))
unique(Temp_2019$WTMP)
Temp_2019$WTMP <- as.numeric(Temp_2019$WTMP)
range(Temp_2019$WTMP, na.rm=TRUE)
Temp_2019$Date.Time <- parse_date_time(Temp_2019$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#March_Temp_2019 <-Temp_2019[14040:15561, ]
weekly_2019 <- aggregate(x = Temp_2019$WTMP,                # Specify data column
                         by = list(Temp_2019$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 


pdf("output/Newport_Buoy_Temps_20210309.pdf")
plot(Temp_2005$Date.Time, Temp_2005$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(Temp_2006$Date.Time, Temp_2006$WTMP, cex=0.1, col="orange")
points(Temp_2007$Date.Time, Temp_2007$WTMP, cex=0.1, col="yellow")
points(Temp_2008$Date.Time, Temp_2008$WTMP, cex=0.1, col="green")
points(Temp_2009$Date.Time, Temp_2009$WTMP, cex=0.1, col="lightblue")
points(Temp_2010$Date.Time, Temp_2010$WTMP, cex=0.1, col="purple")
points(Temp_2011$Date.Time, Temp_2011$WTMP, cex=0.1, col="pink")
points(Temp_2012$Date.Time, Temp_2012$WTMP, cex=0.1, col="gray")
points(Temp_2013$Date.Time, Temp_2013$WTMP, cex=0.1, col="brown")
points(Temp_2014$Date.Time, Temp_2014$WTMP, cex=0.1, col="salmon")
points(Temp_2015$Date.Time, Temp_2015$WTMP, cex=0.1, col="blueviolet")
points(Temp_2016$Date.Time, Temp_2016$WTMP, cex=0.1, col="palegreen")
points(Temp_2017$Date.Time, Temp_2017$WTMP, cex=0.1, col="cyan1")
points(Temp_2018$Date.Time, Temp_2018$WTMP, cex=0.1, col="darkorange3")
points(Temp_2019$Date.Time, Temp_2019$WTMP, cex=0.1, col="darkolivegreen1")
legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012"),
       col=c("red", "orange", "yellow", "green", "lightblue", "purple", "pink", "gray", "brown", "salmon", "blueviolet", "palegreen", "cyan1", "darkorange3", "darkolivegreen1"), lty=1:2, cex=0.8)
dev.off()

# pdf("Output/March_Week1_Newport_Buoy_Temps.pdf")
# plot(March_Temp_2005$Date.Time, March_Temp_2005$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(0,7))
# points(March_Temp_2006$Date.Time, March_Temp_2006$WTMP, cex=0.1, col="orange")
# points(March_Temp_2007$Date.Time, March_Temp_2007$WTMP, cex=0.1, col="yellow")
# points(March_Temp_2008$Date.Time, March_Temp_2008$WTMP, cex=0.1, col="green")
# points(March_Temp_2009$Date.Time, March_Temp_2009$WTMP, cex=0.1, col="lightblue")
# points(March_Temp_2010$Date.Time, March_Temp_2010$WTMP, cex=0.1, col="purple")
# points(March_Temp_2011$Date.Time, March_Temp_2011$WTMP, cex=0.1, col="pink")
# points(March_Temp_2012$Date.Time, March_Temp_2012$WTMP, cex=0.1, col="gray")
# legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012"),
#        col=c("red", "orange", "yellow", "green", "lightblue", "purple", "pink", "gray"), lty=1:2, cex=0.8)
# dev.off()

# Running averages

colnames(Temp_2005)[1] <- "YYYY"
colnames(Temp_2006)[1] <- "YYYY"
colnames(Temp_2007)[1] <- "YYYY"
colnames(Temp_2008)[1] <- "YYYY"
colnames(Temp_2009)[1] <- "YYYY"
colnames(Temp_2010)[1] <- "YYYY"
colnames(Temp_2011)[1] <- "YYYY"
colnames(Temp_2012)[1] <- "YYYY"
colnames(Temp_2013)[1] <- "YYYY"
colnames(Temp_2014)[1] <- "YYYY"
colnames(Temp_2015)[1] <- "YYYY"
colnames(Temp_2016)[1] <- "YYYY"
colnames(Temp_2017)[1] <- "YYYY"
colnames(Temp_2018)[1] <- "YYYY"
colnames(Temp_2019)[1] <- "YYYY"

data <- rbind(Temp_2005,Temp_2006,Temp_2007,Temp_2008,Temp_2009,Temp_2010,
              Temp_2011,Temp_2012,Temp_2013,Temp_2014,Temp_2015,Temp_2016,
              Temp_2017,Temp_2018,Temp_2019)
range(data$WTMP, na.rm=TRUE)
data.lm <- lm(WTMP~Date.Time, data)
summary(data.lm)

data %>% group_by(YYYY) %>%
  ggplot(., aes(x=Date, y=WTMP)) +
  geom_line(aes(color=YYYY)) + 
  xlab("Date")+
  ylab("Temperature °C")

data$Date <- as.Date(data$Date, "%m-%d")

daily.means <- data %>%
  group_by(Date) %>%
  summarise(avg = mean(as.numeric(WTMP))) 


## weekly summary

data.week <- rbind(weekly_2005, weekly_2006, weekly_2007, weekly_2008, weekly_2009, weekly_2010, 
                   weekly_2011, weekly_2012, weekly_2013, weekly_2014, weekly_2015, weekly_2016,
                   weekly_2017, weekly_2018, weekly_2019)
colnames(data.week) <- c("Week", "WTMP")
data.week.lm <- lm(WTMP~Week, data.week)
summary(data.week.lm)
write.csv(data.week, "data/NOAA_temp/NOAA_weekly.csv")

data.week.mean <- aggregate(x = data.week$WTMP,                # Specify data column
                         by = list(data.week$Week),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 
write.csv(data.week.mean, "data/NOAA_temp/NOAA_weekly.mean.csv")








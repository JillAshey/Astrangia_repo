#Title: Astrangia Field Temps
#Project: Astrangia CGP
#Author: HM Putnam 
#Edited by: HM Putnam; J Ashey
#Date Last Modified: 20210614
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
daily_2005 <- aggregate(x = Temp_2005$WTMP,                # Specify data column
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
daily_2006 <- aggregate(x = Temp_2006$WTMP,                # Specify data column
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
daily_2007 <- aggregate(x = Temp_2007$WTMP,                # Specify data column
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
daily_2008 <- aggregate(x = Temp_2008$WTMP,                # Specify data column
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
daily_2009 <- aggregate(x = Temp_2009$WTMP,                # Specify data column
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
daily_2010 <- aggregate(x = Temp_2010$WTMP,                # Specify data column
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
daily_2011 <- aggregate(x = Temp_2011$WTMP,                # Specify data column
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
daily_2012 <- aggregate(x = Temp_2012$WTMP,                # Specify data column
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
daily_2013 <- aggregate(x = Temp_2013$WTMP,                # Specify data column
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
daily_2014 <- aggregate(x = Temp_2014$WTMP,                # Specify data column
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
daily_2015 <- aggregate(x = Temp_2015$WTMP,                # Specify data column
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
daily_2016 <- aggregate(x = Temp_2016$WTMP,                # Specify data column
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
daily_2017 <- aggregate(x = Temp_2017$WTMP,                # Specify data column
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
daily_2018 <- aggregate(x = Temp_2018$WTMP,                # Specify data column
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
daily_2019 <- aggregate(x = Temp_2019$WTMP,                # Specify data column
                         by = list(Temp_2019$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 

Temp_2020 <- read.csv("data/NOAA_temp/NOAA_2020.csv", header=TRUE, na.strings = "NA")[,c(1:3)]
colnames(Temp_2020) <- c("Date", "Time", "WTMP.F")
Temp_2020 <- separate(Temp_2020, col = Date, into = c("MM", "DD", "YYYY"), sep = "/")
Temp_2020$YYYY<- "2020"
Temp_2020 <- separate(Temp_2020, col = Time, into = c("hh", "mm"), sep = ":")
Temp_2020$Date <- paste0(Temp_2020$MM, sep = "-", Temp_2020$DD)
Temp_2020$Time <- paste0(Temp_2020$hh, sep = ":", Temp_2020$mm)
Temp_2020$Date.Time <- paste0(Temp_2020$Date, sep=" ", Temp_2020$Time)
Temp_2020$WTMP.F <- as.numeric(Temp_2020$WTMP.F)
Temp_2020 <- Temp_2020 %>% 
  mutate(WTMP.C = (WTMP.F - 32)*(5/9))
unique(Temp_2020$WTMP.C)
Temp_2020$WTMP.C <- as.numeric(Temp_2020$WTMP.C)
range(Temp_2020$WTMP.C, na.rm=TRUE)
Temp_2020$Date.Time <- parse_date_time(Temp_2020$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
Temp_2020 <- select(Temp_2020, -WTMP.F)
col_order <-c("YYYY", "MM", "DD", "hh", "mm", "WTMP.C", "Date", "Time", "Date.Time")
Temp_2020 <- Temp_2020[,col_order]
colnames(Temp_2020)[6] <- "WTMP"
daily_2020 <- aggregate(x = Temp_2020$WTMP,                # Specify data column
                         by = list(Temp_2020$Date),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 


pdf("output/Newport_Buoy_Temps.test.pdf")
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
points(Temp_2020$Date.Time, Temp_2020$WTMP, cex=0.1, col="black")
legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
       col=c("red", "orange", "yellow", "green", "lightblue", "purple", "pink", "gray", "brown", "salmon", "blueviolet", "palegreen", "cyan1", "darkorange3", "darkolivegreen1", "black"), lty=1:2, cex=0.8)
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

## examine data from March 1st to Aug 15th in all years
Temp_2005_sub <- Temp_2005 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2006_sub <- Temp_2006 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2007_sub <- Temp_2007 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2008_sub <- Temp_2008 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2009_sub <- Temp_2009 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2010_sub <- Temp_2010 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2011_sub <- Temp_2011 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2012_sub <- Temp_2012 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2013_sub <- Temp_2013 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2014_sub <- Temp_2014 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2015_sub <- Temp_2015 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2016_sub <- Temp_2016 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2017_sub <- Temp_2017 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2018_sub <- Temp_2018 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2019_sub <- Temp_2019 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")
Temp_2020_sub <- Temp_2020 %>%
  filter(Date.Time > "0000-03-01 00:06:00" & Date.Time < "0000-8-15 23:54:00")










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
              Temp_2017,Temp_2018,Temp_2019, Temp_2020)
range(data$WTMP, na.rm=TRUE)
data.lm <- lm(WTMP~Date.Time, data)
summary(data.lm)

data %>% group_by(YYYY) %>%
  ggplot(., aes(x=Date, y=WTMP)) +
  geom_line(aes(color=YYYY)) + 
  xlab("Date")+
  ylab("Temperature °C")
daily.means <- data %>%
  group_by(Date) %>%
  summarise(avg = mean(as.numeric(WTMP))) 


## daily summary
data.daily <- rbind(daily_2005, daily_2006, daily_2007, daily_2008, daily_2009, daily_2010, 
                   daily_2011, daily_2012, daily_2013, daily_2014, daily_2015, daily_2016,
                   daily_2017, daily_2018, daily_2019, daily_2020)
colnames(data.daily) <- c("Day", "WTMP")
data.daily.lm <- lm(WTMP~Day, data.daily)
summary(data.daily.lm)
write.csv(data.daily, "data/NOAA_temp/NOAA_daily.csv")

data.daily.mean <- aggregate(x = data.daily$WTMP,                # Specify data column
                         by = list(data.daily$Day),          # Specify group indicator
                         FUN = mean, 
                         na.rm = T) 
colnames(data.daily.mean) <- c("Day", "WTMP")
write.csv(data.daily.mean, "data/NOAA_temp/NOAA_daily.mean.csv")

## Weekly summary 
n <- 7
data.weekly <- aggregate(data.daily.mean, list(rep(1:(nrow(data.daily.mean) %/% n + 1), each = n, len = nrow(data.daily.mean))), mean)[-1]
data.weekly$Week <- rep(1:53)
write.csv(data.weekly, "data/NOAA_temp/NOAA_weekly.mean.csv")









##### Plotting my own data with NOAA buoy data 
my.data <- read.csv("output/Hobo/temp_light_logger/All_Tank_HoboTempLight_data.csv", header = T)
colnames(my.data)[colnames(my.data) == 'Date'] <- 'Date.Time'
my.data <- my.data[,c("Date.Time", 
                      "Tank1_Temp", 
                      "Tank2_Temp", 
                      "Tank3_Temp",
                      "Tank4_Temp",
                      "Tank5_Temp", 
                      "Tank6_Temp", 
                      "Tank7_Temp",
                      "Tank8_Temp",
                      "Tank9_Temp",
                      "Tank10_Temp", 
                      "Tank11_Temp", 
                      "Tank12_Temp", 
                      "Tank15_Temp",
                      "Tank16_Temp", 
                      "Tank17_Temp", 
                      "Tank18_Temp")]

#my.data <- na.omit(my.data)
my.data$Date.Time <- parse_date_time(my.data$Date.Time, "ymdHMS", tz = "EST")
my.data$Date.Time <- my.data$Date.Time + hours(5)

my.data <- separate(my.data, col = Date.Time, into = c("Date", "Time"), sep = " ")
my.data <- separate(my.data, col = Date, into = c("YYYY", "MM", "DD"), sep = "-")
my.data <- separate(my.data, col = Time, into = c("hh", "mm", "ss"), sep = ":")
my.data$Date <- paste0(my.data$MM, sep = "-", my.data$DD)
my.data$Time <- paste0(my.data$hh, sep = ":", my.data$mm)
my.data$Date.Time <- paste0(my.data$Date, sep=" ", my.data$Time)





range(my.data$Tank1_Temp, na.rm=TRUE)
my.data$Date.Time <- parse_date_time(my.data$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
#my.data <- my.data %>%
#  filter(Date.Time <= "0000-02-27 19:50:00" | Date.Time >= "0000-02-28 20:20:00")
my.data$Temp_Amb <- rowMeans(my.data[,c("Tank1_Temp", "Tank2_Temp", "Tank5_Temp",
                                        "Tank8_Temp", "Tank10_Temp", "Tank12_Temp",
                                        "Tank16_Temp", "Tank18_Temp")], na.rm=TRUE)
my.data$Temp_Heat <- rowMeans(my.data[,c("Tank3_Temp", "Tank4_Temp", "Tank6_Temp",
                                        "Tank7_Temp", "Tank9_Temp", "Tank11_Temp",
                                        "Tank15_Temp", "Tank17_Temp")], na.rm=TRUE)


pdf("output/Newport_Buoy_Temps.MyData.pdf")
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
points(Temp_2020$Date.Time, Temp_2020$WTMP, cex=0.1, col="bisque4")
points(my.data$Date.Time, my.data$Temp_Amb, cex=0.25, col="dodgerblue")
points(my.data$Date.Time, my.data$Temp_Heat, cex=0.25, col="firebrick1")
legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
       col=c("red", "orange", "yellow", "green", "lightblue", "purple", "pink", "gray", "brown", "salmon", "blueviolet", "palegreen", "cyan1", "darkorange3", "darkolivegreen1", "bisque4"), lty=1:2, cex=0.8)
legend("topright", legend=c("Ambient", "Heat"),
       col=c("dodgerblue", "firebrick1"), lty=1, cex=0.8)
dev.off()

pdf("output/Newport_Buoy_Temps.2020.MyData.pdf")
plot(Temp_2020$Date.Time, Temp_2020$WTMP, cex=0.1, col="bisque4", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(my.data$Date.Time, my.data$Temp_Amb, cex=0.25, col="dodgerblue")
points(my.data$Date.Time, my.data$Temp_Heat, cex=0.25, col="firebrick1")
legend("topleft", legend=c("2020"),
       col=c("bisque4"), lty=1:2, cex=0.8)
legend("topright", legend=c("Ambient", "Heat"),
       col=c("dodgerblue", "firebrick1"), lty=1, cex=0.8)
dev.off()

pdf("output/Newport_Buoy_Temps.2019.MyData.pdf")
plot(Temp_2019$Date.Time, Temp_2019$WTMP, cex=0.1, col="bisque4", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(my.data$Date.Time, my.data$Temp_Amb, cex=0.25, col="dodgerblue")
points(my.data$Date.Time, my.data$Temp_Heat, cex=0.25, col="firebrick1")
legend("topleft", legend=c("2019"),
       col=c("bisque4"), lty=1:2, cex=0.8)
legend("topright", legend=c("Ambient", "Heat"),
       col=c("dodgerblue", "firebrick1"), lty=1, cex=0.8)
dev.off()


# Filtering NOAA by date from my data 
Temp_2005_sub <- Temp_2005 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2006_sub <- Temp_2006 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2007_sub <- Temp_2007 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2008_sub <- Temp_2008 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2009_sub <- Temp_2009 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2010_sub <- Temp_2010 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2011_sub <- Temp_2011 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2012_sub <- Temp_2012 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2013_sub <- Temp_2013 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2014_sub <- Temp_2014 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2015_sub <- Temp_2015 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2016_sub <- Temp_2016 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2017_sub <- Temp_2017 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2018_sub <- Temp_2018 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2019_sub <- Temp_2019 %>%
  filter(Date.Time %in% my.data$Date.Time)
Temp_2020_sub <- Temp_2020 %>%
  filter(Date.Time %in% my.data$Date.Time)

pdf("output/Newport_Buoy_Temps.MyData.Subset.pdf")
plot(Temp_2005_sub$Date.Time, Temp_2005_sub$WTMP, cex=0.1, col="red", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(Temp_2006_sub$Date.Time, Temp_2006_sub$WTMP, cex=0.1, col="orange")
points(Temp_2007_sub$Date.Time, Temp_2007_sub$WTMP, cex=0.1, col="yellow")
points(Temp_2008_sub$Date.Time, Temp_2008_sub$WTMP, cex=0.1, col="green")
points(Temp_2009_sub$Date.Time, Temp_2009_sub$WTMP, cex=0.1, col="lightblue")
points(Temp_2010_sub$Date.Time, Temp_2010_sub$WTMP, cex=0.1, col="purple")
points(Temp_2011_sub$Date.Time, Temp_2011_sub$WTMP, cex=0.1, col="pink")
points(Temp_2012_sub$Date.Time, Temp_2012_sub$WTMP, cex=0.1, col="gray")
points(Temp_2013_sub$Date.Time, Temp_2013_sub$WTMP, cex=0.1, col="brown")
points(Temp_2014_sub$Date.Time, Temp_2014_sub$WTMP, cex=0.1, col="salmon")
points(Temp_2015_sub$Date.Time, Temp_2015_sub$WTMP, cex=0.1, col="blueviolet")
points(Temp_2016_sub$Date.Time, Temp_2016_sub$WTMP, cex=0.1, col="palegreen")
points(Temp_2017_sub$Date.Time, Temp_2017_sub$WTMP, cex=0.1, col="cyan1")
points(Temp_2018_sub$Date.Time, Temp_2018_sub$WTMP, cex=0.1, col="darkorange3")
points(Temp_2019_sub$Date.Time, Temp_2019_sub$WTMP, cex=0.1, col="darkolivegreen1")
points(Temp_2020_sub$Date.Time, Temp_2020_sub$WTMP, cex=0.1, col="bisque4")
points(my.data$Date.Time, my.data$Temp_Amb, cex=0.25, col="dodgerblue")
points(my.data$Date.Time, my.data$Temp_Heat, cex=0.25, col="firebrick1")
legend("topleft", legend=c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
       col=c("red", "orange", "yellow", "green", "lightblue", "purple", "pink", "gray", "brown", "salmon", "blueviolet", "palegreen", "cyan1", "darkorange3", "darkolivegreen1", "bisque4"), lty=1:2, cex=0.8)
legend("topright", legend=c("Ambient", "Heat"),
       col=c("dodgerblue", "firebrick1"), lty=1, cex=0.8)
dev.off()


## Plotting with 2021 data 
# Newport 2021
# Link to real time Newport buoy data: https://www.ndbc.noaa.gov/station_realtime.php?station=nwpr1
nwpr2021 <- read.csv("data/NOAA_temp/nwpr2021.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
nwpr2021$Date <- paste0(nwpr2021$MM, sep = "-", nwpr2021$DD)
nwpr2021$Time <- paste0(nwpr2021$hh, sep = ":", nwpr2021$mm)
nwpr2021$Date.Time <- paste0(nwpr2021$Date, sep=" ", nwpr2021$Time)
unique(nwpr2021$WTMP)
nwpr2021 <- nwpr2021 %>% naniar::replace_with_na(nwpr2021, replace = list(WTMP=c("MM", "9   9.", ".0 999", "999", "0 999.")))
unique(nwpr2021$WTMP)
nwpr2021$WTMP <- as.numeric(nwpr2021$WTMP)
range(nwpr2021$WTMP, na.rm=TRUE)
nwpr2021$Date.Time <- parse_date_time(nwpr2021$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
daily_nwpr2021 <- aggregate(x = nwpr2021$WTMP,                # Specify data column
                        by = list(nwpr2021$Date),          # Specify group indicator
                        FUN = mean, 
                        na.rm = T) 

# Quonset Point 2021
# Link to real time Quonset buoy data: https://www.ndbc.noaa.gov/station_realtime.php?station=qptr1
qptr2021 <- read.csv("data/NOAA_temp/qptr2021.txt", sep="\t", skip=c(0), header=TRUE, na.strings = "NA")[,c(1:5,15)]
qptr2021$Date <- paste0(qptr2021$MM, sep = "-", qptr2021$DD)
qptr2021$Time <- paste0(qptr2021$hh, sep = ":", qptr2021$mm)
qptr2021$Date.Time <- paste0(qptr2021$Date, sep=" ", qptr2021$Time)
unique(qptr2021$WTMP)
qptr2021 <- qptr2021 %>% naniar::replace_with_na(qptr2021, replace = list(WTMP=c("MM", "9   9.", ".0 999", "999", "0 999.")))
unique(qptr2021$WTMP)
qptr2021$WTMP <- as.numeric(qptr2021$WTMP)
range(qptr2021$WTMP, na.rm=TRUE)
qptr2021$Date.Time <- parse_date_time(qptr2021$Date.Time, "%m!-%d! %H!:%M!" , tz="EST")
daily_qptr2021 <- aggregate(x = qptr2021$WTMP,                # Specify data column
                            by = list(qptr2021$Date),          # Specify group indicator
                            FUN = mean, 
                            na.rm = T) 

# Plot Newport and Quonset 2021 w/ my data 
pdf("output/NOAA.2021.MyData.pdf")
par(mfrow=c(1,2))
plot(nwpr2021$Date.Time, nwpr2021$WTMP, col="bisque4", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(my.data$Date.Time, my.data$Temp_Amb, cex=0.25, col="dodgerblue")
points(my.data$Date.Time, my.data$Temp_Heat, cex=0.25, col="firebrick1")
legend("topleft", legend=c("Newport 2021"),
       col=c("bisque4"), lty=1:2, cex=0.8)
legend("topright", legend=c("Ambient", "Heat"),
       col=c("dodgerblue", "firebrick1"), lty=1, cex=0.8)
plot(qptr2021$Date.Time, qptr2021$WTMP, col="green", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
points(my.data$Date.Time, my.data$Temp_Amb, cex=0.25, col="dodgerblue")
points(my.data$Date.Time, my.data$Temp_Heat, cex=0.25, col="firebrick1")
legend("topleft", legend=c("Quonset 2021"),
       col=c("green"), lty=1:2, cex=0.8)
legend("topright", legend=c("Ambient", "Heat"),
       col=c("dodgerblue", "firebrick1"), lty=1, cex=0.8)
dev.off()

# pdf("output/NOAA.2021.MyData.pdf")
# plot(nwpr2021$Date.Time, nwpr2021$WTMP, col="bisque4", xlab="Date", ylab="Temperature °C", ylim=c(0,25))
# points(qptr2021$Date.Time, qptr2021$WTMP, col = "green", cex=0.25)
# points(my.data$Date.Time, my.data$Temp_Amb, cex=0.25, col="dodgerblue")
# points(my.data$Date.Time, my.data$Temp_Heat, cex=0.25, col="firebrick1")
# legend("topleft", legend=c("Newport 2021", "Quonset 2021"),
#        col=c("bisque4", "green"), lty=1:2, cex=0.8)
# legend("topright", legend=c("Ambient", "Heat"),
#        col=c("dodgerblue", "firebrick1"), lty=1, cex=0.8)
# dev.off()


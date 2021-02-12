#  Title: NOAA buoy data for Newport, RI
# Author: Jill Ashey
# date: 1/18/2021

### Using historical data downloaded from here: https://www.ndbc.noaa.gov/station_history.php?station=nwpr1 
### Script adapted from D. Barshis and H. Aichelman: https://github.com/BarshisLab/AstrangiaPoculata-Thermal-Performance/blob/master/Figures/Figure2/Fig2A_NOAAvsHoboTemp.R 

library(tidyverse)
library(plyr)
library(lubridate)

# Read in Newport, RI buoy data from 2005
newport2005 <- read.delim("data/NOAA_temp/nwpr1h2005.txt", na.strings='999')
newport2005 <- cbind("datetime"=paste(paste(newport2005$YYYY, newport2005$MM, newport2005$DD, sep="-")," ",paste(newport2005$hh,newport2005$mm, sep=":"),sep=""),newport2005)
newport2005$datetime <- strptime(newport2005$datetime, format="%Y-%m-%d %H")
newport2005$WTMP[newport2005$WTMP==999] <- NA
newport2005 <- select(newport2005, c(datetime, YYYY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2005$MM[which(newport2005$MM=="1")] <- "Jan"
newport2005$MM[which(newport2005$MM=="2")] <- "Feb"
newport2005$MM[which(newport2005$MM=="3")] <- "Mar"
newport2005$MM[which(newport2005$MM=="4")] <- "Apr"
newport2005$MM[which(newport2005$MM=="5")] <- "May"
newport2005$MM[which(newport2005$MM=="6")] <- "Jun"
newport2005$MM[which(newport2005$MM=="7")] <- "Jul"
newport2005$MM[which(newport2005$MM=="8")] <- "Aug"
newport2005$MM[which(newport2005$MM=="9")] <- "Sept"
newport2005$MM[which(newport2005$MM=="10")] <- "Oct"
newport2005$MM[which(newport2005$MM=="11")] <- "Nov"
newport2005$MM[which(newport2005$MM=="12")] <- "Dec"
newport2005$WTMP <- as.numeric(newport2005$WTMP)
newport2005 <- na.omit(newport2005)
newport2005$week <- c(0, rep(1:(nrow(newport2005)-1)%/%1042))
# Monthly temp avgs
monthly2005 <- aggregate(x = newport2005$WTMP,                # Specify data column
                         by = list(newport2005$MM),          # Specify group indicator
                         FUN = mean) 
monthly2005$year <- "2005"
colnames(monthly2005) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2005 <- aggregate(x = newport2005$WTMP,                # Specify data column
                        by = list(newport2005$week),          # Specify group indicator
                        FUN = mean) 
weekly2005$year <- "2005"
colnames(weekly2005) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2006
newport2006 <- read.delim("data/NOAA_temp/nwpr1h2006.txt", na.strings='999')
newport2006 <- cbind("datetime"=paste(paste(newport2006$YYYY, newport2006$MM, newport2006$DD, sep="-")," ",paste(newport2006$hh,newport2006$mm, sep=":"),sep=""),newport2006)
newport2006$datetime <- strptime(newport2006$datetime, format="%Y-%m-%d %H")
newport2006$WTMP[newport2006$WTMP==999] <- NA
newport2006 <- select(newport2006, c(datetime, YYYY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2006$MM[which(newport2006$MM=="1")] <- "Jan"
newport2006$MM[which(newport2006$MM=="2")] <- "Feb"
newport2006$MM[which(newport2006$MM=="3")] <- "Mar"
newport2006$MM[which(newport2006$MM=="4")] <- "Apr"
newport2006$MM[which(newport2006$MM=="5")] <- "May"
newport2006$MM[which(newport2006$MM=="6")] <- "Jun"
newport2006$MM[which(newport2006$MM=="7")] <- "Jul"
newport2006$MM[which(newport2006$MM=="8")] <- "Aug"
newport2006$MM[which(newport2006$MM=="9")] <- "Sept"
newport2006$MM[which(newport2006$MM=="10")] <- "Oct"
newport2006$MM[which(newport2006$MM=="11")] <- "Nov"
newport2006$MM[which(newport2006$MM=="12")] <- "Dec"
newport2006$WTMP <- as.numeric(newport2006$WTMP)
newport2006 <- na.omit(newport2006)
newport2006$week <- c(0, rep(1:(nrow(newport2006)-1)%/%1582))
# Monthly temp avgs
monthly2006 <- aggregate(x = newport2006$WTMP,                # Specify data column
                         by = list(newport2006$MM),          # Specify group indicator
                         FUN = mean) 
monthly2006$year <- "2006"
colnames(monthly2006) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2006 <- aggregate(x = newport2006$WTMP,                # Specify data column
                         by = list(newport2006$week),          # Specify group indicator
                         FUN = mean) 
weekly2006$year <- "2006"
colnames(weekly2006) <- c("week", "temp", "year")

# pdf("output/2006_WTMP_RI_NOAA.pdf", height = 10, width = 30)
# plot(newport2006$datetime, newport2006$WTMP, type="l", ylab="Temperature (°C)", main="2006 Water Temp in Newport, RI", ylim=c(2,28), xaxt = "n", xlab='')
# ticks<-seq(from=newport2006$datetime[1], to=as.POSIXct("2006-12-31 23:54:00 EST"), by='1 week')
# lbl<-strftime(ticks, format="%U-%b-%y")
# axis(side=1, at=ticks, labels=F)
# text(ticks, par("usr")[3] - 1, srt=60, adj=1 , labels=lbl, xpd = T, cex=1)
# dev.off()

# Read in Newport, RI buoy data from 2007
newport2007 <- read.delim("data/NOAA_temp/nwpr1h2007.txt", na.strings='999')
newport2007 <- cbind("datetime"=paste(paste(newport2007$X.YY, newport2007$MM, newport2007$DD, sep="-")," ",paste(newport2007$hh,newport2007$mm, sep=":"),sep=""),newport2007)
newport2007$datetime <- strptime(newport2007$datetime, format="%Y-%m-%d %H")
newport2007$WTMP[newport2007$WTMP==999] <- NA
newport2007 <- select(newport2007, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2007$MM[which(newport2007$MM=="1")] <- "Jan"
newport2007$MM[which(newport2007$MM=="2")] <- "Feb"
newport2007$MM[which(newport2007$MM=="3")] <- "Mar"
newport2007$MM[which(newport2007$MM=="4")] <- "Apr"
newport2007$MM[which(newport2007$MM=="5")] <- "May"
newport2007$MM[which(newport2007$MM=="6")] <- "Jun"
newport2007$MM[which(newport2007$MM=="7")] <- "Jul"
newport2007$MM[which(newport2007$MM=="8")] <- "Aug"
newport2007$MM[which(newport2007$MM=="9")] <- "Sept"
newport2007$MM[which(newport2007$MM=="10")] <- "Oct"
newport2007$MM[which(newport2007$MM=="11")] <- "Nov"
newport2007$MM[which(newport2007$MM=="12")] <- "Dec"
newport2007$WTMP <- as.numeric(newport2007$WTMP)
newport2007 <- na.omit(newport2007)
newport2007$week <- c(0, rep(1:(nrow(newport2007)-1)%/%1582))
# Monthly temp avgs
monthly2007 <- aggregate(x = newport2007$WTMP,                # Specify data column
                         by = list(newport2007$MM),          # Specify group indicator
                         FUN = mean) 
monthly2007$year <- "2007"
colnames(monthly2007) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2007 <- aggregate(x = newport2007$WTMP,                # Specify data column
                        by = list(newport2007$week),          # Specify group indicator
                        FUN = mean) 
weekly2007$year <- "2007"
colnames(weekly2007) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2008
newport2008 <- read.delim("data/NOAA_temp/nwpr1h2008.txt", na.strings='999')
newport2008 <- cbind("datetime"=paste(paste(newport2008$X.YY, newport2008$MM, newport2008$DD, sep="-")," ",paste(newport2008$hh,newport2008$mm, sep=":"),sep=""),newport2008)
newport2008$datetime <- strptime(newport2008$datetime, format="%Y-%m-%d %H")
newport2008$WTMP[newport2008$WTMP==999] <- NA
newport2008 <- select(newport2008, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2008$MM[which(newport2008$MM=="1")] <- "Jan"
newport2008$MM[which(newport2008$MM=="2")] <- "Feb"
newport2008$MM[which(newport2008$MM=="3")] <- "Mar"
newport2008$MM[which(newport2008$MM=="4")] <- "Apr"
newport2008$MM[which(newport2008$MM=="5")] <- "May"
newport2008$MM[which(newport2008$MM=="6")] <- "Jun"
newport2008$MM[which(newport2008$MM=="7")] <- "Jul"
newport2008$MM[which(newport2008$MM=="8")] <- "Aug"
newport2008$MM[which(newport2008$MM=="9")] <- "Sept"
newport2008$MM[which(newport2008$MM=="10")] <- "Oct"
newport2008$MM[which(newport2008$MM=="11")] <- "Nov"
newport2008$MM[which(newport2008$MM=="12")] <- "Dec"
newport2008$WTMP <- as.numeric(newport2008$WTMP)
newport2008 <- na.omit(newport2008)
newport2008$week <- c(0, rep(1:(nrow(newport2008)-1)%/%1582))
# Monthly temp avgs
monthly2008 <- aggregate(x = newport2008$WTMP,                # Specify data column
                         by = list(newport2008$MM),          # Specify group indicator
                         FUN = mean) 
monthly2008$year <- "2008"
colnames(monthly2008) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2008 <- aggregate(x = newport2008$WTMP,                # Specify data column
                        by = list(newport2008$week),          # Specify group indicator
                        FUN = mean) 
weekly2008$year <- "2008"
colnames(weekly2008) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2009
newport2009 <- read.delim("data/NOAA_temp/nwpr1h2009.txt", na.strings='999')
newport2009 <- cbind("datetime"=paste(paste(newport2009$X.YY, newport2009$MM, newport2009$DD, sep="-")," ",paste(newport2009$hh,newport2009$mm, sep=":"),sep=""),newport2009)
newport2009$datetime <- strptime(newport2009$datetime, format="%Y-%m-%d %H")
newport2009$WTMP[newport2009$WTMP==999] <- NA
newport2009 <- select(newport2009, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2009$MM[which(newport2009$MM=="1")] <- "Jan"
newport2009$MM[which(newport2009$MM=="2")] <- "Feb"
newport2009$MM[which(newport2009$MM=="3")] <- "Mar"
newport2009$MM[which(newport2009$MM=="4")] <- "Apr"
newport2009$MM[which(newport2009$MM=="5")] <- "May"
newport2009$MM[which(newport2009$MM=="6")] <- "Jun"
newport2009$MM[which(newport2009$MM=="7")] <- "Jul"
newport2009$MM[which(newport2009$MM=="8")] <- "Aug"
newport2009$MM[which(newport2009$MM=="9")] <- "Sept"
newport2009$MM[which(newport2009$MM=="10")] <- "Oct"
newport2009$MM[which(newport2009$MM=="11")] <- "Nov"
newport2009$MM[which(newport2009$MM=="12")] <- "Dec"
newport2009$WTMP <- as.numeric(newport2009$WTMP)
newport2009 <- na.omit(newport2009)
newport2009$week <- c(0, rep(1:(nrow(newport2009)-1)%/%1582))
# Monthly temp avgs
monthly2009 <- aggregate(x = newport2009$WTMP,                # Specify data column
                         by = list(newport2009$MM),          # Specify group indicator
                         FUN = mean) 
monthly2009$year <- "2009"
colnames(monthly2009) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2009 <- aggregate(x = newport2009$WTMP,                # Specify data column
                        by = list(newport2009$week),          # Specify group indicator
                        FUN = mean) 
weekly2009$year <- "2009"
colnames(weekly2009) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2010
newport2010 <- read.delim("data/NOAA_temp/nwpr1h2010.txt", na.strings='999')
newport2010 <- cbind("datetime"=paste(paste(newport2010$X.YY, newport2010$MM, newport2010$DD, sep="-")," ",paste(newport2010$hh,newport2010$mm, sep=":"),sep=""),newport2010)
newport2010$datetime <- strptime(newport2010$datetime, format="%Y-%m-%d %H")
newport2010$WTMP[newport2010$WTMP==999] <- NA
newport2010 <- select(newport2010, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2010$MM[which(newport2010$MM=="1")] <- "Jan"
newport2010$MM[which(newport2010$MM=="2")] <- "Feb"
newport2010$MM[which(newport2010$MM=="3")] <- "Mar"
newport2010$MM[which(newport2010$MM=="4")] <- "Apr"
newport2010$MM[which(newport2010$MM=="5")] <- "May"
newport2010$MM[which(newport2010$MM=="6")] <- "Jun"
newport2010$MM[which(newport2010$MM=="7")] <- "Jul"
newport2010$MM[which(newport2010$MM=="8")] <- "Aug"
newport2010$MM[which(newport2010$MM=="9")] <- "Sept"
newport2010$MM[which(newport2010$MM=="10")] <- "Oct"
newport2010$MM[which(newport2010$MM=="11")] <- "Nov"
newport2010$MM[which(newport2010$MM=="12")] <- "Dec"
newport2010$WTMP <- as.numeric(newport2010$WTMP)
newport2010 <- na.omit(newport2010)
newport2010$week <- c(0, rep(1:(nrow(newport2010)-1)%/%1582))
# Monthly temp avgs
monthly2010 <- aggregate(x = newport2010$WTMP,                # Specify data column
                         by = list(newport2010$MM),          # Specify group indicator
                         FUN = mean) 
monthly2010$year <- "2010"
colnames(monthly2010) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2010 <- aggregate(x = newport2010$WTMP,                # Specify data column
                        by = list(newport2010$week),          # Specify group indicator
                        FUN = mean) 
weekly2010$year <- "2010"
colnames(weekly2010) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2011
newport2011 <- read.delim("data/NOAA_temp/nwpr1h2011.txt", na.strings='999')
newport2011 <- cbind("datetime"=paste(paste(newport2011$X.YY, newport2011$MM, newport2011$DD, sep="-")," ",paste(newport2011$hh,newport2011$mm, sep=":"),sep=""),newport2011)
newport2011$datetime <- strptime(newport2011$datetime, format="%Y-%m-%d %H")
newport2011$WTMP[newport2011$WTMP==999] <- NA
newport2011 <- select(newport2011, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2011$MM[which(newport2011$MM=="1")] <- "Jan"
newport2011$MM[which(newport2011$MM=="2")] <- "Feb"
newport2011$MM[which(newport2011$MM=="3")] <- "Mar"
newport2011$MM[which(newport2011$MM=="4")] <- "Apr"
newport2011$MM[which(newport2011$MM=="5")] <- "May"
newport2011$MM[which(newport2011$MM=="6")] <- "Jun"
newport2011$MM[which(newport2011$MM=="7")] <- "Jul"
newport2011$MM[which(newport2011$MM=="8")] <- "Aug"
newport2011$MM[which(newport2011$MM=="9")] <- "Sept"
newport2011$MM[which(newport2011$MM=="10")] <- "Oct"
newport2011$MM[which(newport2011$MM=="11")] <- "Nov"
newport2011$MM[which(newport2011$MM=="12")] <- "Dec"
newport2011$WTMP <- as.numeric(newport2011$WTMP)
newport2011 <- na.omit(newport2011)
newport2011$week <- c(0, rep(1:(nrow(newport2011)-1)%/%1582))
# Monthly temp avgs
monthly2011 <- aggregate(x = newport2011$WTMP,                # Specify data column
                         by = list(newport2011$MM),          # Specify group indicator
                         FUN = mean) 
monthly2011$year <- "2011"
colnames(monthly2011) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2011 <- aggregate(x = newport2011$WTMP,                # Specify data column
                        by = list(newport2011$week),          # Specify group indicator
                        FUN = mean) 
weekly2011$year <- "2011"
colnames(weekly2011) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2012
newport2012 <- read.delim("data/NOAA_temp/nwpr1h2012.txt", na.strings='999')
newport2012 <- cbind("datetime"=paste(paste(newport2012$X.YY, newport2012$MM, newport2012$DD, sep="-")," ",paste(newport2012$hh,newport2012$mm, sep=":"),sep=""),newport2012)
newport2012$datetime <- strptime(newport2012$datetime, format="%Y-%m-%d %H")
newport2012$WTMP[newport2012$WTMP==999] <- NA
newport2012 <- select(newport2012, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2012$MM[which(newport2012$MM=="1")] <- "Jan"
newport2012$MM[which(newport2012$MM=="2")] <- "Feb"
newport2012$MM[which(newport2012$MM=="3")] <- "Mar"
newport2012$MM[which(newport2012$MM=="4")] <- "Apr"
newport2012$MM[which(newport2012$MM=="5")] <- "May"
newport2012$MM[which(newport2012$MM=="6")] <- "Jun"
newport2012$MM[which(newport2012$MM=="7")] <- "Jul"
newport2012$MM[which(newport2012$MM=="8")] <- "Aug"
newport2012$MM[which(newport2012$MM=="9")] <- "Sept"
newport2012$MM[which(newport2012$MM=="10")] <- "Oct"
newport2012$MM[which(newport2012$MM=="11")] <- "Nov"
newport2012$MM[which(newport2012$MM=="12")] <- "Dec"
newport2012$WTMP <- as.numeric(newport2012$WTMP)
newport2012 <- na.omit(newport2012)
newport2012$week <- c(0, rep(1:(nrow(newport2012)-1)%/%1582))
# Monthly temp avgs
monthly2012 <- aggregate(x = newport2012$WTMP,                # Specify data column
                         by = list(newport2012$MM),          # Specify group indicator
                         FUN = mean) 
monthly2012$year <- "2012"
colnames(monthly2012) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2012 <- aggregate(x = newport2012$WTMP,                # Specify data column
                        by = list(newport2012$week),          # Specify group indicator
                        FUN = mean) 
weekly2012$year <- "2012"
colnames(weekly2012) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2013
newport2013 <- read.delim("data/NOAA_temp/nwpr1h2013.txt", na.strings='999')
newport2013 <- cbind("datetime"=paste(paste(newport2013$X.YY, newport2013$MM, newport2013$DD, sep="-")," ",paste(newport2013$hh,newport2013$mm, sep=":"),sep=""),newport2013)
newport2013$datetime <- strptime(newport2013$datetime, format="%Y-%m-%d %H")
newport2013$WTMP[newport2013$WTMP==999] <- NA
newport2013 <- select(newport2013, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2013$MM[which(newport2013$MM=="1")] <- "Jan"
newport2013$MM[which(newport2013$MM=="2")] <- "Feb"
newport2013$MM[which(newport2013$MM=="3")] <- "Mar"
newport2013$MM[which(newport2013$MM=="4")] <- "Apr"
newport2013$MM[which(newport2013$MM=="5")] <- "May"
newport2013$MM[which(newport2013$MM=="6")] <- "Jun"
newport2013$MM[which(newport2013$MM=="7")] <- "Jul"
newport2013$MM[which(newport2013$MM=="8")] <- "Aug"
newport2013$MM[which(newport2013$MM=="9")] <- "Sept"
newport2013$MM[which(newport2013$MM=="10")] <- "Oct"
newport2013$MM[which(newport2013$MM=="11")] <- "Nov"
newport2013$MM[which(newport2013$MM=="12")] <- "Dec"
newport2013$WTMP <- as.numeric(newport2013$WTMP)
newport2013 <- na.omit(newport2013)
newport2013$week <- c(0, rep(1:(nrow(newport2013)-1)%/%1582))
# Monthly temp avgs
monthly2013 <- aggregate(x = newport2013$WTMP,                # Specify data column
                         by = list(newport2013$MM),          # Specify group indicator
                         FUN = mean) 
monthly2013$year <- "2013"
colnames(monthly2013) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2013 <- aggregate(x = newport2013$WTMP,                # Specify data column
                        by = list(newport2013$week),          # Specify group indicator
                        FUN = mean) 
weekly2013$year <- "2013"
colnames(weekly2013) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2014
newport2014 <- read.delim("data/NOAA_temp/nwpr1h2014.txt", na.strings='999')
newport2014 <- cbind("datetime"=paste(paste(newport2014$X.YY, newport2014$MM, newport2014$DD, sep="-")," ",paste(newport2014$hh,newport2014$mm, sep=":"),sep=""),newport2014)
newport2014$datetime <- strptime(newport2014$datetime, format="%Y-%m-%d %H")
newport2014$WTMP[newport2014$WTMP==999] <- NA
newport2014 <- select(newport2014, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2014$MM[which(newport2014$MM=="1")] <- "Jan"
newport2014$MM[which(newport2014$MM=="2")] <- "Feb"
newport2014$MM[which(newport2014$MM=="3")] <- "Mar"
newport2014$MM[which(newport2014$MM=="4")] <- "Apr"
newport2014$MM[which(newport2014$MM=="5")] <- "May"
newport2014$MM[which(newport2014$MM=="6")] <- "Jun"
newport2014$MM[which(newport2014$MM=="7")] <- "Jul"
newport2014$MM[which(newport2014$MM=="8")] <- "Aug"
newport2014$MM[which(newport2014$MM=="9")] <- "Sept"
newport2014$MM[which(newport2014$MM=="10")] <- "Oct"
newport2014$MM[which(newport2014$MM=="11")] <- "Nov"
newport2014$MM[which(newport2014$MM=="12")] <- "Dec"
newport2014$WTMP <- as.numeric(newport2014$WTMP)
newport2014 <- na.omit(newport2014)
newport2014$week <- c(0, rep(1:(nrow(newport2014)-1)%/%1582))
# Monthly temp avgs
monthly2014 <- aggregate(x = newport2014$WTMP,                # Specify data column
                         by = list(newport2014$MM),          # Specify group indicator
                         FUN = mean) 
monthly2014$year <- "2014"
colnames(monthly2014) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2014 <- aggregate(x = newport2014$WTMP,                # Specify data column
                        by = list(newport2014$week),          # Specify group indicator
                        FUN = mean) 
weekly2014$year <- "2014"
colnames(weekly2014) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2015
newport2015 <- read.delim("data/NOAA_temp/nwpr1h2015.txt", na.strings='999')
newport2015 <- cbind("datetime"=paste(paste(newport2015$X.YY, newport2015$MM, newport2015$DD, sep="-")," ",paste(newport2015$hh,newport2015$mm, sep=":"),sep=""),newport2015)
newport2015$datetime <- strptime(newport2015$datetime, format="%Y-%m-%d %H")
newport2015$WTMP[newport2015$WTMP==999] <- NA
newport2015 <- select(newport2015, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2015$MM[which(newport2015$MM=="1")] <- "Jan"
newport2015$MM[which(newport2015$MM=="2")] <- "Feb"
newport2015$MM[which(newport2015$MM=="3")] <- "Mar"
newport2015$MM[which(newport2015$MM=="4")] <- "Apr"
newport2015$MM[which(newport2015$MM=="5")] <- "May"
newport2015$MM[which(newport2015$MM=="6")] <- "Jun"
newport2015$MM[which(newport2015$MM=="7")] <- "Jul"
newport2015$MM[which(newport2015$MM=="8")] <- "Aug"
newport2015$MM[which(newport2015$MM=="9")] <- "Sept"
newport2015$MM[which(newport2015$MM=="10")] <- "Oct"
newport2015$MM[which(newport2015$MM=="11")] <- "Nov"
newport2015$MM[which(newport2015$MM=="12")] <- "Dec"
newport2015$WTMP <- as.numeric(newport2015$WTMP)
newport2015 <- na.omit(newport2015)
newport2015$week <- c(0, rep(1:(nrow(newport2015)-1)%/%1582))
# Monthly temp avgs
monthly2015 <- aggregate(x = newport2015$WTMP,                # Specify data column
                         by = list(newport2015$MM),          # Specify group indicator
                         FUN = mean) 
monthly2015$year <- "2015"
colnames(monthly2015) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2015 <- aggregate(x = newport2015$WTMP,                # Specify data column
                        by = list(newport2015$week),          # Specify group indicator
                        FUN = mean) 
weekly2015$year <- "2015"
colnames(weekly2015) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2016
newport2016 <- read.delim("data/NOAA_temp/nwpr1h2016.txt", na.strings='999')
newport2016 <- cbind("datetime"=paste(paste(newport2016$X.YY, newport2016$MM, newport2016$DD, sep="-")," ",paste(newport2016$hh,newport2016$mm, sep=":"),sep=""),newport2016)
newport2016$datetime <- strptime(newport2016$datetime, format="%Y-%m-%d %H")
newport2016$WTMP[newport2016$WTMP==999] <- NA
newport2016 <- select(newport2016, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2016$MM[which(newport2016$MM=="1")] <- "Jan"
newport2016$MM[which(newport2016$MM=="2")] <- "Feb"
newport2016$MM[which(newport2016$MM=="3")] <- "Mar"
newport2016$MM[which(newport2016$MM=="4")] <- "Apr"
newport2016$MM[which(newport2016$MM=="5")] <- "May"
newport2016$MM[which(newport2016$MM=="6")] <- "Jun"
newport2016$MM[which(newport2016$MM=="7")] <- "Jul"
newport2016$MM[which(newport2016$MM=="8")] <- "Aug"
newport2016$MM[which(newport2016$MM=="9")] <- "Sept"
newport2016$MM[which(newport2016$MM=="10")] <- "Oct"
newport2016$MM[which(newport2016$MM=="11")] <- "Nov"
newport2016$MM[which(newport2016$MM=="12")] <- "Dec"
newport2016$WTMP <- as.numeric(newport2016$WTMP)
newport2016 <- na.omit(newport2016)
newport2016$week <- c(0, rep(1:(nrow(newport2016)-1)%/%1582))
# Monthly temp avgs
monthly2016 <- aggregate(x = newport2016$WTMP,                # Specify data column
                         by = list(newport2016$MM),          # Specify group indicator
                         FUN = mean) 
monthly2016$year <- "2016"
colnames(monthly2016) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2016 <- aggregate(x = newport2016$WTMP,                # Specify data column
                        by = list(newport2016$week),          # Specify group indicator
                        FUN = mean) 
weekly2016$year <- "2016"
colnames(weekly2016) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2017
newport2017 <- read.delim("data/NOAA_temp/nwpr1h2017.txt", na.strings='999')
newport2017 <- cbind("datetime"=paste(paste(newport2017$X.YY, newport2017$MM, newport2017$DD, sep="-")," ",paste(newport2017$hh,newport2017$mm, sep=":"),sep=""),newport2017)
newport2017$datetime <- strptime(newport2017$datetime, format="%Y-%m-%d %H")
newport2017$WTMP[newport2017$WTMP==999] <- NA
newport2017 <- select(newport2017, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2017$MM[which(newport2017$MM=="1")] <- "Jan"
newport2017$MM[which(newport2017$MM=="2")] <- "Feb"
newport2017$MM[which(newport2017$MM=="3")] <- "Mar"
newport2017$MM[which(newport2017$MM=="4")] <- "Apr"
newport2017$MM[which(newport2017$MM=="5")] <- "May"
newport2017$MM[which(newport2017$MM=="6")] <- "Jun"
newport2017$MM[which(newport2017$MM=="7")] <- "Jul"
newport2017$MM[which(newport2017$MM=="8")] <- "Aug"
newport2017$MM[which(newport2017$MM=="9")] <- "Sept"
newport2017$MM[which(newport2017$MM=="10")] <- "Oct"
newport2017$MM[which(newport2017$MM=="11")] <- "Nov"
newport2017$MM[which(newport2017$MM=="12")] <- "Dec"
newport2017$WTMP <- as.numeric(newport2017$WTMP)
newport2017 <- na.omit(newport2017)
newport2017$week <- c(0, rep(1:(nrow(newport2017)-1)%/%1582))
# Monthly temp avgs
monthly2017 <- aggregate(x = newport2017$WTMP,                # Specify data column
                         by = list(newport2017$MM),          # Specify group indicator
                         FUN = mean) 
monthly2017$year <- "2017"
colnames(monthly2017) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2017 <- aggregate(x = newport2017$WTMP,                # Specify data column
                        by = list(newport2017$week),          # Specify group indicator
                        FUN = mean) 
weekly2017$year <- "2017"
colnames(weekly2017) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2018
newport2018 <- read.delim("data/NOAA_temp/nwpr1h2018.txt", na.strings='999')
newport2018 <- cbind("datetime"=paste(paste(newport2018$X.YY, newport2018$MM, newport2018$DD, sep="-")," ",paste(newport2018$hh,newport2018$mm, sep=":"),sep=""),newport2018)
newport2018$datetime <- strptime(newport2018$datetime, format="%Y-%m-%d %H")
newport2018$WTMP[newport2018$WTMP==999] <- NA
newport2018 <- select(newport2018, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2018$MM[which(newport2018$MM=="1")] <- "Jan"
newport2018$MM[which(newport2018$MM=="2")] <- "Feb"
newport2018$MM[which(newport2018$MM=="3")] <- "Mar"
newport2018$MM[which(newport2018$MM=="4")] <- "Apr"
newport2018$MM[which(newport2018$MM=="5")] <- "May"
newport2018$MM[which(newport2018$MM=="6")] <- "Jun"
newport2018$MM[which(newport2018$MM=="7")] <- "Jul"
newport2018$MM[which(newport2018$MM=="8")] <- "Aug"
newport2018$MM[which(newport2018$MM=="9")] <- "Sept"
newport2018$MM[which(newport2018$MM=="10")] <- "Oct"
newport2018$MM[which(newport2018$MM=="11")] <- "Nov"
newport2018$MM[which(newport2018$MM=="12")] <- "Dec"
newport2018$WTMP <- as.numeric(newport2018$WTMP)
newport2018 <- na.omit(newport2018)
newport2018$week <- c(0, rep(1:(nrow(newport2018)-1)%/%1582))
# Monthly temp avgs
monthly2018 <- aggregate(x = newport2018$WTMP,                # Specify data column
                         by = list(newport2018$MM),          # Specify group indicator
                         FUN = mean) 
monthly2018$year <- "2018"
colnames(monthly2018) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2018 <- aggregate(x = newport2018$WTMP,                # Specify data column
                        by = list(newport2018$week),          # Specify group indicator
                        FUN = mean) 
weekly2018$year <- "2018"
colnames(weekly2018) <- c("week", "temp", "year")

# Read in Newport, RI buoy data from 2019
newport2019 <- read.delim("data/NOAA_temp/nwpr1h2019.txt", na.strings='999')
newport2019 <- cbind("datetime"=paste(paste(newport2019$X.YY, newport2019$MM, newport2019$DD, sep="-")," ",paste(newport2019$hh,newport2019$mm, sep=":"),sep=""),newport2019)
newport2019$datetime <- strptime(newport2019$datetime, format="%Y-%m-%d %H")
newport2019$WTMP[newport2019$WTMP==999] <- NA
newport2019 <- select(newport2019, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2019$MM[which(newport2019$MM=="1")] <- "Jan"
newport2019$MM[which(newport2019$MM=="2")] <- "Feb"
newport2019$MM[which(newport2019$MM=="3")] <- "Mar"
newport2019$MM[which(newport2019$MM=="4")] <- "Apr"
newport2019$MM[which(newport2019$MM=="5")] <- "May"
newport2019$MM[which(newport2019$MM=="6")] <- "Jun"
newport2019$MM[which(newport2019$MM=="7")] <- "Jul"
newport2019$MM[which(newport2019$MM=="8")] <- "Aug"
newport2019$MM[which(newport2019$MM=="9")] <- "Sept"
newport2019$MM[which(newport2019$MM=="10")] <- "Oct"
newport2019$MM[which(newport2019$MM=="11")] <- "Nov"
newport2019$MM[which(newport2019$MM=="12")] <- "Dec"
newport2019$WTMP <- as.numeric(newport2019$WTMP)
newport2019 <- na.omit(newport2019)
newport2019$week <- c(0, rep(1:(nrow(newport2019)-1)%/%1582))
# Monthly temp avgs
monthly2019 <- aggregate(x = newport2019$WTMP,                # Specify data column
                         by = list(newport2019$MM),          # Specify group indicator
                         FUN = mean) 
monthly2019$year <- "2019"
colnames(monthly2019) <- c("month", "temp", "year")
# Weekly temp avgs
weekly2019 <- aggregate(x = newport2019$WTMP,                # Specify data column
                        by = list(newport2019$week),          # Specify group indicator
                        FUN = mean) 
weekly2019$year <- "2019"
colnames(weekly2019) <- c("week", "temp", "year")


## Plot buoy data for 2019
#p = ggplot(data = newport1819, aes(x = datetime, y = WTMP)) + geom_line()
#p + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
#  theme(axis.text.x = element_text(angle = 45))
pdf("output/2019_WTMP_RI_NOAA.pdf")
plot(newport2019$datetime, newport2019$WTMP, type="l", ylab="Temperature (°C)", main="2016-2019 Water Temp in Newport, RI", ylim=c(-2,32), xaxt = "n", xlab='')
#lines(newport2018$datetime, newport2018$WTMP, col = "red")
#abline(h=8, col="blue")
#points(newport1819$datetime, newport1819$WTMP, type="l", col="grey")
ticks<-seq(from=newport2019$datetime[1], to=as.POSIXct("2019-12-31 23:54:00 EST"), by='1 month')
lbl<-strftime(ticks, format="%b-%y")
axis(side=1, at=ticks, labels=F)
text(ticks, par("usr")[3] - 1, srt=60, adj=1 , labels=lbl, xpd = T, cex=1)
#legend("topleft", c("2018", "2019"), lty=1, lwd=3, col=c("black", "grey"), )
dev.off()














#Plot NOAA and Hobo temp data over time considered at both VA and RI sites
#USE THIS ONE (includes CORRECTED VA loggers, updated x-axis labels and plot of Topt lines)
pdf("2005-2019_RI_NOAA.pdf")
plot(newport2005$datetime, newport2005$WTMP, type = "l", ylab="Temperature (°C)", main = "2005-2019_RI_NOAA", ylim=c(-5,30), xaxt='n', xlab='')
points(newport2006$datetime, newport2006$WTMP, type = "l", col = "grey")
points(newport2007$datetime, newport2007$WTMP, type = "l", col = "blue")
points(newport2008$datetime, newport2008$WTMP, type = "l", col = "green")
points(newport2009$datetime, newport2009$WTMP, type = "l", col = "orange")
points(newport2010$datetime, newport2010$WTMP, type = "l", col = "purple")
points(newport2011$datetime, newport2011$WTMP, type = "l", col = "pink")
points(newport2012$datetime, newport2012$WTMP, type = "l", col = "yellow")
points(newport2013$datetime, newport2013$WTMP, type = "l", col = "darkseagreen")
points(newport2014$datetime, newport2014$WTMP, type = "l", col = "cyan1")
points(newport2015$datetime, newport2015$WTMP, type = "l", col = "brown1")
points(newport2016$datetime, newport2016$WTMP, type = "l", col = "deepskyblue")
points(newport2017$datetime, newport2017$WTMP, type = "l", col = "blueviolet")
points(newport2018$datetime, newport2018$WTMP, type = "l", col = "palegreen")
points(newport2019$datetime, newport2019$WTMP, type = "l", col = "tomato")
#abline(h=23.04, col="blue", lwd=3) #RI-brown Topt average
#abline(h=21.35, col="blue", lwd=3, lty=2) #RI-white Topt average
#abline(h=26.81, col="red", lwd=3) #VA-brown Topt average
#abline(h=28.23, col="red", lwd=3, lty=2) #VA-white Topt average
ticks<-seq(from=newport2005$datetime[1], to=as.POSIXct("2019-12-31 23:54:00 EST"), by='1 month')
lbl<-strftime(ticks, format="%b")
axis(side=1, at=ticks, labels=F)
text(ticks, par("usr")[3] - 1, srt=60, adj=1 , labels=lbl, xpd = T, cex=1)
#legend("bottomright", c("NOAA-VA", "hobo-VA", "NOAA-RI", "hobo-RI"), lty=1, lwd=3, col=c("black",  "red", "grey", "blue"), )
dev.off()













































# Bind data together from 2016 - 2019
recent <- rbind(newport2016, newport2017, newport2018, newport2019)
recent_means <- aggregate(x = recent$WTMP,                # Specify data column
                          by = list(recent$MM),          # Specify group indicator
                          FUN = mean) 

means <- rbind(monthly2016,monthly2017, monthly2018, monthly2019)

## Plot buoy data for 2016 - 2019
#p = ggplot(data = newport1819, aes(x = datetime, y = WTMP)) + geom_line()
#p + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
#  theme(axis.text.x = element_text(angle = 45))
pdf("output/2016-2019_WTMP_RI_NOAA.pdf")
plot(recent$datetime, recent$WTMP, type="l", ylab="Temperature (°C)", main="2016-2019 Water Temp in Newport, RI", ylim=c(-2,32), xaxt = "n", xlab='')
#lines(newport2018$datetime, newport2018$WTMP, col = "red")
#abline(h=8, col="blue")
#points(newport1819$datetime, newport1819$WTMP, type="l", col="grey")
ticks<-seq(from=recent$datetime[1], to=as.POSIXct("2019-12-31 23:54:00 EST"), by='1 month')
lbl<-strftime(ticks, format="%b-%y")
axis(side=1, at=ticks, labels=F)
text(ticks, par("usr")[3] - 1, srt=60, adj=1 , labels=lbl, xpd = T, cex=1)
#legend("topleft", c("2018", "2019"), lty=1, lwd=3, col=c("black", "grey"), )
dev.off()





















# Bind data from all years together 
newport_all <- rbind(newport2004, newport2005, newport2006, newport2007, newport2008, newport2009, newport2010, 
                     newport2011, newport2012, newport2013, newport2014, newport2015, newport2016, newport2017, 
                     newport2018, newport2019)

## Plot buoy data for 2004 - 2019
#p = ggplot(data = newport1819, aes(x = datetime, y = WTMP)) + geom_line()
#p + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
#  theme(axis.text.x = element_text(angle = 45))
pdf("output/2004-2019_WTMP_RI_NOAA.pdf")
plot(newport_all$datetime, newport_all$WTMP, type="l", ylab="Temperature (°C)", main="2004-2019 Water Temp in Newport, RI", ylim=c(-2,32), xaxt = "n", xlab='')
#abline(h=8, col="blue")
#points(newport1819$datetime, newport1819$WTMP, type="l", col="grey")
ticks<-seq(from=newport_all$datetime[1], to=as.POSIXct("2019-12-31 23:54:00 EST"), by='1 month')
lbl<-strftime(ticks, format="%b-%y")
axis(side=1, at=ticks, labels=F)
text(ticks, par("usr")[3] - 1, srt=60, adj=1 , labels=lbl, xpd = T, cex=1)
#legend("topleft", c("2018", "2019"), lty=1, lwd=3, col=c("black", "grey"), )
dev.off()






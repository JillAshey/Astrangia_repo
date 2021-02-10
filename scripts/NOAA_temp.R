#  Title: NOAA buoy data for Newport, RI
# Author: Jill Ashey
# date: 1/18/2021

### Using historical data downloaded from here: https://www.ndbc.noaa.gov/station_history.php?station=nwpr1 
### Script adapted from D. Barshis and H. Aichelman: https://github.com/BarshisLab/AstrangiaPoculata-Thermal-Performance/blob/master/Figures/Figure2/Fig2A_NOAAvsHoboTemp.R 

library(tidyverse)

# Read in Newport, RI buoy data from 2004
newport2004 <- read.delim("data/NOAA_temp/nwpr1h2004.txt", na.strings='999')
newport2004 <- data.frame(append(newport2004, c(mm = 10), after = 4))
newport2004 <- cbind("datetime"=paste(paste(newport2004$YYYY, newport2004$MM, newport2004$DD, sep="-")," ",paste(newport2004$hh,newport2004$mm, sep=":"),sep=""),newport2004)
newport2004$datetime <- strptime(newport2004$datetime, format="%Y-%m-%d %H")
newport2004$WTMP[newport2004$WTMP==999] <- NA
newport2004 <- select(newport2004, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
# Replace month numbers with month abbreviations
newport2004$MM[which(newport2004$MM=="1")] <- "Jan"
newport2004$MM[which(newport2004$MM=="2")] <- "Feb"
newport2004$MM[which(newport2004$MM=="3")] <- "Mar"
newport2004$MM[which(newport2004$MM=="4")] <- "Apr"
newport2004$MM[which(newport2004$MM=="5")] <- "May"
newport2004$MM[which(newport2004$MM=="6")] <- "Jun"
newport2004$MM[which(newport2004$MM=="7")] <- "Jul"
newport2004$MM[which(newport2004$MM=="8")] <- "Aug"
newport2004$MM[which(newport2004$MM=="9")] <- "Sept"
newport2004$MM[which(newport2004$MM=="10")] <- "Oct"
newport2004$MM[which(newport2004$MM=="11")] <- "Nov"
newport2004$MM[which(newport2004$MM=="12")] <- "Dec"
newport2004 <- select(newport2004, c(datetime, MM, WTMP))
newport2004 <- na.omit(newport2004)

monthly2004 <- aggregate(x = newport2004$WTMP,                # Specify data column
          by = list(newport2004$MM),          # Specify group indicator
          FUN = mean) 

# Read in Newport, RI buoy data from 2005
newport2005 <- read.delim("data/NOAA_temp/nwpr1h2005.txt", na.strings='999')
newport2005 <- cbind("datetime"=paste(paste(newport2005$YYYY, newport2005$MM, newport2005$DD, sep="-")," ",paste(newport2005$hh,newport2005$mm, sep=":"),sep=""),newport2005)
newport2005$datetime <- strptime(newport2005$datetime, format="%Y-%m-%d %H")
newport2005$WTMP[newport2005$WTMP==999] <- NA
newport2005 <- select(newport2005, c(datetime, X.YY, MM, DD, hh, mm, WTMP))
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
newport2005 <- select(newport2005, c(datetime, MM, WTMP))
newport2005 <- na.omit(newport2005)

monthly2005 <- aggregate(x = newport2005$WTMP,                # Specify data column
                         by = list(newport2005$MM),          # Specify group indicator
                         FUN = mean) 









# Read in Newport, RI buoy data from 2006
newport2006 <- read.delim("data/NOAA_temp/nwpr1h2006.txt", na.strings='999')
newport2006 <- cbind("datetime"=paste(paste(newport2006$YYYY, newport2006$MM, newport2006$DD, sep="-")," ",paste(newport2006$hh,newport2006$mm, sep=":"),sep=""),newport2006)
newport2006$datetime <- strptime(newport2006$datetime, format="%Y-%m-%d %H")
newport2006$WTMP[newport2006$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2007
newport2007 <- read.delim("data/NOAA_temp/nwpr1h2007.txt", na.strings='999')
newport2007 <- cbind("datetime"=paste(paste(newport2007$X.YY, newport2007$MM, newport2007$DD, sep="-")," ",paste(newport2007$hh,newport2007$mm, sep=":"),sep=""),newport2007)
newport2007$datetime <- strptime(newport2007$datetime, format="%Y-%m-%d %H")
newport2007$WTMP[newport2007$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2008
newport2008 <- read.delim("data/NOAA_temp/nwpr1h2008.txt", na.strings='999')
newport2008 <- cbind("datetime"=paste(paste(newport2008$X.YY, newport2008$MM, newport2008$DD, sep="-")," ",paste(newport2008$hh,newport2008$mm, sep=":"),sep=""),newport2008)
newport2008$datetime <- strptime(newport2008$datetime, format="%Y-%m-%d %H")
newport2008$WTMP[newport2008$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2009
newport2009 <- read.delim("data/NOAA_temp/nwpr1h2009.txt", na.strings='999')
newport2009 <- cbind("datetime"=paste(paste(newport2009$X.YY, newport2009$MM, newport2009$DD, sep="-")," ",paste(newport2009$hh,newport2009$mm, sep=":"),sep=""),newport2009)
newport2009$datetime <- strptime(newport2009$datetime, format="%Y-%m-%d %H")
newport2009$WTMP[newport2009$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2010
newport2010 <- read.delim("data/NOAA_temp/nwpr1h2010.txt", na.strings='999')
newport2010 <- cbind("datetime"=paste(paste(newport2010$X.YY, newport2010$MM, newport2010$DD, sep="-")," ",paste(newport2010$hh,newport2010$mm, sep=":"),sep=""),newport2010)
newport2010$datetime <- strptime(newport2010$datetime, format="%Y-%m-%d %H")
newport2010$WTMP[newport2010$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2011
newport2011 <- read.delim("data/NOAA_temp/nwpr1h2011.txt", na.strings='999')
newport2011 <- cbind("datetime"=paste(paste(newport2011$X.YY, newport2011$MM, newport2011$DD, sep="-")," ",paste(newport2011$hh,newport2011$mm, sep=":"),sep=""),newport2011)
newport2011$datetime <- strptime(newport2011$datetime, format="%Y-%m-%d %H")
newport2011$WTMP[newport2011$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2012
newport2012 <- read.delim("data/NOAA_temp/nwpr1h2012.txt", na.strings='999')
newport2012 <- cbind("datetime"=paste(paste(newport2012$X.YY, newport2012$MM, newport2012$DD, sep="-")," ",paste(newport2012$hh,newport2012$mm, sep=":"),sep=""),newport2012)
newport2012$datetime <- strptime(newport2012$datetime, format="%Y-%m-%d %H")
newport2012$WTMP[newport2012$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2013
newport2013 <- read.delim("data/NOAA_temp/nwpr1h2013.txt", na.strings='999')
newport2013 <- cbind("datetime"=paste(paste(newport2013$X.YY, newport2013$MM, newport2013$DD, sep="-")," ",paste(newport2013$hh,newport2013$mm, sep=":"),sep=""),newport2013)
newport2013$datetime <- strptime(newport2013$datetime, format="%Y-%m-%d %H")
newport2013$WTMP[newport2013$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2014
newport2014 <- read.delim("data/NOAA_temp/nwpr1h2014.txt", na.strings='999')
newport2014 <- cbind("datetime"=paste(paste(newport2014$X.YY, newport2014$MM, newport2014$DD, sep="-")," ",paste(newport2014$hh,newport2014$mm, sep=":"),sep=""),newport2014)
newport2014$datetime <- strptime(newport2014$datetime, format="%Y-%m-%d %H")
newport2014$WTMP[newport2014$WTMP==999] <- NA


# Read in Newport, RI buoy data from 2015
newport2015 <- read.delim("data/NOAA_temp/nwpr1h2015.txt", na.strings='999')
newport2015 <- cbind("datetime"=paste(paste(newport2015$X.YY, newport2015$MM, newport2015$DD, sep="-")," ",paste(newport2015$hh,newport2015$mm, sep=":"),sep=""),newport2015)
newport2015$datetime <- strptime(newport2015$datetime, format="%Y-%m-%d %H")
newport2015$WTMP[newport2015$WTMP==999] <- NA

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
newport2016 <- na.omit(newport2016)

monthly2016 <- aggregate(x = newport2016$WTMP,                # Specify data column
                         by = list(newport2016$MM),          # Specify group indicator
                         FUN = mean)
monthly2016$year <- "2016"
colnames(monthly2016) <- c("month", "temp", "year")

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
newport2017 <- na.omit(newport2017)

monthly2017 <- aggregate(x = newport2017$WTMP,                # Specify data column
                         by = list(newport2017$MM),          # Specify group indicator
                         FUN = mean) 
monthly2017$year <- "2017"
colnames(monthly2017) <- c("month", "temp", "year")

# Read in Newport, RI buoy data from 2018
newport2018 <- read.delim("data/NOAA_temp/nwpr1h2018.txt", na.strings='999')
newport2018 <- cbind("datetime"=paste(paste(newport2018$X.YY, newport2018$MM, newport2018$DD, sep="-")," ",paste(newport2018$hh,newport2018$mm, sep=":"),sep=""),newport2018)
newport2018$datetime <- strptime(newport2018$datetime, format="%Y-%m-%d %H:%M")
#newport2018 <- newport2018[order(newport2018$datetime),]
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
newport2018$MM[which(newport2018$MM=="8")] <- "Aug" # no Aug data
newport2018$MM[which(newport2018$MM=="9")] <- "Sept"
newport2018$MM[which(newport2018$MM=="10")] <- "Oct"
newport2018$MM[which(newport2018$MM=="11")] <- "Nov"
newport2018$MM[which(newport2018$MM=="12")] <- "Dec"
newport2018 <- na.omit(newport2018)

monthly2018 <- aggregate(x = newport2018$WTMP,                # Specify data column
                         by = list(newport2018$MM),          # Specify group indicator
                         FUN = mean) 
monthly2018$year <- "2018"
colnames(monthly2018) <- c("month", "temp", "year")


# Read in Newport, RI buoy data from 2019
newport2019 <- read.delim("data/NOAA_temp/nwpr1h2019.txt", na.strings='999')
newport2019 <- cbind("datetime"=paste(paste(newport2019$X.YY, newport2019$MM, newport2019$DD, sep="-")," ",paste(newport2019$hh,newport2019$mm, sep=":"),sep=""),newport2019)
newport2019$datetime <- strptime(newport2019$datetime, format="%Y-%m-%d %H:%M")
#newport2019 <- newport2019[order(newport2019$datetime),]
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
newport2019 <- na.omit(newport2019)

monthly2019 <- aggregate(x = newport2019$WTMP,                # Specify data column
          by = list(newport2019$MM),          # Specify group indicator
          FUN = mean) 
monthly2019$year <- "2019"
colnames(monthly2019) <- c("month", "temp", "year")

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






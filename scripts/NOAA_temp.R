#  Title: NOAA buoy data for Newport, RI
# Author: Jill Ashey
# date: 1/18/2021

### Using historical data downloaded from here: https://www.ndbc.noaa.gov/station_history.php?station=nwpr1 
### Script adapted from D. Barshis and H. Aichelman: https://github.com/BarshisLab/AstrangiaPoculata-Thermal-Performance/blob/master/Figures/Figure2/Fig2A_NOAAvsHoboTemp.R 

# Read in Newport, RI buoy data from 2004
newport2004 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2004.txt", na.strings='999')
newport2004 <- data.frame(append(newport2004, c(mm = 10), after = 4))
newport2004 <- cbind("datetime"=paste(paste(newport2004$YYYY, newport2004$MM, newport2004$DD, sep="-")," ",paste(newport2004$hh,newport2004$mm, sep=":"),sep=""),newport2004)
newport2004$datetime <- strptime(newport2004$datetime, format="%Y-%m-%d %H")
newport2004$WTMP[newport2004$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2005
newport2005 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2005.txt", na.strings='999')
newport2005 <- cbind("datetime"=paste(paste(newport2005$YYYY, newport2005$MM, newport2005$DD, sep="-")," ",paste(newport2005$hh,newport2005$mm, sep=":"),sep=""),newport2005)
newport2005$datetime <- strptime(newport2005$datetime, format="%Y-%m-%d %H")
newport2005$WTMP[newport2005$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2006
newport2006 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2006.txt", na.strings='999')
newport2006 <- cbind("datetime"=paste(paste(newport2006$YYYY, newport2006$MM, newport2006$DD, sep="-")," ",paste(newport2006$hh,newport2006$mm, sep=":"),sep=""),newport2006)
newport2006$datetime <- strptime(newport2006$datetime, format="%Y-%m-%d %H")
newport2006$WTMP[newport2006$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2007
newport2007 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2007.txt", na.strings='999')
newport2007 <- cbind("datetime"=paste(paste(newport2007$X.YY, newport2007$MM, newport2007$DD, sep="-")," ",paste(newport2007$hh,newport2007$mm, sep=":"),sep=""),newport2007)
newport2007$datetime <- strptime(newport2007$datetime, format="%Y-%m-%d %H")
newport2007$WTMP[newport2007$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2008
newport2008 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2008.txt", na.strings='999')
newport2008 <- cbind("datetime"=paste(paste(newport2008$X.YY, newport2008$MM, newport2008$DD, sep="-")," ",paste(newport2008$hh,newport2008$mm, sep=":"),sep=""),newport2008)
newport2008$datetime <- strptime(newport2008$datetime, format="%Y-%m-%d %H")
newport2008$WTMP[newport2008$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2009
newport2009 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2009.txt", na.strings='999')
newport2009 <- cbind("datetime"=paste(paste(newport2009$X.YY, newport2009$MM, newport2009$DD, sep="-")," ",paste(newport2009$hh,newport2009$mm, sep=":"),sep=""),newport2009)
newport2009$datetime <- strptime(newport2009$datetime, format="%Y-%m-%d %H")
newport2009$WTMP[newport2009$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2010
newport2010 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2010.txt", na.strings='999')
newport2010 <- cbind("datetime"=paste(paste(newport2010$X.YY, newport2010$MM, newport2010$DD, sep="-")," ",paste(newport2010$hh,newport2010$mm, sep=":"),sep=""),newport2010)
newport2010$datetime <- strptime(newport2010$datetime, format="%Y-%m-%d %H")
newport2010$WTMP[newport2010$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2011
newport2011 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2011.txt", na.strings='999')
newport2011 <- cbind("datetime"=paste(paste(newport2011$X.YY, newport2011$MM, newport2011$DD, sep="-")," ",paste(newport2011$hh,newport2011$mm, sep=":"),sep=""),newport2011)
newport2011$datetime <- strptime(newport2011$datetime, format="%Y-%m-%d %H")
newport2011$WTMP[newport2011$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2012
newport2012 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2012.txt", na.strings='999')
newport2012 <- cbind("datetime"=paste(paste(newport2012$X.YY, newport2012$MM, newport2012$DD, sep="-")," ",paste(newport2012$hh,newport2012$mm, sep=":"),sep=""),newport2012)
newport2012$datetime <- strptime(newport2012$datetime, format="%Y-%m-%d %H")
newport2012$WTMP[newport2012$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2013
newport2013 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2013.txt", na.strings='999')
newport2013 <- cbind("datetime"=paste(paste(newport2013$X.YY, newport2013$MM, newport2013$DD, sep="-")," ",paste(newport2013$hh,newport2013$mm, sep=":"),sep=""),newport2013)
newport2013$datetime <- strptime(newport2013$datetime, format="%Y-%m-%d %H")
newport2013$WTMP[newport2013$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2014
newport2014 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2014.txt", na.strings='999')
newport2014 <- cbind("datetime"=paste(paste(newport2014$X.YY, newport2014$MM, newport2014$DD, sep="-")," ",paste(newport2014$hh,newport2014$mm, sep=":"),sep=""),newport2014)
newport2014$datetime <- strptime(newport2014$datetime, format="%Y-%m-%d %H")
newport2014$WTMP[newport2014$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2015
newport2015 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2015.txt", na.strings='999')
newport2015 <- cbind("datetime"=paste(paste(newport2015$X.YY, newport2015$MM, newport2015$DD, sep="-")," ",paste(newport2015$hh,newport2015$mm, sep=":"),sep=""),newport2015)
newport2015$datetime <- strptime(newport2015$datetime, format="%Y-%m-%d %H")
newport2015$WTMP[newport2015$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2016
newport2016 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2016.txt", na.strings='999')
newport2016 <- cbind("datetime"=paste(paste(newport2016$X.YY, newport2016$MM, newport2016$DD, sep="-")," ",paste(newport2016$hh,newport2016$mm, sep=":"),sep=""),newport2016)
newport2016$datetime <- strptime(newport2016$datetime, format="%Y-%m-%d %H")
newport2016$WTMP[newport2016$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2017
newport2017 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2017.txt", na.strings='999')
newport2017 <- cbind("datetime"=paste(paste(newport2017$X.YY, newport2017$MM, newport2017$DD, sep="-")," ",paste(newport2017$hh,newport2017$mm, sep=":"),sep=""),newport2017)
newport2017$datetime <- strptime(newport2017$datetime, format="%Y-%m-%d %H")
newport2017$WTMP[newport2017$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2018
newport2018 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2018.txt", na.strings='999')
newport2018 <- cbind("datetime"=paste(paste(newport2018$X.YY, newport2018$MM, newport2018$DD, sep="-")," ",paste(newport2018$hh,newport2018$mm, sep=":"),sep=""),newport2018)
newport2018$datetime <- strptime(newport2018$datetime, format="%Y-%m-%d %H:%M")
#newport2018 <- newport2018[order(newport2018$datetime),]
newport2018$WTMP[newport2018$WTMP==999] <- NA

# Read in Newport, RI buoy data from 2019
newport2019 <- read.delim("~/Desktop/PutnamLab/Astrangia/Temp/nwpr1h2019.txt", na.strings='999')
newport2019 <- cbind("datetime"=paste(paste(newport2019$X.YY, newport2019$MM, newport2019$DD, sep="-")," ",paste(newport2019$hh,newport2019$mm, sep=":"),sep=""),newport2019)
newport2019$datetime <- strptime(newport2019$datetime, format="%Y-%m-%d %H:%M")
#newport2019 <- newport2019[order(newport2019$datetime),]
newport2019$WTMP[newport2019$WTMP==999] <- NA

# Select datetime and water temp, and bind data from all years together 
newport2004 <- select(newport2004, c(datetime, WTMP))
newport2005 <- select(newport2005, c(datetime, WTMP))
newport2006 <- select(newport2006, c(datetime, WTMP))
newport2007 <- select(newport2007, c(datetime, WTMP))
newport2008 <- select(newport2008, c(datetime, WTMP))
newport2009 <- select(newport2009, c(datetime, WTMP))
newport2010 <- select(newport2010, c(datetime, WTMP))
newport2011 <- select(newport2011, c(datetime, WTMP))
newport2012 <- select(newport2012, c(datetime, WTMP))
newport2013 <- select(newport2013, c(datetime, WTMP))
newport2014 <- select(newport2014, c(datetime, WTMP))
newport2015 <- select(newport2015, c(datetime, WTMP))
newport2016 <- select(newport2016, c(datetime, WTMP))
newport2017 <- select(newport2017, c(datetime, WTMP))
newport2018 <- select(newport2018, c(datetime, WTMP))
newport2019 <- select(newport2019, c(datetime, WTMP))
newport_all <- rbind(newport2004, newport2005, newport2006, newport2007, newport2008, newport2009, newport2010, 
                     newport2011, newport2012, newport2013, newport2014, newport2015, newport2016, newport2017, 
                     newport2018, newport2019)

## Plot buoy data 
#p = ggplot(data = newport1819, aes(x = datetime, y = WTMP)) + geom_line()
#p + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
#  theme(axis.text.x = element_text(angle = 45))
pdf("~/Desktop/2004-2019_WTMP_RI_NOAA.pdf")
plot(newport_all$datetime, newport_all$WTMP, type="l", ylab="Temperature (°C)", main="2004-2019 Water Temp in Newport, RI", ylim=c(-2,32), xaxt = "n", xlab='')
abline(h=8, col="blue")
#points(newport1819$datetime, newport1819$WTMP, type="l", col="grey")
ticks<-seq(from=newport_all$datetime[1], to=as.POSIXct("2019-12-31 23:54:00 EST"), by='6 month')
lbl<-strftime(ticks, format="%b-%y")
axis(side=1, at=ticks, labels=F)
text(ticks, par("usr")[3] - 1, srt=60, adj=1 , labels=lbl, xpd = T, cex=1)
#legend("topleft", c("2018", "2019"), lty=1, lwd=3, col=c("black", "grey"), )
dev.off()







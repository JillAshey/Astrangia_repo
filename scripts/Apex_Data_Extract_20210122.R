# Title: Apex plotting - test (data from 20210111 - 20210112)
# Author: Jill Ashey
# date: 20200122

## Apex data from GSO astrangia experiment 
# Data Extracted from 20201226 - 20210122
# code modified from Sam Gurr

#http://www.informit.com/articles/article.aspx?p=2215520
#https://github.com/SamGurr/Intragenerational_thresholds_OA/blob/master/RAnalysis/Scripts/Apex_Data_Extract.R


# Load packages 
library("XML")
library("plyr")
library("tidyverse")

# Read in xml info
xmlfile <- xmlParse("http://131.128.103.229:80/cgi-bin/datalog.xml?sdate=201226&days=26") #read in the date plus x days of Apex data


Apex.Data <- ldply(xmlToList(xmlfile), data.frame) #convert xml to dataframe

# T_1 = probe.value
# T_2 = probe.name.58
# T_3 = probe.name.44
# T_4 = probe.name.46
# T_5 = probe.name.50
# T_6 = probe.name.48
# T_7 = probe.name.54
# T_8 = probe.name.52
# T_9 = probe.name.64
# T_10 = probe.name.60
# T_11 = probe.name.68
# T_12 = probe.name.66
# T_13 = ????
# T_14 = probe.name.62
# T_15 = probe.name.56
# T_16 = probe.name.38
# T_17 = probe.name.42
# T_18 = probe.name.40

#keep columnes with data of interest. This needs to be changed as it will be specific to the Apex configuration
Apex.Data2 <- Apex.Data %>% select(c("date",
                                        "probe.name", "probe.value", # T_1
                                        "probe.name.58", "probe.value.58", # T_2
                                        "probe.name.44", "probe.value.44", # T_3
                                        "probe.name.46", "probe.value.46", # T_4
                                        "probe.name.50", "probe.value.50", # T_5
                                        "probe.name.48", "probe.value.48", # T_6
                                        "probe.name.54", "probe.value.54", # T_7
                                        "probe.name.52", "probe.value.52", # T_8
                                        "probe.name.64", "probe.value.64", # T_9
                                        "probe.name.60", "probe.value.60", # T_10
                                        "probe.name.68", "probe.value.68", # T_11
                                        "probe.name.66", "probe.value.66", #T_12
                                        ### T_13 missing
                                        "probe.name.62", "probe.value.62", # T_14
                                        "probe.name.56", "probe.value.56", # T_15
                                        "probe.name.38", "probe.value.38", # T_16
                                        "probe.name.42", "probe.value.42",# T_17
                                        "probe.name.40", "probe.value.40")) # T_18
Probe.Data <- na.omit(Apex.Data2) # remove NAs
colnames(Probe.Data) <- c("Date.Time", # rename columns
                          "Probe1", "Temp1", 
                          "Probe2", "Temp2",
                          "Probe3", "Temp3",
                          "Probe4", "Temp4",
                          "Probe5", "Temp5",
                          "Probe6", "Temp6",
                          "Probe7", "Temp7",
                          "Probe8", "Temp8",
                          "Probe9", "Temp9",
                          "Probe10", "Temp10",
                          "Probe11", "Temp11",
                          "Probe12", "Temp12",
                          "Probe14", "Temp14",
                          "Probe15", "Temp15",
                          "Probe16", "Temp16",
                          "Probe17", "Temp17",
                          "Probe18", "Temp18")

# Adjust DateTime
Probe.Data$Date.Time <- as.POSIXct(Probe.Data$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz="EST") #convert date to HI time

# CHANGE DATE FOR NEW CSV (risk overwritting previous)
write.csv(Probe.Data, "~/Desktop/PutnamLab/Repositories/Astrangia_repo/Astrangia_repo/data/Apex/20210122_Apex_Data_Output.data.csv") #write file to save data

# Plot Temp and save to output
# CHANGE DATE FOR NEW PDF (risk overwritting previous)
pdf("~/Desktop/PutnamLab/Repositories/Astrangia_repo/Astrangia_repo/output/Apex/20210122_Apex_Plot_Output.data.pdf")
#par(mfrow=c(2,1))
plot(as.numeric(as.character(Temp1)) ~ Date.Time, Probe.Data, col = "grey", lwd = 2, type="l", ylim=c(6,16),  xlab="Time", ylab="Temperature °C")
lines(as.numeric(as.character(Temp2)) ~ Date.Time, Probe.Data, col = "red", lwd = 2)
lines(as.numeric(as.character(Temp3)) ~ Date.Time, Probe.Data, col = "blue", lwd = 2)
lines(as.numeric(as.character(Temp4)) ~ Date.Time, Probe.Data, col = "black", lwd = 2)
lines(as.numeric(as.character(Temp5)) ~ Date.Time, Probe.Data, col = "green", lwd = 2)
lines(as.numeric(as.character(Temp6)) ~ Date.Time, Probe.Data, col = "pink", lwd = 2)
lines(as.numeric(as.character(Temp7)) ~ Date.Time, Probe.Data, col = "orange", lwd = 2)
lines(as.numeric(as.character(Temp8)) ~ Date.Time, Probe.Data, col = "yellow", lwd = 2)
lines(as.numeric(as.character(Temp9)) ~ Date.Time, Probe.Data, col = "coral1", lwd = 2)
lines(as.numeric(as.character(Temp10)) ~ Date.Time, Probe.Data, col = "darkblue", lwd = 2)
lines(as.numeric(as.character(Temp11)) ~ Date.Time, Probe.Data, col = "firebrick", lwd = 2)
lines(as.numeric(as.character(Temp12)) ~ Date.Time, Probe.Data, col = "lightcyan2", lwd = 2)
lines(as.numeric(as.character(Temp14)) ~ Date.Time, Probe.Data, col = "purple", lwd = 2)
lines(as.numeric(as.character(Temp15)) ~ Date.Time, Probe.Data, col = "lightsalmon", lwd = 2)
lines(as.numeric(as.character(Temp16)) ~ Date.Time, Probe.Data, col = "mediumpurple", lwd = 2)
lines(as.numeric(as.character(Temp17)) ~ Date.Time, Probe.Data, col = "mistyrose", lwd = 2)
lines(as.numeric(as.character(Temp18)) ~ Date.Time, Probe.Data, col = "orange3", lwd = 2)
axis.POSIXct(side=1, Probe.Data$Date.Time)
legend("top", legend = c("T_1", "T_2", "T_3", "T_4", "T_5", "T_6", "T_7", "T_8", "T_9", "T_10", "T_11", "T_12", "T_14", "T_15", "T_16", "T_17", "T_18"),
       col = c("grey", "red", "blue", "black", "green", "pink", "orange", "yellow", "coral1", "darkblue", "firebrick", "lightcyan2", "purple", "lightsalmon", "mediumpurple", "mistyrose", "orange3"),
       lty = 1, cex = 0.8, lwd = 2, ncol = 3)
dev.off()

## Zoom in on data between 5.5 - 10 C
# Plot Temp and save to output
# CHANGE DATE FOR NEW PDF (risk overwritting previous)
pdf("~/Desktop/PutnamLab/Repositories/Astrangia_repo/Astrangia_repo/output/Apex/20210122_Apex_Plot_SubsetDegree_Output.data.pdf")
#par(mfrow=c(2,1))
plot(as.numeric(as.character(Temp1)) ~ Date.Time, Probe.Data, col = "grey", lwd = 2, type="l", ylim=c(5.5,10),  xlab="Time", ylab="Temperature °C")
lines(as.numeric(as.character(Temp2)) ~ Date.Time, Probe.Data, col = "red", lwd = 2)
lines(as.numeric(as.character(Temp3)) ~ Date.Time, Probe.Data, col = "blue", lwd = 2)
lines(as.numeric(as.character(Temp4)) ~ Date.Time, Probe.Data, col = "black", lwd = 2)
lines(as.numeric(as.character(Temp5)) ~ Date.Time, Probe.Data, col = "green", lwd = 2)
lines(as.numeric(as.character(Temp6)) ~ Date.Time, Probe.Data, col = "pink", lwd = 2)
lines(as.numeric(as.character(Temp7)) ~ Date.Time, Probe.Data, col = "orange", lwd = 2)
lines(as.numeric(as.character(Temp8)) ~ Date.Time, Probe.Data, col = "yellow", lwd = 2)
lines(as.numeric(as.character(Temp9)) ~ Date.Time, Probe.Data, col = "coral1", lwd = 2)
lines(as.numeric(as.character(Temp10)) ~ Date.Time, Probe.Data, col = "darkblue", lwd = 2)
lines(as.numeric(as.character(Temp11)) ~ Date.Time, Probe.Data, col = "firebrick", lwd = 2)
lines(as.numeric(as.character(Temp12)) ~ Date.Time, Probe.Data, col = "lightcyan2", lwd = 2)
lines(as.numeric(as.character(Temp14)) ~ Date.Time, Probe.Data, col = "purple", lwd = 2)
lines(as.numeric(as.character(Temp15)) ~ Date.Time, Probe.Data, col = "lightsalmon", lwd = 2)
lines(as.numeric(as.character(Temp16)) ~ Date.Time, Probe.Data, col = "mediumpurple", lwd = 2)
lines(as.numeric(as.character(Temp17)) ~ Date.Time, Probe.Data, col = "mistyrose", lwd = 2)
lines(as.numeric(as.character(Temp18)) ~ Date.Time, Probe.Data, col = "orange3", lwd = 2)
axis.POSIXct(side=1, Probe.Data$Date.Time)
legend("bottom", legend = c("T_1", "T_2", "T_3", "T_4", "T_5", "T_6", "T_7", "T_8", "T_9", "T_10", "T_11", "T_12", "T_14", "T_15", "T_16", "T_17", "T_18"),
       col = c("grey", "red", "blue", "black", "green", "pink", "orange", "yellow", "coral1", "darkblue", "firebrick", "lightcyan2", "purple", "lightsalmon", "mediumpurple", "mistyrose", "orange3"),
       lty = 1, cex = 0.8, lwd = 2, ncol = 3)
dev.off()





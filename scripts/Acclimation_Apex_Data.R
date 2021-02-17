# Title: Apex plotting - acclimation
# Author: Jill Ashey
# date: 1/12/2021

## Apex data from GSO astrangia experiment 
# data from Acclimation Period (20201223 - 20210206)
# code modified from Sam Gurr

#http://www.informit.com/articles/article.aspx?p=2215520

# Load packages 
library("XML")
library("plyr")
library("tidyverse")


## Read in data from 20201223 to 20210107
# Read in xml info
xmlfile1 <- xmlParse("http://131.128.103.229:80/cgi-bin/datalog.xml?sdate=201224&days=14") #read in the start date plus x days of Apex data

Apex.Data1 <- ldply(xmlToList(xmlfile1), data.frame) #convert xml to dataframe

tail(Apex.Data1)

# CHECK PROBE NAMES EACH TIME
# T_1 = probe.name
# T_2 = probe.name.58
# T_3 = probe.name.44
# T_4 = probe.name.46
# T_5 = probe.name.50
# T_6 = probe.name.48
# T_7 = probe.name.54
# T_8 = probe.name.52
# T_9 = probe.name.64
# T_10 = probe.name.60
# T_11 = probe.name.69
# T_12 = probe.name.67
# T_14 = probe.name.62 
# T_15 = probe.name.56
# T_16 = probe.name.38
# T_17 = probe.name.42
# T_18 = probe.name.40 -- will not be plotting tank 18, as it holds the corals that ES and EC collected from FW. 

#keep columnes with data of interest. This needs to be changed as it will be specific to the Apex configuration
Apex.Data1_keep <- Apex.Data1 %>% select(c("date",
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
                                     "probe.name.69", "probe.value.69", # T_11
                                     "probe.name.67", "probe.value.67", #T_12
                                     "probe.name.62", "probe.value.62", # T_14
                                     "probe.name.56", "probe.value.56", # T_15
                                     "probe.name.38", "probe.value.38", # T_16
                                     "probe.name.42", "probe.value.42")) # T_17
Probe.Data1 <- na.omit(Apex.Data1_keep) # remove NAs
colnames(Probe.Data1) <- c("Date.Time", # rename columns
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
                          "Probe17", "Temp17")

# Adjust DateTime
Probe.Data1$Date.Time <- as.POSIXct(Probe.Data1$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz="EST") #convert date to est time

# Tank 14 cracked on 1/6/21 when standpipe was being taken out. Corals from tank 14 were moved into tank 18 and tank 18 corals were moved into outside tank
# Tank 18 is used in all analyses going forward. For simplicity, the 'Probe 14' and 'Temp14' columns in Prove.Data1 will be moved to the end of df and renamed as 'Probe18' and 'Temp18'
names(Probe.Data1)[names(Probe.Data1) == "Probe14"] <- "Probe18"
names(Probe.Data1)[names(Probe.Data1) == "Temp14"] <- "Temp18"
col_order <- c("Date.Time", # rename columns
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
                  "Probe15", "Temp15",
                  "Probe16", "Temp16",
                  "Probe17", "Temp17",
                  "Probe18", "Temp18")
Probe.Data1 <- Probe.Data1[, col_order]

# T_14 in column will also be replaced with T_18
Probe.Data1$Probe18 <- gsub("14", "18", Probe.Data1$Probe18)



## Read in data from 20210108 - 20210118
# Read in xml info

xmlfile2 <- xmlParse("http://131.128.103.229:80/cgi-bin/datalog.xml?sdate=210108&days=10") #read in the date plus x days of Apex data

Apex.Data2 <- ldply(xmlToList(xmlfile2), data.frame) #convert xml to dataframe

tail(Apex.Data2)

# CHECK PROBE NAMES EACH TIME
# T_1 = probe.name
# T_2 = probe.name.58
# T_3 = probe.name.44
# T_4 = probe.name.46
# T_5 = probe.name.50
# T_6 = probe.name.48
# T_7 = probe.name.54
# T_8 = probe.name.52
# T_9 = probe.name.64
# T_10 = probe.name.60
# T_11 = probe.name.69
# T_12 = probe.name.67
# T_14 = probe.name.62 -- will not be plotting tank 14. On 1/6/21, tank 14 cracked and tank 14 corals were moved into tank 18. 
# T_15 = probe.name.56
# T_16 = probe.name.38
# T_17 = probe.name.42
# T_18 = probe.name.40

#keep columnes with data of interest. This needs to be changed as it will be specific to the Apex configuration
Apex.Data2_keep <- Apex.Data2 %>% select(c("date",
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
                                           "probe.name.69", "probe.value.69", # T_11
                                           "probe.name.67", "probe.value.67", #T_12
                                           "probe.name.56", "probe.value.56", # T_15
                                           "probe.name.38", "probe.value.38", # T_16
                                           "probe.name.42", "probe.value.42", # t_17
                                           "probe.name.40", "probe.value.40")) # T_18

Probe.Data2 <- na.omit(Apex.Data2_keep) # remove NAs
colnames(Probe.Data2) <- c("Date.Time", # rename columns
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
                          "Probe15", "Temp15",
                          "Probe16", "Temp16",
                          "Probe17", "Temp17",
                          "Probe18", "Temp18")

# Adjust DateTime
Probe.Data2$Date.Time <- as.POSIXct(Probe.Data2$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz="EST") #convert date to est time



## Read in data from 20210119 - 20210118
# Read in xml info

xmlfile3 <- xmlParse("http://131.128.103.229:80/cgi-bin/datalog.xml?sdate=210118&days=19") #read in the date plus x days of Apex data

Apex.Data3 <- ldply(xmlToList(xmlfile3), data.frame) #convert xml to dataframe

tail(Apex.Data3)

# CHECK PROBE NAMES EACH TIME
# T_1 = probe.name
# T_2 = probe.name.58
# T_3 = probe.name.44
# T_4 = probe.name.46
# T_5 = probe.name.50
# T_6 = probe.name.48
# T_7 = probe.name.54
# T_8 = probe.name.52
# T_9 = probe.name.64
# T_10 = probe.name.60
# T_11 = probe.name.69
# T_12 = probe.name.67
# T_14 = probe.name.62 -- will not be plotting tank 14. On 1/6/21, tank 14 cracked and tank 14 corals were moved into tank 18. 
# T_15 = probe.name.56
# T_16 = probe.name.38
# T_17 = probe.name.42
# T_18 = probe.name.40

#keep columnes with data of interest. This needs to be changed as it will be specific to the Apex configuration
Apex.Data3_keep <- Apex.Data3 %>% select(c("date",
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
                                           "probe.name.69", "probe.value.69", # T_11
                                           "probe.name.67", "probe.value.67", #T_12
                                           "probe.name.56", "probe.value.56", # T_15
                                           "probe.name.38", "probe.value.38", # T_16
                                           "probe.name.42", "probe.value.42", # t_17
                                           "probe.name.40", "probe.value.40")) # T_18

Probe.Data3 <- na.omit(Apex.Data3_keep) # remove NAs
colnames(Probe.Data3) <- c("Date.Time", # rename columns
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
                           "Probe15", "Temp15",
                           "Probe16", "Temp16",
                           "Probe17", "Temp17",
                           "Probe18", "Temp18")

# Adjust DateTime
Probe.Data3$Date.Time <- as.POSIXct(Probe.Data3$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz="EST") #convert date to est time

# Bind data together 
all_Probe.Data <- bind_rows(Probe.Data1, Probe.Data2, Probe.Data3)

# CHANGE DATE FOR NEW CSV (risk overwritting previous)
write.csv(all_Probe.Data, "data/Apex/Acclimation_Apex_Data_Output.data.csv") #write file to save data

# Plot Temp and save to output
# CHANGE DATE FOR NEW PDF (risk overwritting previous)
pdf("output/Apex/Acclimation_Apex_Plot_Output.data.pdf")
#par(mfrow=c(2,1))
plot(as.numeric(as.character(Temp1)) ~ Date.Time, all_Probe.Data, col = "grey", lwd = 2, type="l", ylim=c(4,10),  xlab="Time", ylab="Temperature °C")
lines(as.numeric(as.character(Temp2)) ~ Date.Time, all_Probe.Data, col = "red", lwd = 2)
lines(as.numeric(as.character(Temp3)) ~ Date.Time, all_Probe.Data, col = "blue", lwd = 2)
lines(as.numeric(as.character(Temp4)) ~ Date.Time, all_Probe.Data, col = "black", lwd = 2)
lines(as.numeric(as.character(Temp5)) ~ Date.Time, all_Probe.Data, col = "green", lwd = 2)
lines(as.numeric(as.character(Temp6)) ~ Date.Time, all_Probe.Data, col = "pink", lwd = 2)
lines(as.numeric(as.character(Temp7)) ~ Date.Time, all_Probe.Data, col = "orange", lwd = 2)
lines(as.numeric(as.character(Temp8)) ~ Date.Time, all_Probe.Data, col = "yellow", lwd = 2)
lines(as.numeric(as.character(Temp9)) ~ Date.Time, all_Probe.Data, col = "coral1", lwd = 2)
lines(as.numeric(as.character(Temp10)) ~ Date.Time, all_Probe.Data, col = "darkblue", lwd = 2)
lines(as.numeric(as.character(Temp11)) ~ Date.Time, all_Probe.Data, col = "firebrick", lwd = 2)
lines(as.numeric(as.character(Temp12)) ~ Date.Time, all_Probe.Data, col = "lightcyan2", lwd = 2)
lines(as.numeric(as.character(Temp15)) ~ Date.Time, all_Probe.Data, col = "lightsalmon", lwd = 2)
lines(as.numeric(as.character(Temp16)) ~ Date.Time, all_Probe.Data, col = "mediumpurple", lwd = 2)
lines(as.numeric(as.character(Temp17)) ~ Date.Time, all_Probe.Data, col = "mistyrose", lwd = 2)
lines(as.numeric(as.character(Temp18)) ~ Date.Time, all_Probe.Data, col = "orange3", lwd = 2)
axis.POSIXct(side=1, all_Probe.Data$Date.Time)
legend("bottom", legend = c("T_1", "T_2", "T_3", "T_4", "T_5", "T_6", "T_7", "T_8", "T_9", "T_10", "T_11", "T_12", "T_15", "T_16", "T_17", "T_18"),
       col = c("grey", "red", "blue", "black", "green", "pink", "orange", "yellow", "coral1", "darkblue", "firebrick", "lightcyan2", "lightsalmon", "mediumpurple", "mistyrose", "orange3"),
       lty = 1, cex = 0.8, lwd = 2, ncol = 3)
dev.off()
# need to make cut off for tank cleaning 


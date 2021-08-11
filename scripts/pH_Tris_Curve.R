# Title: pH Tris curve 
# Author: Jill Ashey
# date: 20210126


# modified for personal Lenovo PC at PT Whitney summer 2019; modified by SJG
#setwd("~/Desktop/PutnamLab/Repositories/Astrangia_repo/Astrangia_repo/data/pH_Tris/") #set working directory

Calib.Data <-read.table("data/pH_Tris/20210810.csv", header=TRUE, sep=",", na.string="NA", as.is=TRUE) #reads in the data files
model <-lm(mVTris ~ TTris, data=Calib.Data) #runs a linear regression of mV as a function of temperature
coe <- coef(model) #extracts the coeffecients
R2<-summary(model)$r.squared

pdf("output/pH_Tris/20210810.pdf") 
plot(mVTris ~ TTris, data=Calib.Data)
abline(lm(mVTris ~ TTris, data=Calib.Data))
legend('topleft', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off() 
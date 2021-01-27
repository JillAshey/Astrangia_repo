# Title: Light position plotting
# Author: Jill Ashey
# Last updated: 20210126



# Load packages
library(tidyverse)

# Set working directory 
setwd("~/Desktop/PutnamLab/Repositories/Astrangia_repo/Astrangia_repo/data")

# Load data 
light <- read.csv("Light_Data.csv", header = TRUE)

# Plot PAR 
plot(PAR ~ Tank, data = light, col = Position)

tank.model <- lm(PAR ~ Tank,data=light)
summary(tank.model)
# Call:
#   lm(formula = PAR ~ Tank, data = light)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -49.272 -10.992   1.042  11.393  49.796 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 107.1362     4.4166  24.258   <2e-16 ***
#   Tank         -0.4915     0.4230  -1.162    0.249    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 20.02 on 78 degrees of freedom
# Multiple R-squared:  0.01701,	Adjusted R-squared:  0.004411 
# F-statistic:  1.35 on 1 and 78 DF,  p-value: 0.2488

position.model <- lm(PAR ~ Position, data=light)
summary(position.model)
# Call:
#   lm(formula = PAR ~ Position, data = light)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -52.712 -10.156  -0.019   9.925  50.288 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   85.631      4.844  17.678  < 2e-16 ***
#   Position       5.694      1.461   3.898 0.000204 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 18.47 on 78 degrees of freedom
# Multiple R-squared:  0.1631,	Adjusted R-squared:  0.1523 
# F-statistic:  15.2 on 1 and 78 DF,  p-value: 0.0002036


# Plot light by position for each tank
pdf("../Output/Light.by.Position.pdf")
par(mfrow=c(3,4))
Tank1 <- subset(light, Tank==1)
plot(PAR ~ Position, data=Tank1, col=Position, ylim=c(0,160), main = "Tank1")

Tank2 <- subset(light, Tank==2)
plot(PAR ~ Position, data=Tank2, col=Position, ylim=c(0,160), main = "Tank2")

Tank3 <- subset(light, Tank==3)
plot(PAR ~ Position, data=Tank3, col=Position, ylim=c(0,160), main = "Tank3")

Tank4 <- subset(light, Tank==4)
plot(PAR ~ Position, data=Tank4, col=Position, ylim=c(0,160), main = "Tank4")

Tank5 <- subset(light, Tank==5)
plot(PAR ~ Position, data=Tank5, col=Position, ylim=c(0,160), main = "Tank5")

Tank6 <- subset(light, Tank==6)
plot(PAR ~ Position, data=Tank6, col=Position, ylim=c(0,160), main = "Tank6")

Tank7 <- subset(light, Tank==7)
plot(PAR ~ Position, data=Tank7, col=Position, ylim=c(0,160), main = "Tank7")

Tank8 <- subset(light, Tank==8)
plot(PAR ~ Position, data=Tank8, col=Position, ylim=c(0,160), main = "Tank8")

Tank9 <- subset(light, Tank==9)
plot(PAR ~ Position, data=Tank9, col=Position, ylim=c(0,160), main = "Tank9")

Tank10 <- subset(light, Tank==10)
plot(PAR ~ Position, data=Tank10, col=Position, ylim=c(0,160), main = "Tank10")

Tank11 <- subset(light, Tank==11)
plot(PAR ~ Position, data=Tank11, col=Position, ylim=c(0,160), main = "Tank11")

Tank12 <- subset(light, Tank==12)
plot(PAR ~ Position, data=Tank12, col=Position, ylim=c(0,160), main = "Tank12")

Tank15 <- subset(light, Tank==15)
plot(PAR ~ Position, data=Tank15, col=Position, ylim=c(0,160), main = "Tank15")

Tank16 <- subset(light, Tank==16)
plot(PAR ~ Position, data=Tank16, col=Position, ylim=c(0,160), main = "Tank16")

Tank17 <- subset(light, Tank==17)
plot(PAR ~ Position, data=Tank17, col=Position, ylim=c(0,160), main = "Tank17")

Tank18 <- subset(light, Tank==18)
plot(PAR ~ Position, data=Tank18, col=Position, ylim=c(0,160), main = "Tank18")

dev.off()

#mean6 <- aggregate(PAR ~ Tank*Position, data=light, FUN=mean)
#mean6

# Plot light by tank
pdf("../Output/Light.by.Tank.pdf")
plot(as.factor(light$Tank), light$PAR, xlab="Tank", ylab="PAR", ylim=c(0,160), las=2)
boxplot(PAR ~ Tank, data = light, outpch = NA) 
stripchart(PAR ~ Tank, data = light, 
           vertical = TRUE, method = "jitter", 
           pch = 21, col = "blue", bg = "blue", 
           add = TRUE) 
dev.off()










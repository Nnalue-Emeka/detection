install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("patchwork")

library(patchwork)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

cor(NigDS[c('LEV', 'Fsiz','Bind','Bsiz','Fage', 'FRQ')])

hist(NigDS$FRQ)

plot(FRQ~LEV,data=NigDS)

plot(FRQ~Fsiz,data=NigDS)

summary(NigDS)

NigDS$X=as.factor(NigDS$X)

FRQ.lm <- lm(FRQ ~ LEV + Fsiz+ Bind + Fage + X, data = NigDS)

FRQ.lm

summary(FRQ.lm)

NigDS = NigDS %>% 
  mutate(X = relevel(X, ref = "Pre"))

FRQ.lm <- lm(FRQ ~ LEV + Fsiz+ Bind + Fage + X, data = NigDS)

FRQ.lm

summary(FRQ.lm)

par(mfrow =c(1,1))
plot(FRQ.lm)
par(mfrow=c(0.5,0.5))
return()

NigDS$predicted.y <- predict.lm(FRQ.lm, data = NigDS)


FRQ.plot <- ggplot(NigDS, aes(x=LEV, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = LEV, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Leverage \n and IFRS status",
       x = "Leverage",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot

FRQ.plot1 <- ggplot(NigDS, aes(x=Fsiz, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Fsiz, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Firm Size \n and IFRS status",
       x = "Firm Size",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot1

FRQ.plot2 <- ggplot(NigDS, aes(x=Bsiz, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Bsiz, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Board Size \n and IFRS status",
       x = "Board Size",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot2

FRQ.plot3 <- ggplot(NigDS, aes(x=Bind, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Bind, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Board \n independence and IFRS status",
       x = "Board Independence",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot3

FRQ.plot4 <- ggplot(NigDS, aes(x=Fage, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Fage, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot4

waka <- (FRQ.plot|FRQ.plot1)/(FRQ.plot2|FRQ.plot3)/(FRQ.plot4)

waka

ggsave(filename = "FRQNGA1.png", plot =waka,
       width = 20, height = 25,dpi = 2500, units = "cm")

#EMG

cor(NigDS[c('LEV', 'Fsiz','Bind','Bsiz','Fage', 'EMG')])

hist(NigDS$EMG)

plot(EMG~LEV,data=NigDS)

plot(EMG~Fsiz,data=NigDS)

summary(NigDS)

NigDS$X=as.factor(NigDS$X)

EMG.lm <- lm(EMG ~ LEV + Fsiz+ Bind + Fage + X, data = NigDS)

EMG.lm

summary(EMG.lm)

NigDS = NigDS %>% 
  mutate(X = relevel(X, ref = "Pre"))

EMG.lm <- lm(EMG ~ LEV + Fsiz+ Bind + Fage + X, data = NigDS)

EMG.lm

summary(EMG.lm)

par(mfrow =c(1,1))
plot(EMG.lm)
par(mfrow=c(0.5,0.5))
return()

NigDS$predicted.y1 <- predict.lm(EMG.lm, data = NigDS)


EMG.plot <- ggplot(NigDS, aes(x=LEV, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = LEV, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Leverage \n and IFRS status",
       x = "Leverage",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot

EMG.plot1 <- ggplot(NigDS, aes(x=Fsiz, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Fsiz, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Firm Size \n and IFRS status",
       x = "Firm Size",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot1

EMG.plot2 <- ggplot(NigDS, aes(x=Bsiz, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Bsiz, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Board Size \n and IFRS status",
       x = "Board Size",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot2

EMG.plot3 <- ggplot(NigDS, aes(x=Bind, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Bind, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Board \n independence and IFRS status",
       x = "Board Independence",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot3

EMG.plot4 <- ggplot(NigDS, aes(x=Fage, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Fage, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot4

waka1 <- (EMG.plot|EMG.plot1)/(EMG.plot2|EMG.plot3)/(EMG.plot4)

waka1

ggsave(filename = "EMGNGA1.png", plot =waka1,
       width = 20, height = 25,dpi = 2500, units = "cm")

#FRQ2
cor(NigDS[c('Prof','Fage', 'FRQ')])

hist(NigDS$FRQ)

plot(FRQ~Fage,data=NigDS)

plot(FRQ~Prof,data=NigDS)

summary(NigDS)

NigDS$X=as.factor(NigDS$X)

FRQ2.lm <- lm(FRQ ~ Prof + Fage + X, data = NigDS)

FRQ2.lm

summary(FRQ2.lm)
NigDS = NigDS %>% 
  mutate(X = relevel(X, ref = "Pre"))

FRQ2.lm <- lm(FRQ ~ Prof + Fage + X, data = NigDS)

FRQ2.lm

summary(FRQ2.lm)

par(mfrow =c(1,1))
plot(FRQ2.lm)
par(mfrow=c(0.5,0.5))
return()

NigDS$predicted.y2 <- predict.lm(FRQ2.lm, data = NigDS)


FRQ2.plot <- ggplot(NigDS, aes(x=Prof, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Prof, y = predicted.y2, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality as a function of Profitability and IFRS status",
       x = "Profitability",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ2.plot

FRQ2.plot4 <- ggplot(NigDS, aes(x=Fage, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Fage, y = predicted.y2, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ2.plot4

waka2 <- (FRQ2.plot)/(FRQ2.plot4)

waka2

ggsave(filename = "FRQ2NGA1.png", plot =waka2,
       width = 20, height = 25,dpi = 2500, units = "cm")

#EMG2

cor(NigDS[c('Prof','Fage', 'EMG')])

hist(NigDS$EMG)

plot(EMG~Fage,data=NigDS)

plot(EMG~Prof,data=NigDS)

summary(NigDS)

NigDS$X=as.factor(NigDS$X)

EMG2.lm <- lm(EMG ~ Prof + Fage + X, data = NigDS)

EMG2.lm

summary(EMG2.lm)
NigDS = NigDS %>% 
  mutate(X = relevel(X, ref = "Pre"))

EMG2.lm <- lm(EMG ~ Prof + Fage + X, data = NigDS)

EMG2.lm

summary(EMG2.lm)

par(mfrow =c(1,1))
plot(EMG2.lm)
par(mfrow=c(0.5,0.5))
return()

NigDS$predicted.y3 <- predict.lm(EMG2.lm, data = NigDS)


EMG2.plot <- ggplot(NigDS, aes(x=Prof, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Prof, y = predicted.y3, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings Management as a function of Profitability and IFRS status",
       x = "Profitability",
       y = "Earnings Management",
       color = "IFRS Status")

EMG2.plot

EMG2.plot4 <- ggplot(NigDS, aes(x=Fage, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = NigDS, aes (x = Fage, y = predicted.y3, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings Management as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Earnings Management",
       color = "IFRS Status")

EMG2.plot4

waka3 <- (EMG2.plot)/(EMG2.plot4)

waka3

ggsave(filename = "EMG2NGA1.png", plot =waka3,
       width = 20, height = 25,dpi = 2500, units = "cm")
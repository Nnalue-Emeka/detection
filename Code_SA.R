cor(SouDS[c('LEV', 'Fsiz','Bind','Bsiz','Fage', 'FRQ')])

hist(SouDS$FRQ)

plot(FRQ~LEV,data=SouDS)

plot(FRQ~Fsiz,data=SouDS)

summary(SouDS)

SouDS$X=as.factor(SouDS$X)

FRQ.lm <- lm(FRQ ~ LEV + Fsiz+ Bind + Fage + X, data = SouDS)

FRQ.lm

summary(FRQ.lm)

SouDS = SouDS %>% 
  mutate(X = relevel(X, ref = "PRE"))

FRQ.lm <- lm(FRQ ~ LEV + Fsiz+ Bind + Fage + X, data = SouDS)

FRQ.lm

summary(FRQ.lm)

par(mfrow =c(1,1))
plot(FRQ.lm)
par(mfrow=c(0.5,0.5))
return()

SouDS$predicted.y <- predict.lm(FRQ.lm, data = SouDS)


FRQ.plot <- ggplot(SouDS, aes(x=LEV, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = LEV, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Leverage \n and IFRS status",
       x = "Leverage",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot

FRQ.plot1 <- ggplot(SouDS, aes(x=Fsiz, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Fsiz, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Firm Size \n and IFRS status",
       x = "Firm Size",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot1

FRQ.plot2 <- ggplot(SouDS, aes(x=Bsiz, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Bsiz, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Board Size \n and IFRS status",
       x = "Board Size",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot2

FRQ.plot3 <- ggplot(SouDS, aes(x=Bind, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Bind, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality \n as a function of Board \n independence and IFRS status",
       x = "Board Independence",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot3

FRQ.plot4 <- ggplot(SouDS, aes(x=Fage, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Fage, y = predicted.y, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ.plot4

waka <- (FRQ.plot|FRQ.plot1)/(FRQ.plot2|FRQ.plot3)/(FRQ.plot4)

waka

ggsave(filename = "FRQSOU1.png", plot =waka,
       width = 20, height = 25,dpi = 2500, units = "cm")

#EMG

cor(SouDS[c('LEV', 'Fsiz','Bind','Bsiz','Fage', 'EMG')])

hist(SouDS$EMG)

plot(EMG~LEV,data=SouDS)

plot(EMG~Fsiz,data=SouDS)

summary(SouDS)

SouDS$X=as.factor(SouDS$X)

EMG.lm <- lm(EMG ~ LEV + Fsiz+ Bind + Fage + X, data = SouDS)

EMG.lm

summary(EMG.lm)

SouDS = SouDS %>% 
  mutate(X = relevel(X, ref = "PRE"))

EMG.lm <- lm(EMG ~ LEV + Fsiz+ Bind + Fage + X, data = SouDS)

EMG.lm

summary(EMG.lm)

par(mfrow =c(1,1))
plot(EMG.lm)
par(mfrow=c(0.5,0.5))
return()

SouDS$predicted.y1 <- predict.lm(EMG.lm, data = SouDS)


EMG.plot <- ggplot(SouDS, aes(x=LEV, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = LEV, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Leverage \n and IFRS status",
       x = "Leverage",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot

EMG.plot1 <- ggplot(SouDS, aes(x=Fsiz, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Fsiz, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Firm Size \n and IFRS status",
       x = "Firm Size",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot1

EMG.plot2 <- ggplot(SouDS, aes(x=Bsiz, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Bsiz, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Board Size \n and IFRS status",
       x = "Board Size",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot2

EMG.plot3 <- ggplot(SouDS, aes(x=Bind, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Bind, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management \n as a function of Board \n independence and IFRS status",
       x = "Board Independence",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot3

EMG.plot4 <- ggplot(SouDS, aes(x=Fage, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Fage, y = predicted.y1, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings management as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Earnings management",
       color = "IFRS Status")

EMG.plot4

waka1 <- (EMG.plot|EMG.plot1)/(EMG.plot2|EMG.plot3)/(EMG.plot4)

waka1

ggsave(filename = "EMGSOU1.png", plot =waka1,
       width = 20, height = 25,dpi = 2500, units = "cm")

#FRQ2
cor(SouDS[c('Prof','Fage', 'FRQ')])

hist(SouDS$FRQ)

plot(FRQ~Fage,data=SouDS)

plot(FRQ~Prof,data=SouDS)

summary(SouDS)

SouDS$X=as.factor(SouDS$X)

FRQ2.lm <- lm(FRQ ~ Prof + Fage + X, data = SouDS)

FRQ2.lm

summary(FRQ2.lm)
SouDS = SouDS %>% 
  mutate(X = relevel(X, ref = "PRE"))

FRQ2.lm <- lm(FRQ ~ Prof + Fage + X, data = SouDS)

FRQ2.lm

summary(FRQ2.lm)

par(mfrow =c(1,1))
plot(FRQ2.lm)
par(mfrow=c(0.5,0.5))
return()

SouDS$predicted.y2 <- predict.lm(FRQ2.lm, data = SouDS)


FRQ2.plot <- ggplot(SouDS, aes(x=Prof, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Prof, y = predicted.y2, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality as a function of Profitability and IFRS status",
       x = "Profitability",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ2.plot

FRQ2.plot4 <- ggplot(SouDS, aes(x=Fage, y=FRQ, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Fage, y = predicted.y2, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Finance Reporting Quality as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Finance Reporting Quality",
       color = "IFRS Status")

FRQ2.plot4

waka2 <- (FRQ2.plot)/(FRQ2.plot4)

waka2

ggsave(filename = "FRQ2SOU1.png", plot =waka2,
       width = 20, height = 25,dpi = 2500, units = "cm")

#EMG2

cor(SouDS[c('Prof','Fage', 'EMG')])

hist(SouDS$EMG)

plot(EMG~Fage,data=SouDS)

plot(EMG~Prof,data=SouDS)

summary(SouDS)

SouDS$X=as.factor(SouDS$X)

EMG2.lm <- lm(EMG ~ Prof + Fage + X, data = SouDS)

EMG2.lm

summary(EMG2.lm)
SouDS = SouDS %>% 
  mutate(X = relevel(X, ref = "Post"))

EMG2.lm <- lm(EMG ~ Prof + Fage + X, data = SouDS)

EMG2.lm

summary(EMG2.lm)

par(mfrow =c(1,1))
plot(EMG2.lm)
par(mfrow=c(0.5,0.5))
return()

SouDS$predicted.y3 <- predict.lm(EMG2.lm, data = SouDS)


EMG2.plot <- ggplot(SouDS, aes(x=Prof, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Prof, y = predicted.y3, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings Management as a function of Profitability and IFRS status",
       x = "Profitability",
       y = "Earnings Management",
       color = "IFRS Status")

EMG2.plot

EMG2.plot4 <- ggplot(SouDS, aes(x=Fage, y=EMG, col = X)) + 
  geom_point() + 
  geom_line(data = SouDS, aes (x = Fage, y = predicted.y3, color = X), size =1.25) + 
  theme_bw() + 
  labs(title = "Earnings Management as a function of Firm Age and IFRS status",
       x = "Firm Age",
       y = "Earnings Management",
       color = "IFRS Status")

EMG2.plot4

waka3 <- (EMG2.plot)/(EMG2.plot4)

waka3

ggsave(filename = "EMG2SOU1.png", plot =waka3,
       width = 20, height = 25,dpi = 2500, units = "cm")

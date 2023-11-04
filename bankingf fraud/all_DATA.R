library(patchwork)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(performance)

colnames(Newdataset)<- c("IFRS_Status", "Bank", "Year", "Fsize", "Bmeet", "Lev","Bsize", "Bind", "Prof","FRQ", "Country", "EMG")


summary(Newdataset)

Newdataset$IFRS_Status=as.factor(Newdataset$IFRS_Status)


FRQ.lm <- lm(FRQ ~ Lev + Fsize+ + Prof + Bind + Bsize + Bmeet + IFRS_Status + Country, data = Newdataset)

FRQ.lm

summary(FRQ.lm)

png(filename = "myplot.png", width =780, height = 842, units = "px" , pointsize = 12, bg = "white", res = NA,
    restoreConsole = TRUE)

performance::check_model(FRQ.lm)

dev.off()

glimpse(Newdataset)
Newdataset$predicted.y <- predict.lm(FRQ.lm, data = Newdataset)


FRQ.plot <- ggplot(Newdataset, aes(x=Lev, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Lev, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Leverage \n and Country",
       x = "Leverage",
       y = "Financial Reporting Quality",
       color = "Country")

FRQ.plot

FRQ.plot1 <- ggplot(Newdataset, aes(x=Fsize, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Fsize, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Firm Size \n and Country",
       x = "Firm Size",
       y = "Financial Reporting Quality",
       color = "Country")

FRQ.plot1

FRQ.plot2 <- ggplot(Newdataset, aes(x=Bsize, y=FRQ,)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bsize, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Board Size \n and Country",
       x = "Board Size",
       y = "Financial Reporting Quality",
       color = "Country")

FRQ.plot2

FRQ.plot3 <- ggplot(Newdataset, aes(x=Bind, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bind, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Board \n independence and Country",
       x = "Board Independence",
       y = "Financial Reporting Quality",
       color = "Country")

FRQ.plot3

FRQ.plot4 <- ggplot(Newdataset, aes(x=Bmeet, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bmeet, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Board \nMeeting and Country",
       x = "Board Meeting",
       y = "Financial Reporting Quality",
       color = "Country")

FRQ.plot4

FRQ.plot5 <- ggplot(Newdataset, aes(x=Prof, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Prof, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality\n as a function of Profitability \n and Country",
       x = "Profitability",
       y = "Financial Reporting Quality",
       color = "Country")

FRQ.plot5

waka <- (FRQ.plot|FRQ.plot1)/(FRQ.plot2|FRQ.plot3)/(FRQ.plot4|FRQ.plot5)

waka

ggsave(filename = "FRQT.png", plot =waka,
       width = 20, height = 25,dpi = 2500, units = "cm")


FRQI.plot <- ggplot(Newdataset, aes(x=Lev, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Lev, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Leverage \n and IFRS_Status",
       x = "Leverage",
       y = "Financial Reporting Quality",
       color = "IFRS_Status")

FRQI.plot

FRQI.plot1 <- ggplot(Newdataset, aes(x=Fsize, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Fsize, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Firm Size \n and IFRS_Status",
       x = "Firm Size",
       y = "Financial Reporting Quality",
       color = "IFRS_Status")

FRQI.plot1

FRQI.plot2 <- ggplot(Newdataset, aes(x=Bsize, y=FRQ,)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bsize, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Board Size \n and IFRS_Status",
       x = "Board Size",
       y = "Financial Reporting Quality",
       color = "IFRS_Status")

FRQI.plot2

FRQI.plot3 <- ggplot(Newdataset, aes(x=Bind, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bind, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Board \n independence and IFRS_Status",
       x = "Board Independence",
       y = "Financial Reporting Quality",
       color = "IFRS_Status")

FRQI.plot3

FRQI.plot4 <- ggplot(Newdataset, aes(x=Bmeet, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bmeet, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality \n as a function of Board \nMeeting and IFRS_Status",
       x = "Board Meeting",
       y = "Financial Reporting Quality",
       color = "IFRS_Status")

FRQI.plot4

FRQI.plot5 <- ggplot(Newdataset, aes(x=Prof, y=FRQ)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Prof, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Financial Reporting Quality\n as a function of Profitability \n and IFRS_Status",
       x = "Profitability",
       y = "Financial Reporting Quality",
       color = "IFRS_Status")

FRQI.plot5

waka2 <- (FRQI.plot|FRQI.plot1)/(FRQI.plot2|FRQI.plot3)/(FRQI.plot4|FRQI.plot5)

waka2

ggsave(filename = "FRQIFRS.png", plot =waka2,
       width = 20, height = 25,dpi = 2500, units = "cm")

EMG.lm <- lm(EMG ~ Fsize+ + Prof + Bind + Bsize + Bmeet + IFRS_Status + Country, data = Newdataset)

EMG.lm

summary(EMG.lm)

png(filename = "myplot23.png", width =780, height = 842, units = "px" , pointsize = 12, bg = "white", res = NA,
    restoreConsole = TRUE)

performance::check_model(EMG.lm)

dev.off()
Newdataset$predicted.yEMG <- predict.lm(EMG.lm, data = Newdataset)




EMG.plot1 <- ggplot(Newdataset, aes(x=Fsize, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Fsize, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice \n as a function of Firm Size \n and Country",
       x = "Firm Size",
       y = "Earning Management Practice",
       color = "Country")

EMG.plot1

EMG.plot2 <- ggplot(Newdataset, aes(x=Bsize, y=EMG,)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bsize, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice \n as a function of Board Size \n and Country",
       x = "Board Size",
       y = "Earning Management Practice",
       color = "Country")

EMG.plot2

EMG.plot3 <- ggplot(Newdataset, aes(x=Bind, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bind, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Manageement Practice \n as a function of Board \n independence and Country",
       x = "Board Independence",
       y = "Earning Management Practice",
       color = "Country")

EMG.plot3

EMG.plot4 <- ggplot(Newdataset, aes(x=Bmeet, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bmeet, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice \n as a function of Board \nMeeting and Country",
       x = "Board Meeting",
       y = "Earning Management Practice",
       color = "Country")

EMG.plot4

EMG.plot5 <- ggplot(Newdataset, aes(x=Prof, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Prof, y = predicted.y, color = Country), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice\n as a function of Profitability \n and Country",
       x = "Profitability",
       y = "Earning Management Practice",
       color = "Country")

EMG.plot5

waka <- (EMG.plot1)/(EMG.plot2|EMG.plot3)/(EMG.plot4|EMG.plot5)

waka

ggsave(filename = "EMGT.png", plot =waka,
       width = 20, height = 25,dpi = 2500, units = "cm")


EMGI.plot1 <- ggplot(Newdataset, aes(x=Fsize, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Fsize, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice as a function of Firm Size and IFRS_Status",
       x = "Firm Size",
       y = "Earning Management Practice",
       color = "IFRS_Status")

EMGI.plot1

EMGI.plot2 <- ggplot(Newdataset, aes(x=Bsize, y=EMG,)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bsize, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice \n as a function of Board Size \n and IFRS_Status",
       x = "Board Size",
       y = "Earning Management Practice",
       color = "IFRS_Status")

EMGI.plot2

EMGI.plot3 <- ggplot(Newdataset, aes(x=Bind, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bind, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice \n as a function of Board \n independence and IFRS_Status",
       x = "Board Independence",
       y = "Earning Management Practice",
       color = "IFRS_Status")

EMGI.plot3

EMGI.plot4 <- ggplot(Newdataset, aes(x=Bmeet, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Bmeet, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice \n as a function of Board \nMeeting and IFRS_Status",
       x = "Board Meeting",
       y = "Earning Management Practice",
       color = "IFRS_Status")

EMGI.plot4

EMGI.plot5 <- ggplot(Newdataset, aes(x=Prof, y=EMG)) + 
  geom_point() + 
  geom_line(data = Newdataset, aes (x = Prof, y = predicted.y, color = IFRS_Status), size =1.25) + 
  theme_bw() + 
  labs(title = "Earning Management Practice\n as a function of Profitability \n and IFRS_Status",
       x = "Profitability",
       y = "Earning Management Practice",
       color = "IFRS_Status")

EMGI.plot5

waka2 <- (EMGI.plot1)/(EMGI.plot2|EMGI.plot3)/(EMGI.plot4|EMGI.plot5)

waka2

ggsave(filename = "EMGIFRS.png", plot =waka2,
       width = 20, height = 25,dpi = 2500, units = "cm")
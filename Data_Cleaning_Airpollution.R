library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")

JC_PM2.5_2016 <- read.csv("JakartaCentral_PM2.5_2016_YTD.csv")
JC_PM2.5_2017 <- read.csv("JakartaCentral_PM2.5_2017.csv")
JC_PM2.5_2018 <- read.csv("JakartaCentral_PM2.5_2018_YTD.csv")
HCM_PM2.5_2016 <- read.csv("HoChiMinhCity_PM2.5_2016_YTD.csv")
HCM_PM2.5_2017 <- read.csv("HoChiMinhCity_PM2.5_2017_YTD.csv")
HCM_PM2.5_2018 <- read.csv("HoChiMinhCity_PM2.5_2018_YTD.csv")


## clean  up  2016  data

JC_PM2.5_2016$Raw.Conc.[JC_PM2.5_2016$Raw.Conc.<0] <- NA
JC_PM2.5_2016$Raw.Conc.[JC_PM2.5_2016$Raw.Conc.>900] <- -NA

daily_2016_JC <- JC_PM2.5_2016 %>%
  mutate(day = as.Date(Date..LT., format="%Y-%m-%d")) %>%
  group_by(day) %>% # 
  summarise(JC.Av.Conc =mean(Raw.Conc.)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

## clean  up  2017  data

JC_PM2.5_2017$Raw.Conc.[JC_PM2.5_2017$Raw.Conc.<0] <- NA
JC_PM2.5_2017$Raw.Conc.[JC_PM2.5_2017$Raw.Conc.>900] <- -NA

daily_2017_JC <- JC_PM2.5_2017 %>%
  mutate(day = as.Date(Date..LT., format="%Y-%m-%d")) %>%
  group_by(day) %>% # group by the day column
  summarise(JC.Av.Conc =mean(Raw.Conc.)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

JC_PM2.5_2018$Raw.Conc.[JC_PM2.5_2018$Raw.Conc.<0] <- NA
JC_PM2.5_2018$Raw.Conc.[JC_PM2.5_2018$Raw.Conc.>900] <- -NA

daily_2018_JC <- JC_PM2.5_2018 %>%
  mutate(day = as.Date(Date..LT., format="%m/%d/%y")) %>%
  group_by(day) %>% # group by the day column
  summarise(JC.Av.Conc =mean(Raw.Conc.)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

# Ho Chi Minh City

## clean  up  2016  data

HCM_PM2.5_2016$Raw.Conc.[HCM_PM2.5_2016$Raw.Conc.<0] <- NA
HCM_PM2.5_2016$Raw.Conc.[HCM_PM2.5_2016$Raw.Conc.>900] <- -NA

daily_2016_HCM <- HCM_PM2.5_2016 %>%
  mutate(day = as.Date(Date..LT., format="%Y-%m-%d")) %>%
  group_by(day) %>% # group by the day column
  summarise(HCM.Av.Conc =mean(Raw.Conc.)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

## clean  up  2017  data

HCM_PM2.5_2017$Raw.Conc.[HCM_PM2.5_2017$Raw.Conc.<0] <- NA
HCM_PM2.5_2017$Raw.Conc.[HCM_PM2.5_2017$Raw.Conc.>900] <- -NA

daily_2017_HCM <- HCM_PM2.5_2017 %>%
  mutate(day = as.Date(Date..LT., format="%Y-%m-%d")) %>%
  group_by(day) %>% # group by the day column
  summarise(HCM.Av.Conc =mean(Raw.Conc.)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

# clean up 2018 data

HCM_PM2.5_2018$Raw.Conc.[HCM_PM2.5_2018$Raw.Conc.<0] <- NA
HCM_PM2.5_2018$Raw.Conc.[HCM_PM2.5_2018$Raw.Conc.>900] <- -NA

daily_2018_HCM <- HCM_PM2.5_2018 %>%
  mutate(day = as.Date(Date..LT., format="%Y-%m-%d")) %>%
  group_by(day) %>% # group by the day column
  summarise(HCM.Av.Conc =mean(Raw.Conc.)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

plot(daily_2016_JC$day, daily_2016_JC$JC.Av.Conc,
     main="2016 PM2.5 distribution",
     xlab="Day of the Year",
     ylab="Mean Daily PM2.5 Concentration (UG/m3)")



plot(daily_2017_JC$day, daily_2017_JC$JC.Av.Conc,
     main="2017 PM2.5 distribution",
     xlab="Day of the Year",
     ylab="Mean Daily PM2.5 Concentration (UG/m3)")


plot(daily_2018_JC$day, daily_2018_JC$JC.Av.Conc,
      main="2018 PM2.5 distribution",
     xlab="Day of the Year",
     ylab="Mean Daily PM2.5 Concentration (UG/m3)")


# Ho Chi Minh City

plot(daily_2016_HCM$day, daily_2016_HCM$HCM.Av.Conc,
     main="HCM 2016 PM2.5 distribution",
     xlab="Day of the Year",
     ylab="Mean Daily PM2.5 Concentration (UG/m3)")


plot(daily_2017_HCM$day, daily_2017_HCM$HCM.Av.Conc,
     main="HCM 2017 PM2.5 distribution",
     xlab="Day of the Year",
     ylab="Mean Daily PM2.5 Concentration (UG/m3)")

plot(daily_2018_HCM$day, daily_2018_HCM$HCM.Av.Conc,
     main="HCM 2018 PM2.5 distribution",
     xlab="Day of the Year",
     ylab="Mean Daily PM2.5 Concentration (UG/m3)")

# Transform the BERDO data into tidy form for 2016
data.2016 <- gather(daily_2016_JC,                     # Use the gather() function and specify the berdo17 dataframe
                 key = `2016 PM2.5`,      # Create the name of the key (category) column
                 value = `JC.Av.Conc`,             # Create the name of the value column
                 `JC.Av.Conc`, na.rm = T) # List the variables the gather

# create the boxplot
JC2016.boxplot <- ggplot(data.2016) +
  geom_boxplot(aes(x = `2016 PM2.5`, y = `JC.Av.Conc`))
JC2016.boxplot

# Transform the BERDO data into tidy form 2017
data.2017 <- gather(daily_2017_JC,                     # Use the gather() function and specify the berdo17 dataframe
                    key = `2017 PM2.5`,      # Create the name of the key (category) column
                    value = `JC.Av.Conc`,             # Create the name of the value column
                    `JC.Av.Conc`, na.rm = T) # List the variables the gather

# create the boxplot
JC2017.boxplot <- ggplot(data.2017) +
  geom_boxplot(aes(x = `2017 PM2.5`, y = `JC.Av.Conc`))
JC2017.boxplot

# HCM

# Transform the BERDO data into tidy form 2016
data.2016 <- gather(daily_2016_HCM,                     # Use the gather() function and specify the berdo17 dataframe
                    key = `2016 PM2.5`,      # Create the name of the key (category) column
                    value = `JC.Av.Conc`,             # Create the name of the value column
                    `JC.Av.Conc`, na.rm = T) # List the variables the gather

# create the boxplot
HCM2016.boxplot <- ggplot(data.2016) +
  geom_boxplot(aes(x = `2016 PM2.5`, y = `HCM.Av.Conc`))
HCM2016.boxplot

# Transform the BERDO data into tidy form 2016
data.2017 <- gather(daily_2017_HCM,                     # Use the gather() function and specify the berdo17 dataframe
                    key = `2017 PM2.5`,      # Create the name of the key (category) column
                    value = `HCM.Av.Conc`,             # Create the name of the value column
                    `HCM.Av.Conc`, na.rm = T) # List the variables the gather

# create the boxplot
HCM2017.boxplot <- ggplot(data.2017) +
  geom_boxplot(aes(x = `2017 PM2.5`, y = `HCM.Av.Conc`))
HCM2017.boxplot

# Transform the BERDO data into tidy form 2016
data.2018 <- gather(daily_2018_HCM,                     # Use the gather() function and specify the berdo17 dataframe
                    key = `2018 PM2.5`,      # Create the name of the key (category) column
                    value = `HCM.Av.Conc`,             # Create the name of the value column
                    `HCM.Av.Conc`, na.rm = T) # List the variables the gather

# create the boxplot
HCM2018.boxplot <- ggplot(data.2018) +
  geom_boxplot(aes(x = `2018 PM2.5`, y = `HCM.Av.Conc`))
HCM2018.boxplot



Fires_Indonesia <- read.csv("archive_fires_for_Indonesia.csv")

 # 2016

 fires2016 <- subset(Fires_Indonesia, format(as.Date(ACQ_DATE),"%Y")==2016)
 
 fires2016$FRP[fires2016$CONFIDENCE < 75] <- 0
 fires2016$BRIGHTNESS[fires2016$CONFIDENCE < 75] <- 0
 
 ##  Summarize your fire  data  
 
 daily_2016_fires <- fires2016 %>%
   mutate(day = as.Date(ACQ_DATE, format="%Y-%m-%d")) %>%
   group_by(day) %>% # group by the day column
   summarise(Av.Bright = mean(BRIGHTNESS), Av.FRP = mean(FRP), Pixels = n() ) %>%  # calculate the SUM of all precipitation that occurred on each day
   na.omit()

 fires2016 <- subset(Fires_Indonesia, format(as.Date(ACQ_DATE),"%Y")==2016)
 
 fires2016$FRP[fires2016$CONFIDENCE < 75] <- 0
 fires2016$BRIGHTNESS[fires2016$CONFIDENCE < 75] <- 0
 
 ##  2017
 
 fires2017 <- subset(Fires_Indonesia, format(as.Date(ACQ_DATE),"%Y")==2017)
 
 fires2017$FRP[fires2017$CONFIDENCE < 75] <- 0
 fires2017$BRIGHTNESS[fires2017$CONFIDENCE < 75] <- 0
 
 daily_2017_fires <- fires2017 %>%
   mutate(day = as.Date(ACQ_DATE, format="%Y-%m-%d")) %>%
   group_by(day) %>% # group by the day column
   summarise(Av.Bright = mean(BRIGHTNESS), Av.FRP = mean(FRP),Pixels = n() ) %>%  # calculate the SUM of all precipitation that occurred on each day
   na.omit()

 # 2018
 
 fires2018 <- subset(Fires_Indonesia, format(as.Date(ACQ_DATE),"%Y")==2018)
 
 fires2018$FRP[fires2018$CONFIDENCE < 75] <- 0
 fires2018$BRIGHTNESS[fires2018$CONFIDENCE < 75] <- 0
 
 daily_2018_fires <- fires2018 %>%
   mutate(day = as.Date(ACQ_DATE, format="%Y-%m-%d")) %>%
   group_by(day) %>% # group by the day column
   summarise(Av.Bright = mean(BRIGHTNESS), Av.FRP = mean(FRP), Pixels = n() ) %>%  # calculate the SUM of all precipitation that occurred on each day
   na.omit()
 
allfires <- rbind(daily_2016_fires, daily_2017_fires, daily_2018_fires) 
allpollution_JC <- rbind(daily_2016_JC, daily_2017_JC, daily_2018_JC)
allpollution_HCM <- rbind(daily_2016_HCM, daily_2017_HCM, daily_2018_HCM)

allpollution_JC$City = 'Jakarta'
allpollution_HCM$City = 'Ho Chi Minh City'

# use this for joining and having separate columns for HCM and Jakarta
total <- full_join(allpollution_JC, allpollution_HCM, by=c("day"))
total <- full_join(allfires, total, by=c("day"))

# use this code when you want a variable called city
colnames(allpollution_JC)[colnames(allpollution_JC)=="JC.Av.Conc"] <- "Av.Conc"
colnames(allpollution_HCM)[colnames(allpollution_HCM)=="HCM.Av.Conc"] <- "Av.Conc"

# here I normalize everything before I join them!

allpollution_JC$Month <- month(allpollution_JC$day)

allpollution_JC$Season[allpollution_JC$Month >= 4 & allpollution_JC$Month <= 9] <- 'Summer'
allpollution_JC$Season[allpollution_JC$Month < 4] <- 'Winter'
allpollution_JC$Season[allpollution_JC$Month > 9] <- 'Winter'

allpollution_HCM$Month <- month(allpollution_HCM$day)

allpollution_HCM$Season[allpollution_HCM$Month >= 4 & allpollution_HCM$Month <= 9] <- 'Summer'
allpollution_HCM$Season[allpollution_HCM$Month < 4] <- 'Winter'
allpollution_HCM$Season[allpollution_HCM$Month > 9] <- 'Winter'

allpollution_JC$mean[allpollution_JC$Season == 'Summer'] <- mean_summer_JC
allpollution_JC$mean[allpollution_JC$Season == 'Winter'] <- mean_winter_JC
allpollution_HCM$mean[allpollution_HCM$Season == 'Summer'] <- mean_summer_HCM
allpollution_HCM$mean[allpollution_HCM$Season == 'Winter'] <- mean_winter_HCM

allpollution_JC$Av.Conc <-  allpollution_JC$Av.Conc-allpollution_JC$mean
allpollution_HCM$Av.Conc <-  allpollution_HCM$Av.Conc-allpollution_HCM$mean

total_2 <- rbind(allpollution_JC, allpollution_HCM)
total_2 <- left_join(allfires, total_2, by=c("day"))
total_2$TotalFRP <- total_2$Av.FRP*total_2$Pixels
total_2$TotalBright <- total_2$Av.Bright*total_2$Pixels

# testing correlations of fire params

cor.test(total$Av.Bright, total$Av.FRP)
cor.test(total$Av.Bright, total$Pixels)
cor.test(total$Av.FRP, total$Pixels)
cor.test(total_norm$JC.Av.Conc, total_norm$HCM.Av.Conc)

# Av. Bright, Av. FRP and pixels are all correlated

plot(total$Av.Bright, total$Av.FRP)
plot(total$Av.Bright, total$Pixels)
plot(total$Av.FRP, total$Pixels)

# finding the total FRP and brightness of fire per day
total$TotalFRP <- total$Av.FRP*total$Pixels
total$TotalBright <- total$Av.Bright*total$Pixels

cor.test(total$TotalFRP, total$JC.Av.Conc)
cor.test(total$Av.FRP, total$JC.Av.Conc)
cor.test(total$TotalFRP, total$HCM.Av.Conc)
cor.test(total$Av.FRP, total$HCM.Av.Conc)

plot(total$TotalFRP, total$JC.Av.Conc)
plot(total$TotalFRP, total$HCM.Av.Conc)

#  plotting jakarta 

ggplot(aes(x = day, y = JC.Av.Conc), data = total)+
  geom_smooth(na.rm = T)+geom_jitter(alpha = 0.2, na.rm = T)+
  scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
  labs( x = "Date", y = "Daily PM 2.5 Concentration (ug/m3)",
        title = "Jakarta Daily PM 2.5 Concentration")+
  theme(plot.title = element_text(size=16, face="bold", 
                                  margin = margin(10, 0, 10, 0)))

# Plotting Ho Chi Minh City

ggplot(aes(x = day, y = HCM.Av.Conc), data = total)+
  geom_smooth(na.rm = T)+geom_jitter(alpha = 0.2, na.rm = T)+
  scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
  labs( x = "Date", y = "Daily PM 2.5 Concentration (ug/m3)",
        title = "Ho Chi Minh Daily PM 2.5 Concentration")+
  theme(plot.title = element_text(size=16, face="bold", 
                                  margin = margin(10, 0, 10, 0)))

# Jakarta vs.Ho Chi Minh City

ggplot(aes(x = day), data = total)+
  geom_smooth(aes(y = JC.Av.Conc, color = "Jakarta"), na.rm = T )+
  geom_smooth(aes(y = HCM.Av.Conc, color = "Ho Chi Minh City"))+
  scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
  labs( x = "Date", y = "Daily PM 2.5 Concentration (ug/m3)",
        title = "Jakarta vs. Ho Chi Minh City Daily PM 2.5 Concentration")+
  scale_color_discrete(name="City")

ggplot(aes(x = day, y = TotalFRP), data = total)+
  geom_smooth(na.rm = T)+geom_jitter(alpha = 0.2, na.rm = T)+
  scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
  scale_y_log10()+
  labs( x = "Date", y = "Total FRP (log 10)",
        title = "Scaled Daily Total FRP ")

ggplot(aes(x = day), data = total)+
  geom_smooth(aes(y = JC.Av.Conc, color = "Jakarta"), na.rm = T )+
  geom_smooth(aes(y = HCM.Av.Conc, color = "Ho Chi Minh City"),  na.rm = T)+
  geom_jitter(aes(y = TotalFRP/500, color = "Total FRP"))+
  scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "Total FRP"))+
  labs( x = "Date", y = "Daily PM 2.5 Concentration (ug/m3)",
        title = "Daily PM 2.5 Concentration vs. Total FRP")+
  scale_color_discrete(name="Legend")+
  theme(plot.title = element_text(size=16, face="bold", 
                                   margin = margin(10, 0, 10, 0)))

ggplot(aes(x = day), data = total)+
  geom_smooth(aes(y = JC.Av.Conc, color = "Jakarta"), na.rm = T )+
  geom_smooth(aes(y = HCM.Av.Conc, color = "Ho Chi Minh City"),  na.rm = T)+
  geom_jitter(aes(y = Pixels/30, color = "Pixels"))+
  scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
  scale_y_continuous(sec.axis = sec_axis(~.*30, name = "Pixels"))+
  labs( x = "Date", y = "Daily PM 2.5 Concentration (ug/m3)",
        title = "Daily PM 2.5 Concentration vs. Pixels")+
  scale_color_discrete(name="Legend")+
  theme(plot.title = element_text(size=16, face="bold", 
                                  margin = margin(10, 0, 10, 0)))

library(lubridate)

total$Month <- month(total$day)
total$Season[total$Month >= 4 & total$Month <= 9] <- 'Summer'
total$Season[total$Month < 4] <- 'Winter'
total$Season[total$Month > 9] <- 'Winter'

 total %>%
  #select(Date, Price) %>%
  group_by(Season) %>%
  summarise(season.JC.Conc = mean(JC.Av.Conc, na.rm = TRUE), 
            season.HCM.Conc = mean(HCM.Av.Conc, na.rm = TRUE))
 
 mean_summer_JC = 42.0
 mean_winter_JC = 30.9
 mean_summer_HCM = 23.3
 mean_winter_HCM = 33.7
 
 total_norm <- total

 total_norm$mean_JC[total_norm$Season == 'Summer'] <- mean_summer_JC
 total_norm$mean_HCM[total_norm$Season == 'Summer'] <- mean_summer_HCM
 total_norm$mean_JC[total_norm$Season == 'Winter'] <- mean_winter_JC
 total_norm$mean_HCM[total_norm$Season == 'Winter'] <- mean_winter_HCM
 
 total_norm$JC.Av.Conc <-  total_norm$JC.Av.Conc-total_norm$mean_JC
 total_norm$HCM.Av.Conc <-   total_norm$HCM.Av.Conc-total_norm$mean_HCM

  ## plots of normalized data!!
 
 ggplot(aes(x = day), data = total_norm)+
   geom_smooth(aes(y = JC.Av.Conc, color = "Jakarta"), na.rm = T )+
   geom_smooth(aes(y = HCM.Av.Conc, color = "Ho Chi Minh City"))+
   scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
   labs( x = "Date", y = "Season Normalized Concentration (ug/m3)",
         title = "Jakarta vs. Ho Chi Minh City Season Normalized PM 2.5 Concentration")+
   scale_color_discrete(name="City")
 
 ggplot(aes(x = day), data = total_norm)+
   geom_smooth(aes(y = JC.Av.Conc, color = "Jakarta"), na.rm = T )+
   geom_smooth(aes(y = HCM.Av.Conc, color = "Ho Chi Minh City"),  na.rm = T)+
   geom_jitter(aes(y = TotalFRP/1000, color = "Total FRP"), alpha = 0.2)+
   scale_x_date(date_labels = "%m/%y", date_breaks = "3 months")+
   scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Total FRP"))+
   labs( x = "Date", y = "Season Nomalized PM 2.5 Concentration (ug/m3)",
         title = "Season Normalized PM 2.5 Concentration vs. Total FRP")+
   scale_color_discrete(name="Legend")+
   theme(plot.title = element_text(size=16, face="bold", 
                                   margin = margin(10, 0, 10, 0)))
 
# calculate regression
 
 y <- lm(formula = Av.Conc ~ TotalFRP +
      TotalBright + as.factor(City) +
      as.factor(City):TotalFRP + as.factor(City):TotalBright
    , data = total_2)
 
 y1 <- lm(formula = Av.Conc ~ TotalFRP +
       as.factor(City) +
      as.factor(City):TotalFRP 
    , data = total_2)
 
y2 <- lm(formula = Av.Conc ~ TotalBright +
      as.factor(City) +
      as.factor(City):TotalBright 
    , data = total_2)
 
 model <- lm(formula = Av.Conc ~ TotalFRP +
      as.factor(City) +
      as.factor(City):TotalFRP 
    , data = total_2)
 
 



 
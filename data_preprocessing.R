setwd("/Users/Michael/R working directory/Master thesis")
getwd()
#install.packages('lubridate')
#install.packages('mapview')
library(lubridate)
#install.packages(c('lubridate','dplyr','tidyr','sp','raster','rgdal','rgeos','spdep','sf','ggplot2'))
library(dplyr)
library(tidyr)
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)
library(sf)
library(ggplot2)
library(mapview)

#clear workspace
rm(list=ls())

#read in data
data <- read.csv("./csv_data/SFpark_ParkingSensorData_HourlyOccupancy_20112013.csv", header = TRUE,
                 stringsAsFactors = F)


##mutate CAL_DATE into 'Date' format and START_TIME_DT into POSIXct format
data$CAL_DATE <- as.Date(data$CAL_DATE, format='%d-%b-%Y')
data$START_TIME_DT <- as.POSIXct(data$START_TIME_DT, format='%d-%b-%Y %H:%M:%S')
data$WEEKDAY <- weekdays(data$CAL_DATE)
data$WEEKDAY_INT <- wday(data$START_TIME_DT)

##compute occupancy rate
data$OCC_RATE <- data$TOTAL_OCCUPIED_TIME / 
  (data$TOTAL_OCCUPIED_TIME+data$TOTAL_VACANT_TIME)

##derive number of spaces per parking segment
data$PARK_CAPACITY <- (data$TOTAL_TIME / 3600)

##number of data points each day from 2011-2013
daily_sensors <- tally(group_by(data, CAL_DATE))
ggplot(data = daily_sensors, aes(x = CAL_DATE, y = n)) + 
  geom_line(color = "#FC4E07", size = 1) +
  xlab("Time") + ylab("Daily data points") + ylim(8500,9750)

##number of functioning sensors each hour from 2011-2013
hourly_sensors <- tally(group_by(data, START_TIME_DT))
ggplot(data = hourly_sensors, aes(x = START_TIME_DT, y = n)) + 
  geom_line(color = "#FC4E07", size = 1) +
  xlab("Time") + ylab("Hourly Sensors working") + ylim(350,410)


##create data for 2011.04.08-2011-07-04
dataAMJ <- data
dataAMJ <- dataAMJ[dataAMJ$CAL_DATE>="2011-04-08" & dataAMJ$CAL_DATE<="2011-07-04",]

##number of functioning sensors each hour for 2011.04.15-2011-07-15
hourly_sensorsAMJ <- tally(group_by(dataAMJ, START_TIME_DT) %>% print(n=100))
ggplot(data = hourly_sensorsAMJ, aes(x = START_TIME_DT, y = n)) + 
  geom_line(color = "#FC4E07", size = 1) +
  xlab("Time") + ylab("Sensors working")

##get rid of redundant cols
dataAMJ <- dataAMJ[ , (names(dataAMJ) %in% 
                         c("BLOCK_ID","STREET_NAME","BLOCK_NUM","STREET_BLOCK",
                           "PM_DISTRICT_NAME","START_TIME_DT","CAL_DATE","TIME_OF_DAY",
                           "DAY_TYPE","WEEKDAY","WEEKDAY_INT","TOTAL_TIME","TOTAL_OCCUPIED_TIME",
                           "TOTAL_VACANT_TIME","OCC_RATE","PARK_CAPACITY"))]


##Example Valencia ST 800
ValenciaST800 <- dataAMJ[dataAMJ$BLOCK_ID==70008,]
ValenciaST800May <- ValenciaST800[ValenciaST800$CAL_DATE>="2011-05-01" & 
                                 ValenciaST800$CAL_DATE<="2011-05-06",]

ggplot(data = ValenciaST800May, aes(x = START_TIME_DT , y = OCC_RATE)) + 
  geom_line(color = "#FC4E07", size = 1) +
  xlab("Time") + ylab("OCC RATE")

##data with NA values for occupancy
dataAMJna <- dataAMJ[is.na(dataAMJ$OCC_RATE),]
##data without NA values for occupancy
dataAMJnona <- dataAMJ[!is.na(dataAMJ$OCC_RATE),]

##street data with missing occupancy data & number of missing values
street_missing_data <- dataAMJna %>%
  group_by(dataAMJna$STREET_BLOCK) %>%
  summarise(length(STREET_BLOCK))

##exclude streets with missing occupancy data
dataAMJ <- dataAMJ[ ! dataAMJ$STREET_BLOCK %in% street_missing_data$`dataAMJna$STREET_BLOCK`,]

##create lags of occupancy rates
dataAMJ[c("OCC_RATE_t1", "OCC_RATE_t2", "OCC_RATE_t3", "OCC_RATE_t4","OCC_RATE_t5",
          "OCC_RATE_t6", "OCC_RATE_t7", "OCC_RATE_t8", "OCC_RATE_t9","OCC_RATE_t10")] <- NA

dataAMJ <- dataAMJ %>%
  group_by(BLOCK_ID) %>%
  arrange(START_TIME_DT, .by_group = TRUE) %>%
  mutate(OCC_RATE_t1 = lag(OCC_RATE, 1)) %>%
  mutate(OCC_RATE_t2 = lag(OCC_RATE, 2)) %>%
  mutate(OCC_RATE_t3 = lag(OCC_RATE, 3)) %>%
  mutate(OCC_RATE_t4 = lag(OCC_RATE, 4)) %>%
  mutate(OCC_RATE_t5 = lag(OCC_RATE, 5)) %>%
  mutate(OCC_RATE_t6 = lag(OCC_RATE, 6)) %>%
  mutate(OCC_RATE_t7 = lag(OCC_RATE, 7)) %>%
  mutate(OCC_RATE_t8 = lag(OCC_RATE, 8)) %>%
  mutate(OCC_RATE_t9 = lag(OCC_RATE, 9)) %>%
  mutate(OCC_RATE_t10 = lag(OCC_RATE, 10))

##cut data from 2011-04-11 - 2011-07-03 (monday to sunday (12 weeks))
dataAMJ <- dataAMJ[dataAMJ$CAL_DATE>="2011-04-11" & dataAMJ$CAL_DATE<="2011-07-03",]

dataAMJ <- dataAMJ[order(dataAMJ$CAL_DATE),]
dataAMJ <- as.data.frame(dataAMJ)
dataAMJ$TIME_OF_DAY <- as.numeric(as.character(dataAMJ$TIME_OF_DAY))

##mean occupancy rate by Street block, weekday, time of day (no more Nan values)
avgocc_time_weekday_block <- dataAMJ %>%
  group_by(dataAMJ$STREET_BLOCK, dataAMJ$WEEKDAY, dataAMJ$TIME_OF_DAY) %>%
  summarise(mean(OCC_RATE))

names(avgocc_time_weekday_block) <- c("STREET_BLOCK","WEEKDAY","TIME_OF_DAY","OCC_RATE")


##mean occupancy by day of week and time of day (excluding nan values)
avgocc_time_weekday <- dataAMJ %>%
  group_by(dataAMJ$TIME_OF_DAY, dataAMJ$WEEKDAY) %>%
  summarise(mean(OCC_RATE))

names(avgocc_time_weekday) <- c("time","weekday","average_occ")
avgocc_time_weekday$DATE <- NA

avgocc_time_weekday$time <- avgocc_time_weekday$time /100

avgocc_time_weekday$DATE <- ifelse((avgocc_time_weekday$weekday) == "Monday",
                                   "19/08/2019", avgocc_time_weekday$DATE)
avgocc_time_weekday$DATE <- ifelse((avgocc_time_weekday$weekday) == "Tuesday",
                                   "20/08/2019", avgocc_time_weekday$DATE)
avgocc_time_weekday$DATE <- ifelse((avgocc_time_weekday$weekday) == "Wednesday",
                                   "21/08/2019", avgocc_time_weekday$DATE)
avgocc_time_weekday$DATE <- ifelse((avgocc_time_weekday$weekday) == "Thursday",
                                   "22/08/2019", avgocc_time_weekday$DATE)
avgocc_time_weekday$DATE <- ifelse((avgocc_time_weekday$weekday) == "Friday",
                                   "23/08/2019", avgocc_time_weekday$DATE)
avgocc_time_weekday$DATE <- ifelse((avgocc_time_weekday$weekday) == "Saturday",
                                   "24/08/2019", avgocc_time_weekday$DATE)
avgocc_time_weekday$DATE <- ifelse((avgocc_time_weekday$weekday) == "Sunday",
                                   "25/08/2019", avgocc_time_weekday$DATE)

##save time only as daily hours (no weekday information)
avgocc_time_weekday$time_hours <- do.call(paste, 
                                        c(avgocc_time_weekday[c("time", "weekday")], 
                                          sep = " ")) 
avgocc_time_weekday$time_hours <- as.POSIXct(avgocc_time_weekday$time_hours, 
                                             format='%H %A')

##save time with weekday information (from Mo to Su)
avgocc_time_weekday$time_weekday <- do.call(paste, 
                                        c(avgocc_time_weekday[c("time", "weekday", "DATE")], 
                                          sep = " ")) 
avgocc_time_weekday$time_weekday <- as.POSIXct(avgocc_time_weekday$time_weekday, 
                                           format='%H %A %d/%m/%Y')


avgocc_time_weekday_district <- dataAMJ %>%
  group_by(TIME_OF_DAY, WEEKDAY, PM_DISTRICT_NAME) %>%
  summarise(mean(OCC_RATE))
names(avgocc_time_weekday_district) <- c("time","weekday","district_name","average_occ")

avgocc_time_weekday_district$time <- avgocc_time_weekday_district$time /100

avgocc_time_weekday_district$DATE <- NA
avgocc_time_weekday_district$time_hours <- NA

avgocc_time_weekday_district$DATE <- ifelse((avgocc_time_weekday_district$weekday) == "Monday",
                                   "19/08/2019", avgocc_time_weekday_district$DATE)
avgocc_time_weekday_district$DATE <- ifelse((avgocc_time_weekday_district$weekday) == "Tuesday",
                                   "20/08/2019", avgocc_time_weekday_district$DATE)
avgocc_time_weekday_district$DATE <- ifelse((avgocc_time_weekday_district$weekday) == "Wednesday",
                                   "21/08/2019", avgocc_time_weekday_district$DATE)
avgocc_time_weekday_district$DATE <- ifelse((avgocc_time_weekday_district$weekday) == "Thursday",
                                   "22/08/2019", avgocc_time_weekday_district$DATE)
avgocc_time_weekday_district$DATE <- ifelse((avgocc_time_weekday_district$weekday) == "Friday",
                                   "23/08/2019", avgocc_time_weekday_district$DATE)
avgocc_time_weekday_district$DATE <- ifelse((avgocc_time_weekday_district$weekday) == "Saturday",
                                   "24/08/2019", avgocc_time_weekday_district$DATE)
avgocc_time_weekday_district$DATE <- ifelse((avgocc_time_weekday_district$weekday) == "Sunday",
                                   "25/08/2019", avgocc_time_weekday_district$DATE)

##save time only as daily hours (no weekday information)
avgocc_time_weekday_district$time_hours <- do.call(paste, 
                                          c(avgocc_time_weekday_district[c("time", "weekday")], 
                                            sep = " ")) 
avgocc_time_weekday_district$time_hours <- as.POSIXct(avgocc_time_weekday_district$time_hours, 
                                             format='%H %A')

##save time with weekday information (from Mo to Su)
avgocc_time_weekday_district$time_weekday <- do.call(paste, 
                                            c(avgocc_time_weekday_district[c("time", "weekday", "DATE")], 
                                              sep = " ")) 
avgocc_time_weekday_district$time_weekday <- as.POSIXct(avgocc_time_weekday_district$time_weekday, 
                                               format='%H %A %d/%m/%Y')

avgocc_time_weekday_district$region <- NA

avgocc_time_weekday_district$region <- ifelse((avgocc_time_weekday_district$district_name) %in%
                                        c("Union","South Embarcadero","Downtown"),
                                      "commercial", avgocc_time_weekday_district$region)
avgocc_time_weekday_district$region <- ifelse((avgocc_time_weekday_district$district_name) %in%
                                        c("Marina","Fillmore","Civic Center","Mission",
                                          "Inner Richmond","West Portal"),
                                      "residential", avgocc_time_weekday_district$region)
avgocc_time_weekday_district$region <- ifelse((avgocc_time_weekday_district$district_name) == 
                                        "Fisherman's Wharf",
                                      "touristic", avgocc_time_weekday_district$region)


avgocc_time_weekday_region <- avgocc_time_weekday_district  %>%
  group_by(time_weekday,time_hours,weekday, region) %>%
  summarise(mean(average_occ))

names(avgocc_time_weekday_region) <- c("time_weekday","time_hours","weekday","region","average_occ")


avgocc_time_district <- avgocc_time_weekday_district  %>%
  group_by(time_hours, district_name) %>%
  summarise(mean(average_occ))

names(avgocc_time_district) <- c("time_hours","district_name","average_occ")


avgocc_time_region <- avgocc_time_weekday_region  %>%
  group_by(time_hours, region) %>%
  summarise(mean(average_occ))

names(avgocc_time_region) <- c("time_hours","region","average_occ")



###### plots



##Average occupancy for each weekday, overlain
p = ggplot(avgocc_time_weekday, aes(time_hours, average_occ, color = weekday), size = 1.5) + 
  geom_line() +
  scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M") +
  xlab('Time') +
  ylab('Occupancy') +
  theme(legend.position = "right") + 
  labs(title = "Average Daily Occupancy")
print(p)


##plot average occupancy for one week
p2 = ggplot() + 
  geom_line(data = avgocc_time_weekday, 
            aes(x = time_weekday, y = average_occ), color = "red") +
  scale_x_datetime(date_breaks = "24 hours", date_labels = "%a") +
  labs(title="Average Weekday Occupancy") +
  xlab('Weekday') +
  ylab('Occupancy')
print(p2)


##Average daily occupancy for each district (10)
p3 = ggplot(avgocc_time_district, aes(time_hours, average_occ, color = district_name),
            size = 1.5) + 
  geom_line() +
  scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M") +
  xlab('Time') +
  ylab('Occupancy') +
  theme(legend.position = "right") + 
  labs(title = "Average Daily Occupancy")
print(p3)

avgocc_time_district

##Average daily occupancy for each region (3)
p4 = ggplot(avgocc_time_region, aes(time_hours, average_occ, color = region),
            size = 1.5) + 
  geom_line() +
  scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M") +
  xlab('Time') +
  ylab('Occupancy') +
  theme(legend.position = "right") + 
  labs(title = "Average Daily Occupancy")
print(p4)



##Average occupancy for each weekday and district (10)
p5 = ggplot(avgocc_time_weekday_district, aes(time_hours, average_occ, color = district_name),
            size = 1.5) + 
  geom_point() +
  scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M") +
  xlab('Time') +
  ylab('Occupancy') +
  theme(legend.position = "right") + 
  labs(title = "Average Weekday Occupancy")
print(p5)


##Average occupancy for each weekday and region (3)
p6 = ggplot() + 
  geom_line(data = avgocc_time_weekday_region, 
            aes(x = time_weekday,y = average_occ, group=region, color = region)) +
  scale_colour_brewer(palette = "Paired") +
  scale_x_datetime(date_breaks = "24 hours", date_labels = "%a") +
  xlab('Weekday') +
  ylab('Average occupancy') +
  labs(title = "Weekly Parking Occupancy")
print(p6)


###example of Occupancy rate
segment10203 <- dataAMJ[dataAMJ$BLOCK_ID==10203,]
segment10203 <- segment10203[segment10203$CAL_DATE>="2011-04-11" & 
                               segment10203$CAL_DATE<="2011-04-17",]


ggplot(segment10203, aes(START_TIME_DT, OCC_RATE)) + 
  geom_line(color='#3182bd',size=1) +
  theme_bw()+
  scale_x_datetime(breaks = seq(as.POSIXct("2011-04-11 00:00:00"),
                                as.POSIXct("2011-04-14 00:00:00"), "24 hours"),
                   date_labels = "%d-%m-%Y",
                   expand = c(0, 0),
                   limits = c(
                     as.POSIXct("2011-04-11 00:00:00"),
                     as.POSIXct("2011-04-14 00:00:00"))) +
  scale_y_continuous(limits=c(0.2,1.0),breaks=seq(0.2,1.0,0.2),expand = c(0,0)) + 
  xlab('Date') +
  ylab('Occupancy rate')+
  theme(legend.position = "right",axis.text.x = element_text(angle = 30,hjust = 1),
        plot.margin=unit(c(.5,.5,.5,.5),"cm"))

#############

###example of Occupancy rate
segment61202 <- dataAMJ[dataAMJ$BLOCK_ID==61202,]
segment61202 <- segment61202[segment61202$CAL_DATE>="2011-04-11" & 
                               segment61202$CAL_DATE<="2011-04-30",]


ggplot(segment61202, aes(START_TIME_DT, OCC_RATE)) + 
  geom_line(color='#3182bd',size=1) +
  theme_linedraw() +
  scale_x_datetime(breaks = seq(as.POSIXct("2011-04-11 00:00:00"),
                                as.POSIXct("2011-04-14 00:00:00"), "24 hours"),
                   date_labels = "%d-%m-%Y",
                   expand = c(0, 0),
                   limits = c(
                     as.POSIXct("2011-04-11 00:00:00"),
                     as.POSIXct("2011-04-14 00:00:00"))) +
  #scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8,1))+
  ylim(c(0,1))+
  xlab('Date') +
  ylab('Occupancy rate')+
  theme(legend.position = "right",axis.text.x = element_text(angle = 30,hjust = 1),
        plot.margin=unit(c(.5,.3,.3,.5),"cm"))


################
###example of Occupancy rate
segment46405 <- dataAMJ[dataAMJ$BLOCK_ID==46405,]
segment46405 <- segment46405[segment46405$CAL_DATE>="2011-04-11" & 
                               segment46405$CAL_DATE<="2011-04-30",]


ggplot() + 
  geom_line(data=segment61202, aes(START_TIME_DT, OCC_RATE),
            color='#4daf4a',size=1,alpha=0.9) +
  geom_line(data=segment46405, aes(START_TIME_DT, OCC_RATE),
            color='#377eb8',size=1,alpha=0.9) +
  theme_bw() +
  scale_x_datetime(breaks = seq(as.POSIXct("2011-04-11 00:00:00"),
                                as.POSIXct("2011-04-25 00:00:00"), "48 hours"),
                   date_labels = "%d-%m-%Y",
                   expand = c(0, 0),
                   limits = c(
                     as.POSIXct("2011-04-11 00:00:00"),
                     as.POSIXct("2011-04-25 00:00:00"))) +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),expand = c(0.01,0.01)) + 
  xlab('Date') +
  ylab('Occupancy rate')+
  theme(legend.position = "right",axis.text.x = element_text(angle = 30,hjust = 1),
        plot.margin=unit(c(.5,.3,.3,.5),"cm"))




###join average occ values to occ values with NaN
# dataAMJcopy <- dataAMJ
# dataAMJcopy <- dataAMJcopy %>%
#   left_join(avgocc_time_weekday_block, by = c("STREET_BLOCK","WEEKDAY","TIME_OF_DAY")) %>%
#   mutate(OCC_RATE = ifelse(is.nan(OCC_RATE.x), OCC_RATE.y, OCC_RATE.x))


# #read in on-street parking census data
# onstreetcensus <- read.csv("onstreet_parking_census_capacity_2014.csv", header = TRUE, sep=';')
# 
# dataAMJ %>% 
#   group_by(dataAMJ$STREET_BLOCK) %>%
#   summarise(no_rows = length(STREET_BLOCK))
# 
# ##create new column to  ST_NAME and ST_TYPE
# onstreetcensus$NEW_STREET <- do.call(paste, c(onstreetcensus[c("ST_NAME", "ST_TYPE")], sep = " "))
# 
# onstreetcensus %>% 
#   group_by(onstreetcensus$NEW_STREET) %>%
#   summarise(no_rows = length(NEW_STREET))
# ##copy of onstreetcensus
# osc_copy <- onstreetcensus
# osc_copy <- within(osc_copy, rm("FID_", "CNN", "ST_TYPE", "YEAR"))
# ##aggregate Streets by name and summarize parking supply
# ##create new df
# parking_supply <- data.frame(aggregate(PRKNG_SPLY ~ NEW_STREET, data=osc_copy, sum))



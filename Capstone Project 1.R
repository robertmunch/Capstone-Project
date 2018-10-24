library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(ggfortify)
library(fpp2)


PrecipData <- read_csv("~/R/Capstone Project/PrecipData.txt", 
                      comment = "#")

noaa.co2 <- read_table2("~/R/Capstone Project/noaa.co2.txt", 
                        col_names = FALSE, comment = "#")

#nameing columns for noaa.co2
colnames(noaa.co2) <- c("Year", "Month", "Decimal Date", "Average.ppm", "Interpolated.ppm", "Trend.ppm", "#Days")

#removing data from outside the SWE data
noaa.co2 <- noaa.co2 %>% 
  filter(Year >= 1980 & Year <= 2017)
  
#Removeing excess row caused by data transfer
PrecipData <- PrecipData[-c(1),]
#changing the column names as to later be more easily organized by number
colnames(PrecipData) <- c("Station.Id", "Station.Name", "Year", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
#Changing the month, year and SWE.mm values to numerics, which requires them to be unlisted. 
#PrecipData[, 3:15] <- as.numeric(unlist(PrecipData[, 3:15]))
#Removing the rows that contain incomplete data and placing all the month values into their own column
PrecipData <- PrecipData %>%
  na.omit(PrecipData) %>% 
  gather("Month", "SWE.mm", 4:15)
#Changing the year, month and SWE.mm values to numerials which requires them to be unlisted. 
PrecipData[, c(3,5)] <- as.numeric(unlist(PrecipData[, c(3,5)]))
#Filtering the data to be between 1980 and 2017, the earliest and latest years there is complete data.
PrecipData <- PrecipData %>% 
  filter(Year >= 1980 & Year <= 2017) 

#Create Data table that combines month and year into one column as to aggreage by station.
PrecipData.combine.date <- PrecipData %>% 
  unite("Date", "Year", "Month", sep = "-") %>% 
  arrange(Date)

#Aggregate of Average SWE for each month across all stations and name columns
station.agg <- aggregate(PrecipData.combine.date$SWE.mm, list(PrecipData.combine.date$Date), mean)
colnames(station.agg) <- c("Date", "SWE.mm.Avg")

#Combine station.agg and noaa.co2 as a way to compare co2 ppm and SWE 
combine.data <- bind_cols(station.agg, noaa.co2)
combine.data <- combine.data %>% 
  select("Year", "Month", "SWE.mm.Avg", "Interpolated.ppm") 

#time series of SWE avg station.agg
#ts.SWE <- ts(data = station.agg$SWE.mm.Avg, start = 1980, frequency = 12)

#time series of CO2 ppm from noaa.co2
#ts.co2 <- ts(data = noaa.co2$Interpolated.ppm, start = 1980, frequency = 12)

#attempt at making the ppm and SWE into a timeseries matrix
#ts.combine.data <- combine.data %>%
 # select("SWE.mm.Avg", "Interpolated.ppm")
  #ts(start = 1980, frequency = 12)

#Coorelating SWE and co2 values as dot plot to consider linear relationship
ggplot(combine.data, aes(x = Year, y = SWE.mm.Avg)) +
  geom_boxplot(aes(group = Year)) +
  geom_smooth(method = "lm", se = FALSE, aes(color ="red")) +
  labs(y = "Snow Water Equivalent (SWE) across CO") +
  theme(legend.position="none")

ggplot(combine.data, aes(x = Year)) +
  geom_histogram(position = "stack", bins = 38, aes(group = SWE.mm.Avg, fill = SWE.mm.Avg)) +
  labs(y = "Snow Water Equivalent (SWE) count")
  
#Splines
#fit <- lm(SWE.mm.Avg ~ bs(Date, knots = c(5,18,29)),data = station.agg)
#summary(fit)

#ggplot(station.agg, aes(x = Date, y = SWE.mm.Avg)) +
  #geom_line()

#messing around with bs()
#bs <- bs(station.agg$SWE.mm.Avg, df = 4)
#autoplot(bs)

#fit <- lm(bs(station.agg$SWE.mm.Avg, df = 4) ~ Date,data = station.agg)
#summary(fit)

#to simplify the data, I'm attempting aggregate all months into a yearly SWE average.
SWE.year.agg <- aggregate(PrecipData$SWE.mm, list(PrecipData$Year), mean)
colnames(SWE.year.agg) <- c("Year", "SWE.mm.Avg")

#aggregting average co2 ppm by year
co2.year.agg <- aggregate(noaa.co2$Interpolated.ppm, list(noaa.co2$Year), mean)
colnames(co2.year.agg) <- c("Year", "co2.ppm.Avg")

#combining the yearly average points in an attempt to make a coorelation
SWE.co2.year.agg <- SWE.year.agg %>%
  bind_cols(co2.year.agg) %>%
  select(c(1,2,4))

ggplot(SWE.co2.year.agg, aes(x = co2.ppm.Avg, y = SWE.mm.Avg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(x = "Average annual atmospheric CO2 (ppm)", y = "Average annual SWE (mm)") +
  theme(legend.position="none")

lm.year.avg <- lm(SWE.mm.Avg ~ co2.ppm.Avg, data = SWE.co2.year.agg)
summary(lm.year.avg)
checkresiduals(lm.year.avg)

#make a time series out of SWE.co2.year.agg as to use a tslm
ts.combine.data.avg <- SWE.co2.year.agg %>% 
  select(c(2,3)) %>% 
  ts(start = 1980)

tslm.combine.data.avg <- tslm(SWE.mm.Avg ~ co2.ppm.Avg, data = ts.combine.data.avg)
summary(tslm.combine.data.avg)

checkresiduals(tslm.combine.data.avg)

#playing around with snaive forcasting nothing promsing as it only goes back one year.
ts.SWE <- ts(combine.data3$SWE.mm.Avg, start = 1980, frequency = 12)
snaive(ts.SWE)
autoplot(snaive(ts.SWE))
rwf(ts.SWE, drift=TRUE)
autoplot(rwf(ts.SWE, drift=TRUE))


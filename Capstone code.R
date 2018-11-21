library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(ggfortify)
library(fpp2)
library(GGally)

PrecipData <- read_csv("~/R/Capstone Project/PrecipData.txt", 
                       comment = "#")

#Removing the rows that contain incomplete data since giving them zeros would affect the final data.
#Numbering the months rather than names, and placing all the month values into their own column.
#Renaming station id and station name to be without spaces.
#filtering to only years 1980 to 2017, the first and last years with complete years worth of data.
#arranging by year to appear chronological.
PrecipData <- PrecipData %>%
  na.omit(PrecipData) %>%
  rename("01" = Jan, "02" = Feb , "03" = Mar, "04" = Apr, "05" = May, "06" = Jun, "07" = Jul, "08" = Aug, "09" = Sep, "10" = Oct, "11" = Nov, "12" = Dec) %>%
  rename(Station.Id = "Station Id", Station.Name = "Station Name") %>% 
  gather("Month", "SWE.mm", 4:15) %>% 
  filter(Year >= 1980 & Year <= 2017) %>%
  arrange(Year)

#Aggregating SWE data by Year, rewuiring SWE and Year to be numerics.
#Renaming columns, as names are lost in aggregating process.
SWE.year.agg <- aggregate(as.numeric(PrecipData$SWE.mm), list(as.numeric(PrecipData$Year)), mean)
colnames(SWE.year.agg) <- c("Year", "SWE.mm.Avg")

#Aggregating SWE data by month and Year, allowing data to be aggregated across all stations.
PrecipData.combine.date <- PrecipData %>% 
  unite("Date", "Year", "Month", sep = "-") %>% 
  arrange(Date)
PrecipData.station.agg <- aggregate(as.numeric(PrecipData.combine.date$SWE.mm), list(PrecipData.combine.date$Date), mean)
colnames(PrecipData.station.agg) <- c("Date", "SWE.mm")
PrecipData.station.agg <- separate(PrecipData.station.agg, "Date", c("Year", "Month"), sep = "-")
lm(SWE.mm.Avg ~ Year, SWE.year.agg)

#Plotting SWE over time
ggplot(SWE.year.agg, aes(x = Year, y = SWE.mm.Avg)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, aes(colour = "red")) +
  labs(y = "Snow Water Equivalent Annual Average", title = "Colorado Average Snow Water Equivlent Over Time") +
  theme(legend.position = "none")

#Summary of the linear model for SWE over time
summary(lm(SWE.mm.Avg ~ Year, SWE.year.agg))

#Residuals for the linear model of SWE over time
checkresiduals(lm(SWE.mm.Avg ~ Year, SWE.year.agg))

# Making vector PrecipData.1981, has only stations available in 1981
PrecipData.1981 <- PrecipData %>% 
  filter(Year == 1981) %>% 
  arrange(Station.Id) %>% 
  distinct(Station.Id) %>% 
  as.vector()

#making data frame that has data for stations only available in 1981. 
PrecipData.outlier <- PrecipData %>% 
  filter(Station.Id %in% unlist(PrecipData.1981))

#Aggregating annual data for PrecipData.outlier
PrecipData.outlier <- aggregate(as.numeric(PrecipData.outlier$SWE.mm), list(as.numeric(PrecipData.outlier$Year)), FUN = mean)  
colnames(PrecipData.outlier) <- c("Year", "SWE.mm.Avg.outlier")

#combining precipdata.outlier and noraml data to chart
PrecipData.outlier <- PrecipData.outlier %>%   
  bind_cols(SWE.year.agg) %>% 
  select(Year, SWE.mm.Avg.outlier, SWE.mm.Avg)

#chart comparing only 1981 stations and all stations data 
ggplot(PrecipData.outlier, aes(x = Year)) +
  geom_point(aes(y = SWE.mm.Avg.outlier, colour = "red")) +
  geom_smooth(method = "lm", se = FALSE, aes(y = SWE.mm.Avg.outlier, colour = "red")) +
  geom_point(aes(y = SWE.mm.Avg, colour = "blue")) +
  geom_smooth(method = "lm", se = FALSE, aes(y = SWE.mm.Avg, colour = "blue")) +
  labs(x = "Year", y = "Average annual SWE (mm)") +
  scale_colour_discrete(name = "", labels=c("Treatment", "Control"))

#chi squared test to show correlation between data for 1981 and normal data.
chisq.test(PrecipData.outlier$SWE.mm.Avg.outlier, PrecipData.outlier$SWE.mm.Avg)


#Importing atmospheric CO2 data from NOAA.
noaa.co2 <- read_table2("~/R/Capstone Project/noaa.co2.txt", 
                        col_names = FALSE, comment = "#")

#Naming columns that will be used.
#Removing data from outside the SWE data timeline.
noaa.co2 <- noaa.co2 %>% 
  rename(Year = X1, Month = X2, Interpolated.ppm = X5) %>% 
  filter(Year >= 1980 & Year <= 2017)

#Combine PrecipData.station.agg and noaa.co2 as a way to compare co2 ppm and SWE 
combine.data <- bind_cols(PrecipData.station.agg, noaa.co2) %>% 
  select("Year", "Month", "SWE.mm", "Interpolated.ppm")

#aggregting average co2 ppm by year
co2.year.agg <- aggregate(noaa.co2$Interpolated.ppm, list(noaa.co2$Year), mean)
colnames(co2.year.agg) <- c("Year", "co2.ppm.Avg")

#graphng co2 data over time
ggplot(co2.year.agg, aes(x = Year, y = co2.ppm.Avg)) +
  geom_line() +
  labs(y = "Carbon Dioxide Concentration (ppm)", title = "Annual Atmospheric Carbon Dioxide Concentration (ppm)")
summary(lm(co2.ppm.Avg ~ Year, co2.year.agg))

#combing CO2 and SWE data onto the same dta frame
SWE.co2.year.agg <- SWE.year.agg %>%
  bind_cols(co2.year.agg) %>%
  select(c(1,2,4))

#linear model of SWE and CO2 data.
lm.year.avg <- lm(SWE.mm.Avg ~ co2.ppm.Avg, data = SWE.co2.year.agg)

#Plot comparing SWE and CO2 data
ggplot(SWE.co2.year.agg, aes(x = co2.ppm.Avg, y = SWE.mm.Avg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Average Annual SWE (mm) and Carbon Dioxide Concentration (ppm)", x = "Average annual atmospheric CO2 (ppm)", y = "Average annual SWE (mm)") +
  theme(legend.position="none")

#summary and residuals for CO2 and SWE data
summary(lm.year.avg)
checkresiduals(lm.year.avg)
#loading ski visitor data
ski.visits <- read.csv("~/R/Capstone Project/Ski visit data.csv")

#Renaming columns
#Becasue column name was changed from Visitor(millions), value must be changed as well.
#Removing years outside data window.
colnames(ski.visits) <- c("Year", "Visitors")
ski.visits <- ski.visits %>%
  mutate(Visitors = Visitors * 1e06) %>% 
  arrange(Year) %>% 
  filter(Year >= 1980)

#Combining visitors into data table with CO2 and SWE data. 
combine.all <-cbind(SWE.co2.year.agg, ski.visits$Visitors)
colnames(combine.all) <- c("Year", "SWE.mm", "CO2.ppm", "Visitors")

#plotting ski visitors over time
ggplot(ski.visits, aes(x = Year, y = Visitors)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Visitors per year in Rocky Mountain Region") +
  theme(legend.position = "none")

#Loading tourism spending data
CO.tourism <- read_excel("CO tourism finacial data.xlsx")

#Changing tourism money to dollars
#Arranging Chronologically
CO.tourism <- CO.tourism %>% 
  mutate(Spending = Spending * 1e07) %>% 
  arrange(Year)

#Plotting tourism spending over time
ggplot(CO.tourism, aes(x = Year, y = Spending)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Tourism Spending in the Mountain Resort Area of Colorado") +
  theme(legend.position = "none")

#Because tourism spending data is only from 1996, other data must be restricted to include data from 1996 to 2017.  
combine.all.tourism <- combine.all %>% 
  arrange(Year) %>% 
  filter(Year >= 1996)

#combing all current data into one data frame and renaming cloumns
combine.all.tourism <- cbind(combine.all.tourism, CO.tourism$Spending)
colnames(combine.all.tourism) <- c("Year", "SWE.mm", "CO2.ppm", "Visitors", "Spending")

#Plotting tourism spending and visitors
ggplot(combine.all.tourism, aes(Spending, Visitors)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Spending and Tourism Data") +
  theme(legend.position = "none")

#Combining the change from one year to the next of SWE and Visitor data to plot.
#renaming columns and removing NAs
delta.all <-
  as.data.frame(
    cbind(
      combine.all$Year,
      (lead(combine.all$SWE.mm, 1) - combine.all$SWE.mm) / lead(combine.all$SWE.mm, 1),
      (lead(combine.all$Visitors, 1) - combine.all$Visitors) / lead(combine.all$Visitors, 1)))
colnames(delta.all) <- c("Year", "SWE", "Visitors")
delta.all <- na.omit(delta.all)

#PLotting delta value
ggplot(delta.all, aes(SWE, Visitors)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Annual Change of SWE and Visitor Data") +
  theme(legend.position = "none")

#plotting zoomed in delta chart
ggplot(delta.all, aes(SWE, Visitors)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Zoomed Section in Annual Change of SWE and Visitor Data") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(-0.75, 0.5), ylim = c(-0.2, 0.2))

#summary of linear model made by change over year of SWE and visitor data
summary(lm(Visitors ~ SWE, delta.all))

#ggpairs of year aggregates for SWE, CO2, Visitors
ggpairs(combine.all[,2:4])

#Loading data of CO2 emissions from 2005 to 2015 for all states and naming columns
state_CO2_emissions <- read_excel("state CO2 emisssions.xlsx", 
                                  sheet = "Sheet1", col_names = c("State", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "Change.Percent", "Change.Abs"))

state_CO2_emissions$Change.Percent <- as.numeric(state_CO2_emissions$Change.Percent)

#Cleaning up state emissions data to remove NAs and removing old title column
state_CO2_emissions <- state_CO2_emissions %>% 
  na.omit(state_CO2_emissions) %>% 
  slice(2:52) %>% 
  arrange(Change.Percent)


#plot to show COlorados high change compared to all other states
ggplot(state_CO2_emissions, aes(State, Change.Percent)) +
  geom_col(aes(fill = ifelse(State == "Colorado", "Colorado", NA))) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Change in Carbon Dioxide emissions 2005-2015")


#Importing per capita emissions data
per_capita_emissions <- read_excel("per capita emissions.xlsx", 
                                   col_names = c("State", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "Change.Percent", "Change.Abs"))
per_capita_emissions <- per_capita_emissions %>% 
  na.omit(per_capita_emissions) %>% 
  slice(2:52) %>% 
  arrange(Change.Percent)
per_capita_emissions$Change.Percent <- as.numeric(per_capita_emissions$Change.Percent)


#Showing Colorado's relative low change to other states
ggplot(per_capita_emissions, aes(State, Change.Percent)) +
  geom_col(aes(fill = ifelse(State == "Colorado", "Colorado", NA))) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(title = "Per-capita change in Carbon Dioxide Emissions 2005-2015")


#Doing same for maine to show comparison in states ranking of carbon emissions
ggplot(state_CO2_emissions, aes(State, Change.Percent)) +
  geom_col(aes(fill = ifelse(State == "Maine", "Colorado", NA))) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Change in Carbon Dioxide emissions 2005-2015")

ggplot(per_capita_emissions, aes(State, Change.Percent)) +
  geom_col(aes(fill = ifelse(State == "Maine", "Colorado", NA))) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(title = "Per-capita change in Carbon Dioxide Emissions 2005-2015")

# install required packages
# tidyverse, lubridate, ggplot

library(tidyverse)
library(lubridate)
library(ggplot2)

# displays working directory
getwd()
# set working directory to a specified file path
setwd("/Users/UCHE/Desktop/Google Analytics/Capstone Project/cyclistic_data_2021/CSV_files")

#Import data sets (csv files) here
dec_2020 <- read_csv("12_2020.csv")
jan_2021 <- read_csv("01_2021.csv")
feb_2021 <- read_csv("02_2021.csv")
mar_2021 <- read_csv("03_2021.csv")
april_2021 <- read_csv("04_2021.csv")
may_2021 <- read_csv("05_2021.csv")
june_2021 <- read_csv("06_2021.csv")
july_2021 <- read_csv("07_2021.csv")
aug_2021 <- read_csv("08_2021.csv")
sept_2021 <- read_csv("09_2021.csv")

#Inspecting the data frames
str(dec_2020) #same function was used for other months

#Joining all data frames
all_trips <- bind_rows(dec_2020,jan_2021,feb_2021,mar_2021,april_2021,may_2021,june_2021,july_2021,aug_2021,sept_2021)

#View Data
View(all_trips)

# Create new data frame by selecting columns of interests
all_trips<- all_trips %>%
  select(ride_id, rideable_type, started_at,ended_at,member_casual)

#Clean up and add data to prepare for analysis
colnames(all_trips) #List of column names
nrow(all_trips) #Number of rows
dim(all_trips) #Dimensions of data frames
head(all_trips) #See the first 6 rows of data frame
str(all_trips) #See list of columns and data types
summary(all_trips) #Statistical summary of data. Mainly for numeric data

#Add columns that list date, month, day and year of each ride
all_trips$date<-as.Date(all_trips$started_at)
all_trips$month<-format(as.Date(all_trips$date),"%m")
all_trips$day<-format(as.Date(all_trips$date),"%d")
all_trips$year<-format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week<-format(as.Date(all_trips$date),"%A")

#Add a "ride_length" calculation to all_trips data frame (in seconds)
all_trips$ride_length<- difftime(all_trips$ended_at,all_trips$started_at)

View(all_trips)

#Convert "ride_length: from Factor to numeric
is.factor(all_trips$ride_length)
all_trips$ride_length<-as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#Remove "bad" data i.e ride_length rows with negative values
# We create a new data frame by excluding rows with negative ride_length
all_trips_v2 <- all_trips %>%
  filter(ride_length > 0)

# Number of rows with negative ride_length
nrow(all_trips) - nrow(all_trips_v2)

#View the new data frame
View(all_trips_v2)

# Inspecting and confirming that new data frame all_trips_v2 has no negative ride_length
all_trips_v2%>%
  filter(ride_length<0)

summary(all_trips_v2)

# Descriptive Analysis
# mean_ride_length for casual and annual membership
all_trips_v2 %>% group_by(member_casual)%>%summarize(mean_ride_length=mean(ride_length))

# Overall ride_length statistics
summary(all_trips_v2$ride_length)

# mean_ride_length for casual and annual membership for each month
all_trips_v2%>%group_by(month,member_casual)%>%summarize(mean_ride_length=mean(ride_length))

#mean ride_length for each day of the week and for casual and annual membership
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual+all_trips_v2$day_of_week,FUN=mean)

#Arrange day_of_week in the correct order
all_trips_v2$day_of_week<-ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Making Plots
# make a plot of number of rides for each day of the week
all_trips_v2%>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  group_by(member_casual,day_of_week)%>%
  summarize(number_ride=n(), mean_duration=mean(ride_length))%>%
  ggplot(aes(x=day_of_week,y=number_ride,fill=member_casual))+geom_col(position="dodge")

# make a plot of average ride_length for each day of the week
all_trips_v2 %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)%>%
  ggplot(aes(x=day_of_week,y=average_duration,fill=member_casual))+geom_col(position="dodge")

# make a plot of average ride_length for each month
all_trips_v2%>%
  group_by(member_casual,month)%>%
  summarize(average_ride_length=mean(ride_length))%>%
  arrange(member_casual,month)%>%
  ggplot(aes(x=month,y=average_ride_length,fill=member_casual))+geom_col(position="dodge")

# make a plot of number of rides for each month
all_trips_v2%>%
  group_by(member_casual,month)%>%
  summarize(number_of_rides=n())%>%
  arrange(member_casual,month)%>%
  ggplot(aes(x=month,y=number_of_rides,fill=member_casual))+geom_col(position="dodge")

# Export Summary file for further Analysis on Excel or Tableau
# Average ride length for casual and members for each day of the week
ride_length_day_of_week<-all_trips_v2%>%
  group_by(member_casual,day_of_week)%>%
  summarize(average_ride_length=mean(ride_length))%>%
  arrange(member_casual,day_of_week)

# Average ride length for casual and members for each month
ride_length_month<-all_trips_v2%>%
  group_by(member_casual,month)%>%
  summarize(average_ride_length=mean(ride_length))%>%
  arrange(member_casual,month)

# Average number of rides for casual and members for each day of the week
number_of_ride_day_of_week<-all_trips_v2%>%
  group_by(member_casual,day_of_week)%>%
  summarize(number_of_ride=n())%>%
  arrange(member_casual,day_of_week)

# Average ride length for casual and members for each month
number_of_ride_month<-all_trips_v2%>%
  group_by(member_casual,month)%>%
  summarize(number_of_ride=n())%>%
  arrange(member_casual,month)

#Viewing each data frame
View(ride_length_month)
View(ride_length_day_of_week)
View(number_of_ride_month)
View(number_of_ride_day_of_week)

# Exporting data frame
write.csv(ride_length_day_of_week,file='C:/Users/UCHE/Desktop/Google Analytics/Capstone Project/cyclistic_data_2021/CSV_files/ride_length_day_of_week.csv')
write.csv(ride_length_month,file='C:/Users/UCHE/Desktop/Google Analytics/Capstone Project/cyclistic_data_2021/CSV_files/ride_length_month.csv')
write.csv(number_of_ride_day_of_week,file='C:/Users/UCHE/Desktop/Google Analytics/Capstone Project/cyclistic_data_2021/CSV_files/number_of_ride_day_of_week.csv')
write.csv(number_of_ride_month,file='C:/Users/UCHE/Desktop/Google Analytics/Capstone Project/cyclistic_data_2021/CSV_files/number_of_ride_month.csv')


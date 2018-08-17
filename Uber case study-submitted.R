library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(gridExtra)
##########download data
uber<-read.csv("Uber Request Data.csv")

########data cleaning and creation of new columns for date time
sum(is.na(uber$Request.timestamp))
sum(is.na(uber$Drop.timestamp)) # 58% of observations have NA values
uber$request_time1<-dmy_hm(uber$Request.timestamp)
uber$request_time2<-dmy_hms(uber$Request.timestamp)
uber$request_time<-ifelse(is.na(uber$request_time1),uber$request_time2,uber$request_time1) # combining above two columns into a single column
uber$request_time<-as.POSIXct(uber$request_time, tz="IST", origin = "1970-01-01 00:00:00")
#uber$request_time3<-as.POSIXct(uber$request_time, tz="Asia/Calcutta", origin = "1970-01-01 00:00:00")
uber$req_yr<-format(uber$request_time,"%Y")
uber$req_month<-format(uber$request_time,"%m")
uber$req_day<-format(uber$request_time,"%d")
uber$req_hr<-format(uber$request_time,"%H")
uber$req_weekday<-weekdays(uber$request_time)
uber$req_hr_n<-as.numeric(uber$req_hr)
# creation of time slots
uber$early_morning<-ifelse(uber$req_hr_n<=4,1,0)
uber$morning<-ifelse(uber$req_hr_n>4 & uber$req_hr_n<=9,1,0)
uber$late_morning_afternoon<-ifelse(uber$req_hr_n>9 & uber$req_hr_n<=16,1,0)
uber$evening<-ifelse(uber$req_hr_n>16 & uber$req_hr_n<=21,1,0)
uber$night<-ifelse(uber$req_hr_n>21,1,0)
uber1 <- gather(uber, time,my_val, early_morning:night)
uber1 <- uber1[!(uber1$my_val == 0),]
uber<-uber1
ggplot(uber,aes(req_hr,fill=time))+geom_histogram(stat = "count")

uber$drop_time1<-dmy_hm(uber$Drop.timestamp)
uber$drop_time2<-dmy_hms(uber$Drop.timestamp)
uber$drop_time<-ifelse(is.na(uber$drop_time1),uber$drop_time2,uber$drop_time1)
uber$drop_time<-as.POSIXct(uber$drop_time, tz="IST", origin = "1970-01-01 00:00:00")
uber$drop_yr<-format(uber$drop_time,"%Y")
uber$drop_month<-format(uber$drop_time,"%m")
uber$drop_day<-format(uber$drop_time,"%d")
uber$drop_hr<-format(uber$drop_time,"%H")

#Understand the data and basic univariate analysis
str(uber)
summary(factor(uber$Driver.id))
summary(factor(uber$req_weekday))
summary(factor(uber$req_yr))
###### is trip duration a factor
unique(uber$Request.id)
summary(uber$Pickup.point)
uber$trip_time<-difftime(uber$drop_time,uber$request_time,units = "mins")
uber$trip_time<-as.numeric(uber$trip_time)
uber$trip_time<-round(uber$trip_time,1)
ggplot(uber,aes(x=trip_time))+geom_histogram(binwidth = 1, fill="blue")+ggtitle("Distribution of length of trips in mins")
ggplot(uber,aes(x=trip_time, fill=Pickup.point))+geom_histogram(binwidth = 1) # shows marning message because 3914 rows are NA-incomplete calls
ggplot(uber,aes(x=trip_time, fill=time))+geom_histogram(binwidth = 1)

hr_gp<-group_by(uber,req_hr)
hr_trip_time<-summarise(hr_gp,Avg_time=mean(trip_time,na.rm = TRUE))
par(mfrow=c(2,1))
hist(uber$trip_time, xlab="Time/Trip",ylab="Frequency",main="Histogram of time taken per trip",col = 26 )
boxplot(trip_time~req_hr,hr_gp,xlab="Time of Day",ylab="Trip Time",main="Impact of time of day on Trip time")

#table(factor(uber$Driver.id),uber$Status)
driver<-table(factor(uber$Driver.id))
summary(factor(uber$Driver.id))
plot(driver)

###############weekday
plot1<-ggplot(uber,aes(x=factor(req_weekday),fill=Status))+geom_bar() +ggtitle("Status of requests vs Weekday")# no data for Saturday or Sunday
plot2<-ggplot(uber,aes(x=factor(req_weekday),fill=time))+geom_bar(position = "fill") +ggtitle(" Weekday vs. time of day")# timing of requests is largely same across days of week
############### time of day
ggplot(uber,aes(x=time, fill=Status))+geom_histogram(stat = "count")
plot3<-ggplot(uber,aes(x=time))+geom_histogram(stat = "count", fill="blue")+ggtitle("Distribution of requests during time of day") # distribution of requests during time of day

grid.arrange(plot1,plot2,plot3)


# frequency of cancellation
class(uber$Status)
summary(uber$Status)
length(unique(uber$Request.id))
ggplot(uber,aes(Status))+geom_histogram(stat="count", fill="blue")+ggtitle(" Completed vs. non completed requests")

summary(uber$Status)
ggplot(uber,aes(x=Pickup.point,fill=Status))+geom_bar()
##### plot of requests that get cancelled or show "no cars available"
ggplot(uber,aes(x=Pickup.point,fill=Status))+geom_bar(position = "fill")+ggtitle("Request status vs Route")
########plot of problematic time slots
ggplot(uber,aes(x=factor(req_hr),fill=Status))+geom_histogram(stat = "count")
ggplot(uber,aes(x=factor(req_hr),fill=Status))+geom_bar(position = "stack")+ggtitle("Request status vs. time of day")

############# time of route
ggplot(uber,aes(x=factor(req_hr),fill=Pickup.point))+geom_bar() +ggtitle("Pickup point vs. time of day")



##########status of requests based on pickup point and hours
uber_airport<-subset(uber,uber$Pickup.point=="Airport")
uber_city<-subset(uber,uber$Pickup.point=="City")
ggplot(uber_airport,aes(x=factor(req_hr),fill=Status))+geom_bar(position = "stack")
ggplot(uber_city,aes(x=factor(req_hr),fill=Status))+geom_bar(position = "stack")

##############net demand on hrly basis and calculation of demand supply gap
uber$cancelled<-ifelse(uber$Status=="Cancelled",1,0)
uber$no_cars_available<-ifelse(uber$Status=="No Cars Available",1,0)
uber$trip_completed<-ifelse(uber$Status=="Trip Completed",1,0)
demand<-group_by(uber,req_hr)
dem_sup<-summarise(demand,cancelled_trip=sum(cancelled),not_aval_trip=sum(no_cars_available),completed_trip=sum(trip_completed))
dem_sup$high_demand<-dem_sup$not_aval_trip+dem_sup$cancelled_trip # aggregate demand supply gap
ggplot(dem_sup,aes(x=req_hr,y=high_demand))+geom_col(fill="green")
write.csv(dem_sup,"demand_supply_aggregate.csv")
write.csv(uber,"uber_final2.csv")
##############net demand on hrly basis and calculation of demand supply gap for different routes
##### airport to city
uber_airport$cancelled<-ifelse(uber_airport$Status=="Cancelled",1,0)
uber_airport$no_cars_available<-ifelse(uber_airport$Status=="No Cars Available",1,0)
uber_airport$trip_completed<-ifelse(uber_airport$Status=="Trip Completed",1,0)
demand<-group_by(uber_airport,req_hr)
dem_sup_air<-summarise(demand,cancelled_trip=sum(cancelled),not_aval_trip=sum(no_cars_available),completed_trip=sum(trip_completed))
dem_sup_air$high_demand<-dem_sup_air$not_aval_trip+dem_sup_air$cancelled_trip # demand supply gap from trips to Airport
ggplot(dem_sup_air,aes(x=req_hr,y=high_demand))+geom_col(fill="red")
write.csv(dem_sup_air,"demand_supply_airport.csv")

##### city to airport
uber_city$cancelled<-ifelse(uber_city$Status=="Cancelled",1,0)
uber_city$no_cars_available<-ifelse(uber_city$Status=="No Cars Available",1,0)
uber_city$trip_completed<-ifelse(uber_city$Status=="Trip Completed",1,0)
demand<-group_by(uber_city,req_hr)
dem_sup_city<-summarise(demand,cancelled_trip=sum(cancelled),not_aval_trip=sum(no_cars_available),completed_trip=sum(trip_completed))
dem_sup_city$high_demand<-dem_sup_city$not_aval_trip+dem_sup$cancelled_trip # demand supply gap for trips to Airport
ggplot(dem_sup_city,aes(x=req_hr,y=high_demand))+geom_col(fill="blue")
write.csv(dem_sup_city,"demand_supply_city.csv")




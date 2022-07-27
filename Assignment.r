data<-read.table("Group_Assignment_1_Dataset.txt",header = T,sep = ",")
library(lubridate)

d1<-ymd("2007-01-01")
as.interval(dweeks(16), start=d1)

as.interval(dweeks(17), start=d1)

int<-interval(as.POSIXct("2007-04-23"),as.POSIXct("2007-04-29"))
# Week 17 is April 23 to April 29
data_17<-data[as.POSIXct(data$Date,format="%d/%m/%Y")%within%int,]
# Extract the 17th week data

# Arithmetic mean, the median, the mode and the standard deviation
summary <- function(x) {
funs <- c(mean, median, sd)
sapply(funs, function(f) f(x, na.rm = TRUE))
}
ABC<-data_17[,3:5]
round(vapply(ABC,summary,
FUN.VALUE=c(mean=0,median=0,sd=0)),3)
       
# the mode of A B C
# install.packages("prettyR")
library(prettyR)
A<-data_17[,3]
B<-data_17[,4]
C<-data_17[,5]
Mode(A,na.rm = TRUE)
Mode(B,na.rm = TRUE)
Mode(C,na.rm = TRUE)

# Geometric mean
gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
    if(any(x < 0, na.rm = TRUE)){
        return(NaN)
    }
    if(zero.propagate){
        if(any(x == 0, na.rm = TRUE)){
            return(0)
        }
        exp(mean(log(x), na.rm = na.rm))
    } else {
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    }
}
gm_mean(A)
gm_mean(B)
gm_mean(C)

library(tidyverse)

library(hms)

# Make it daytime from 8:00 to 20:00
Time<-factor(data_17$Time)
hour=format(as.POSIXct(Time,format="%H:%M:%S"),"%H")
hour<-as.numeric(hour) # This is the data time, only consider the hour
day_vec<-(hour>=8)&(hour<20)
day_17<-data_17[day_vec,]

night_vec<-(hour>=20)|(hour<8)  # night hour
night_17<-data_17[night_vec,]   # day hour

# day hour
# weekdays
int_week<-interval(as.POSIXct("2007-04-23"),as.POSIXct("2007-04-27"))
dayweek<-day_17[as.POSIXct(day_17$Date,format="%d/%m/%Y")%within%int_week,]  # Data for daytime on weekdays
# weekend
int_weekend<-interval(as.POSIXct("2007-04-28"),as.POSIXct("2007-04-29"))   # Data for weekend night 
daywend<-day_17[as.POSIXct(day_17$Date,format="%d/%m/%Y")%within%int_weekend,]

# features A and B compute the min and max values of day hour + weekdays
# A
min(dayweek$Global_active_power)
max(dayweek$Global_active_power)
#B
min(dayweek$Global_reactive_power)
max(dayweek$Global_reactive_power)

# features A and B compute the min and max values of day hour + weekend
# A
min(daywend$Global_active_power)
max(daywend$Global_active_power)
#B
min(daywend$Global_reactive_power)
max(daywend$Global_reactive_power)

# night hour
# weekdays
int_week<-interval(as.POSIXct("2007-04-23"),as.POSIXct("2007-04-27"))  
nightweek<-night_17[as.POSIXct(night_17$Date,format="%d/%m/%Y")%within%int_week,]  # Data for weekday nights
# weekend
int_weekend<-interval(as.POSIXct("2007-04-28"),as.POSIXct("2007-04-29"))  
nightwend<-night_17[as.POSIXct(night_17$Date,format="%d/%m/%Y")%within%int_weekend,]   # Data for weekend nights

# features A and B compute the min and max values of night hour + weekdays
# A
min(nightweek$Global_active_power)
max(nightweek$Global_active_power)
#B
min(nightweek$Global_reactive_power)
max(nightweek$Global_reactive_power)

# features A and B compute the min and max values of night hour + weekend
# A
min(nightwend$Global_active_power)
max(nightwend$Global_active_power)
#B
min(nightwend$Global_reactive_power)
max(nightwend$Global_reactive_power)


# install.packages("corrplot")
library(corrplot)
library(Hmisc)

ABCDEFG<-data_17[,3:ncol(data)]

rmat<-rcorr(as.matrix(ABCDEFG),type = "pearson")

corrplot(rmat$r, order = "hclust", tl.col = "black", tl.srt = 45)

library(dplyr)

ndayweek<-dayweek[,c("Date", "Time","Global_intensity"  )]
ave_week_day<-ndayweek%>%group_by(Time)%>%summarise(mean(Global_intensity,na.rm=TRUE))

# Weekend data is missing, only weekday data can be used
names(ave_week_day)<-c("Time","mean_Globalintensity")
# Average data for working day during the day
reg_data_day<-merge(dayweek,ave_week_day)

nnightweek<-nightweek[,c("Date", "Time","Global_intensity"  )]
ave_week_night<-nnightweek%>%group_by(Time)%>%summarise(mean(Global_intensity,na.rm=TRUE))
# Weekend data is missing, only weekday data can be used
names(ave_week_night)<-c("Time","mean_Globalintensity")
# Average data for working night during the day
reg_data_night<-merge(nightweek,ave_week_night) 

# Part 1
ave_week_day$Time <- as.POSIXct(ave_week_day$Time, format="%H:%M:%S")
plot(ave_week_day$Time, ave_week_day$mean_Globalintensity/1000, xaxt='n', pch=20, las=1, 
     xlab='Time',)
axis.POSIXct(1, ave_week_day$Time, format="%H:%M:%S")

# Day
library(ggplot2)
linear1<-lm(mean_Globalintensity~Global_active_power,data = reg_data_day)  # regression
ggplot(data = reg_data_day,aes(x=Global_active_power,y=mean_Globalintensity))+geom_point(alpha=.4,color="red")+geom_smooth(method="lm")

poly1<-lm(mean_Globalintensity~poly(Global_active_power,2),data = reg_data_day)
ggplot(data = reg_data_day,aes(x=Global_active_power,y=mean_Globalintensity))+geom_point(alpha=.4,color="red")+geom_smooth(method="gam",formula = y ~ x +I(x ^ 2))

linear2<-lm(mean_Globalintensity~Global_active_power,data = reg_data_night)
ggplot(data = reg_data_night,aes(x=Global_active_power,y=mean_Globalintensity))+geom_point(alpha=.4,color="red")+geom_smooth(method="lm")

linear2<-lm(mean_Globalintensity~Global_active_power,data = reg_data_night)
ggplot(data = reg_data_night,aes(x=Global_active_power,y=mean_Globalintensity))+geom_point(alpha=.4,color="red")+geom_smooth(method="lm")

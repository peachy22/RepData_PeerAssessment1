.packages = c("ggplot2","dplyr")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)
library(ggplot2)
library(dplyr)
library(lubridate)

fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,destfile="./data.zip",method="curl")
unzip('./data.zip', files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".", unzip = "internal",setTimes = FALSE)
ACT<-read.csv('activity.csv')

#What is mean total number of steps taken per day?

daysteps<-tapply(ACT$steps, ACT$date,sum,na.rm=TRUE)
daysteps_mean = mean(daysteps)
daysteps_median = median(daysteps)

png(filename="plot1.png")
hist(daysteps,breaks=10)
abline(v=daysteps_mean,col='blue')
abline(v=daysteps_median,col='red')
dev.off()

#What is the average daily activity pattern?

intsteps<-tapply(ACT$steps, ACT$interval,mean,na.rm=TRUE)
intsteps<-as.data.frame(intsteps)
intsteps <- cbind(time = rownames(intsteps), intsteps)
intsteps_max<-max(intsteps$intsteps)

png(filename="plot2.png")
plot(x=intsteps$time,y=intsteps$intsteps,type="l")
abline(h=intsteps_max,col='blue')
dev.off()

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs

ACT_na<-sum(!complete.cases(ACT))

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.

ACT_imp<-ACT
ACT_imp$steps[ACT_imp$interval == intsteps$time & is.na(ACT_imp$steps)]<-intsteps$intsteps
daysteps_imp<-tapply(ACT_imp$steps, ACT_imp$date,sum,na.rm=TRUE)
daysteps_imp_mean = mean(daysteps_imp)
daysteps_imp_median = median(daysteps_imp)


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

png(filename="plot3.png")
par(mfrow=c(1,2))
hist(daysteps,breaks=10)
abline(v=daysteps_mean,col='blue',lwd=2)
abline(v=daysteps_median,col='red',lwd=2)
hist(daysteps_imp,breaks=10)
abline(v=daysteps_imp_mean,col='blue',lwd=2)
abline(v=daysteps_imp_median,col='red',lwd=2)
dev.off()

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
#Make a panel plot containing a time series plot (i.e. of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

ACT_imp$date<-as.POSIXlt(ymd(ACT_imp$date))
ACT_imp$day<-weekdays(ACT_imp$date)
day_levels <- c("Weekday","Weekend")
ACT_imp$day[ACT_imp$day=="Monday"|ACT_imp$day=="Tuesday"|ACT_imp$day=="Wednesday"|ACT_imp$day=="Thursday"|ACT_imp$day=="Friday"]<-"Weekday"
ACT_imp$day[ACT_imp$day=="Saturday"|ACT_imp$day=="Sunday"]<-"Weekend"
ACT_imp$day<-factor(ACT_imp$day, levels=day_levels)

ACT_imp_weekday<-subset(ACT_imp,ACT_imp$day=="Weekday")
ACT_imp_weekend<-subset(ACT_imp,ACT_imp$day=="Weekend")

intsteps_weekday<-tapply(ACT_imp_weekday$steps, ACT_imp_weekday$interval,mean)
intsteps_weekday<-as.data.frame(intsteps_weekday)
intsteps_weekday <- cbind(time = rownames(intsteps_weekday), intsteps_weekday)
intsteps_weekend<-tapply(ACT_imp_weekend$steps, ACT_imp_weekend$interval,mean)
intsteps_weekend<-as.data.frame(intsteps_weekend)
intsteps_weekend <- cbind(time = rownames(intsteps_weekend), intsteps_weekend)

png(filename="plot4.png")
par(mfrow=c(2,1))
plot(x=intsteps_weekday$time,y=intsteps_weekday$intsteps,type="l")
plot(x=intsteps_weekend$time,y=intsteps_weekend$intsteps,type="l")
dev.off()

## Only need during the development
#######
if(!file.exists("C:/Users/Herman/data-science/PA1")){
        dir.create("C:/Users/Herman/data-science/PA1")
}

setwd("C:/Users/Herman/data-science/PA1")
#######
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "activity.zip")
unzip("activity.zip","activity.csv",overwrite=T)
activity <- read.csv("activity.csv",colClasses=c(NA,"Date",NA))

##  Eliminate NA values
##  Summarize steps by day
activity2 <- subset(activity, complete.cases(activity)=="TRUE")
ActbyDay <- split(activity2,activity2$date)
ActbyDay2 <- sapply(ActbyDay, function(x) sum(x$steps))

## Plot distribution
hist(ActbyDay2, main="Total Steps by Day", xlab="#Steps")

## Mean and Median
summary(ActbyDay2)

##  Summarize steps by interval
ActbyInt <- split(activity2,activity2$interval)
ActbyInt2 <- sapply(ActbyInt, function(x) mean(x$steps))

## Plot distribution
plot(ActbyInt2, type="l", ylab="#Steps", xlab="Interval") 

## Interval with Max value
names(which.max(ActbyInt2))

## Number of missing values
ActbyNA <- complete.cases(activity)
NArows <- length(ActbyNA[ActbyNA=="FALSE"])
NArows

##  Split activities by NA's
##  Use the average #steps for the interval to impute missing value
ActbyNA2 <- cbind(activity, ActbyNA)
ActbyNA3 <- split(ActbyNA2, ActbyNA2$ActbyNA)
for (row in 1:nrow(ActbyNA3[["FALSE"]])){
        ActbyNA3[["FALSE"]][row,1] <- round(subset(ActbyInt2,names(ActbyInt2)== 
                                                   ActbyNA3[["FALSE"]][row,3]))
}

## Unsplit dataframe and order
newactivity <- rbind(ActbyNA3[["FALSE"]],ActbyNA3[["TRUE"]])
newactivity <- newactivity[order(newactivity$date, newactivity$interval),]

## plot histogram
newActbyDay <- split(newactivity,newactivity$date)
newActbyDay2 <- sapply(newActbyDay, function(x) sum(x$steps))
hist(newActbyDay2, main="Total Steps by Day (Inmuted)", xlab="#Steps")

## Mean and Median
summary(newActbyDay2)

## Add Weekend indicator to inmuted data (newactivity)
newactivity$IsWeekend <- weekdays(newactivity$date) %in% c('Sunday','Saturday')

## Plot average #steps by interval and weekday v weekend
newact2 <- aggregate(steps ~ interval + IsWeekend, newactivity, mean)
library(lattice)
xyplot(steps~interval|factor(IsWeekend,labels=c("Weekday","Weekend")), type="l", 
       data=newact2, main="Average #Steps by Weekday v Weekend", 
       layout=(c(1,2)))


print(newactivity[newactivity$IsWeekend==TRUE,])
summary(newactivity)
str(ActbyDay2)


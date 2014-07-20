### Reproducible Research: Peer Assessment 1

# Loading and preprocessing the data
library(ggplot2)
library(plyr)
unzip("repdata-data-activity.zip")
dat <- read.csv('activity.csv', header = T)
dat$date <- as.Date(dat$date)
str(dat)

## What is mean total number of steps taken per day?
dat.sum <- ddply(dat, "date", summarize, total.steps = sum(steps, na.rm = TRUE))
head(dat.sum)
ggplot(dat.sum, aes(x = total.steps)) + 
  geom_histogram(binwidth= 1000, colour="black", fill="blue") + 
  labs(title = "Total number of steps taken each day", y = "Frequency",  x = "Total steps") 

as.integer( with(dat.sum, mean(total.steps)) )
as.integer( with(dat.sum, median(total.steps)) )

## What is the average daily activity pattern?
dat.avg <- ddply(dat, "interval", summarize, average.steps = mean(steps, na.rm = TRUE))
head(dat.avg)
with(dat.avg, plot(interval, average.steps, type = "l", xlab = "Time (minutes)", 
     ylab = "Ave # of steps per 5 minute increment") )
ggplot(dat.avg, aes(x = interval, y = average.steps)) + 
  geom_line(color = "springgreen") + 
  labs(title = "Average number of steps taken per 5-minutes interval", 
       y = "average number of steps taken",  x = "5 minutes interval across all days") 

index <- with(dat.avg, which.max(average.steps))
dat.avg[index, ]
  
## Imputing missing values
with(dat, sum(is.na(steps)))
with(dat, table(is.na(steps)))
# impute the data by average 5 mins interval, using for loop, find the missing value for 
# each row, then replace NA with corresponding value in dat.avg
dat.filled <- dat
for (i in 1:dim(dat.filled)[1]){
  if ( is.na(dat.filled[i, 1]) ){
    specific.int = dat.filled[i, 3]
    index = which(dat.avg$interval == specific.int)
    dat.filled[i, 1] <- dat.avg[index, 2]
  }
}
###  now check all the data
sum(!complete.cases(dat.filled))
### all the rows are complete

dat.sum2 <- ddply(dat.filled, "date", summarize, total.steps = sum(steps))

ggplot(dat.sum2, aes(x = total.steps)) + 
  geom_histogram(binwidth= 1000, colour="black", fill="blue") + 
  labs(title = "Total number of steps taken each day after imputation", y = "Frequency",  x = "Total steps") 

as.integer(with(dat.sum2, mean(total.steps)))
as.integer(with(dat.sum2, median(total.steps)))



# The comparison of total steps taken between the original data having missing values 
# and the imputed data:
  
# Mean steps taken with the original data containing missing data: 9354
# Mean steps taken with the imputed data: 10395
# Median steps taken with the original data containing missing data: 10766
# Median steps taken with the imputed data: 10766
# Impact of imputing the missing data with average number of steps taken per 5 minute interval: 
# Compared to the histogram drawn from data having missing values, 
# the histogrm drawn from the imputed data seems to be more normal, 
# because it imputed the missing values (i.e., steps taken per day='NA') with average number 
# of steps taken per day.


## Are there differences in activity patterns between weekdays and weekends?
dat.filled$days <- weekdays(dat.sum2$date)
dat.filled$type <- NULL
for (i in 1:dim(dat.filled)[1]){
  if (dat.filled$days[i] == "Saturday" | dat.filled$days[i] == "Sunday"){
    dat.filled$type[i] <- "weekend"
  }
  else{
    dat.filled$type[i] <- "weekday"
  }
}
dat.avg2 <- ddply(dat.filled, c("interval", "type"), summarize, average.steps = mean(steps, na.rm = TRUE))
head(dat.avg2, 10)
# library(lattice)
# xyplot(average.steps ~ interval | type, data = dat.avg2, type = "l", layout = c(1, 2), 
#       xlab = "interval", ylab = "average number of steps", 
#       main = "comparison of the Activity Patterns between Weekends and Weekdays")

ggplot(dat.avg2, aes(x= interval , y = average.steps, color = type)) + 
  geom_line(size = 0.5) + 
  labs(title = "Comparison of the activity patterns between weekends and weekdays", 
       y = "average number of steps",  x = "interval") + facet_grid(type ~ .)




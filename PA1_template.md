# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity1 <- read.csv('activity.csv')
activity <- na.omit(activity1)
```

## What is mean total number of steps taken per day?

```r
library('knitr')
plotdata <- aggregate(activity[c("steps")], list(date = activity$date), sum)
kable(plotdata, format = "pandoc", digit = 2, align = 'l', caption = "Total Number of Steps Taken Per Day", col.names = c("Date", "Steps"))
```



Table: Total Number of Steps Taken Per Day

Date         Steps 
-----------  ------
2012-10-02   126   
2012-10-03   11352 
2012-10-04   12116 
2012-10-05   13294 
2012-10-06   15420 
2012-10-07   11015 
2012-10-09   12811 
2012-10-10   9900  
2012-10-11   10304 
2012-10-12   17382 
2012-10-13   12426 
2012-10-14   15098 
2012-10-15   10139 
2012-10-16   15084 
2012-10-17   13452 
2012-10-18   10056 
2012-10-19   11829 
2012-10-20   10395 
2012-10-21   8821  
2012-10-22   13460 
2012-10-23   8918  
2012-10-24   8355  
2012-10-25   2492  
2012-10-26   6778  
2012-10-27   10119 
2012-10-28   11458 
2012-10-29   5018  
2012-10-30   9819  
2012-10-31   15414 
2012-11-02   10600 
2012-11-03   10571 
2012-11-05   10439 
2012-11-06   8334  
2012-11-07   12883 
2012-11-08   3219  
2012-11-11   12608 
2012-11-12   10765 
2012-11-13   7336  
2012-11-15   41    
2012-11-16   5441  
2012-11-17   14339 
2012-11-18   15110 
2012-11-19   8841  
2012-11-20   4472  
2012-11-21   12787 
2012-11-22   20427 
2012-11-23   21194 
2012-11-24   14478 
2012-11-25   11834 
2012-11-26   11162 
2012-11-27   13646 
2012-11-28   10183 
2012-11-29   7047  


```r
# code to plot histogram
hist(plotdata$steps, xlab = "Total number of steps taken each day", main = "Histogram of the Total Number of Steps Taken Each Day", col = "Blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 


```r
#code to calculate the mean and median of the total number of steps taken each day
means_steps <- format(mean(plotdata$steps), digits = 1)
median_steps <- median(plotdata$steps)
```
#### The mean of the total number of steps taken each day is 10766
#### The median of the total number of steps taken each day is 10765

## What is the average daily activity pattern?

```r
timeseries <- aggregate(activity[c("steps")], list(interval = activity$interval), mean)
plot(timeseries$interval, timeseries$steps, type="l", xlab = "Interval", ylab = "Average Number of Steps Taken", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
# find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
max_steps_interval = timeseries$interval[which.max(timeseries$steps)]
```

#### Interval, on average across all the days in the dataset that contains the maximum number of steps is 835

## Imputing missing values

```r
missing_values <- sum(is.na(activity1))
```
#### Number of missing values = 2304

```r
for (i in 1:nrow(activity1)) {
    if (is.na (activity1$steps[i])) {
         activity1$steps[i] <- subset(timeseries, timeseries$interval == activity1$interval[i])$steps
    }
}
plotdata <- aggregate(activity1[c("steps")], list(date = activity1$date), sum)
hist(plotdata$steps, xlab = "Total number of steps taken each day", main = "Histogram of the Total Number of Steps Taken Each Day", col = "Blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


```r
#code to calculate the mean and median of the total number of steps taken each day
means_steps <- format(mean(plotdata$steps), digits = 1)
median_steps <- format(median(plotdata$steps), digits = 1)
```
#### The mean of the total number of steps taken each day is 10766
#### The median of the total number of steps taken each day is 10766
#### Only the median is different from the first part of this assignment. Imputing missing values has minimal impact on the results. 

## Are there differences in activity patterns between weekdays and weekends?

```r
day_of_week <- weekdays(as.Date(activity1$date))
for (i in 1:nrow(activity1)) {
  if (day_of_week[i] == "Saturday" || day_of_week[i] == "Sunday") {
    day_of_week[i] <- "weekend"
  }
  else {
    day_of_week[i] <- "weekday"
  }   
}
activity2 <- cbind(activity1, day_of_week)
timeseries <- aggregate(activity2[c("steps")], list(interval = activity2$interval, day = activity2$day_of_week), mean)
#plot(timeseries$interval, timeseries$steps, type="l", xlab = "Interval", ylab = "Average Number of Steps Taken", main = "Average Daily Activity Pattern")
library(ggplot2)
p <- ggplot(timeseries, aes(x=interval, y=steps)) +
  geom_line() +
  facet_grid(day~.) +
  ggtitle("Differences in Activity Patterns Between Weekdays and Weekends")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

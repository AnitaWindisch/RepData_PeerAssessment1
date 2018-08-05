---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


```r
data<-read.table("/home/anita/mycourses/DataScienceSpecialization/ReproducibleResearch/CourseProject1/activity.csv",header = TRUE, sep=",")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
totalSteps<-tapply(data$steps,data$date,sum,na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day


```r
hist(totalSteps, ylim=c(0,30),xlab = "Total Number of Steps per Day",main="")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day 

Calculate mean:


```r
totalStepsMean<-mean(totalSteps,na.rm=TRUE)
totalStepsMean
```

```
## [1] 9354.23
```

Calculate median:


```r
totalStepsMedian<-median(totalSteps,na.rm=TRUE)
totalStepsMedian
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averaged<-aggregate(steps~interval,data,mean)

plot(averaged$interval,averaged$steps,type = "l", xlab="Time Period (5-minute interval)", ylab="Steps average")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximumSteps<-averaged[which.max(averaged$steps),]
maximumSteps
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numberNA<-sum(is.na(data))
numberNA
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**I used the average that was calculated in the beginning.**

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData<-data

for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i]<-averaged$steps[which(newData$interval[i]==averaged$interval)]
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsNewData<-tapply(newData$steps,newData$date,sum)
hist(totalStepsNewData,ylim=c(0,40),xlab = "Total Number of Steps per Day",main="New Dataset")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Calculate mean:

```r
totalStepsNewDataMean<-mean(totalStepsNewData)
totalStepsNewDataMean
```

```
## [1] 10766.19
```

Calculate median:

```r
totalStepsNewDataMedian<-median(totalStepsNewData)
totalStepsNewDataMedian
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
newData$daytype<-weekdays(as.Date(newData$date))
newData$daytype<-ifelse(newData$daytype %in% c("Saturday", "Sunday"),"weekend", "weekday")
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2)

averagedNewData<-aggregate(steps~interval+daytype,data=newData,mean)

g <- ggplot(averagedNewData,aes(interval,steps))
g <- g+ geom_line() + facet_grid(daytype ~ .)
g <- g+ xlab("The 5-minute Interval") + ylab("Average Number of Steps per Day Type")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

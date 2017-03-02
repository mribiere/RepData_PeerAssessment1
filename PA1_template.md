# Reproducible Research: Peer Assessment 1
## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken


The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Loading and preprocessing the data


```r
activity <- read.csv("./activity.csv")
```



## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
stepsPerDay <- aggregate(steps ~ date, activity, sum)
hist(stepsPerDay$steps, main = "Total number of steps per day", xlab="Number Of Steps per Day",col="grey")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)

```r
## or ggplot(stepsPerDay, aes(steps)) + geom_histogram(bins=5)
```

2. Calculate and report the **mean** and **median** total number of steps taken per day
- the mean value of the total number of steps per day ....

```r
meanSteps <- mean(stepsPerDay$steps)
```
....is 1.0766189\times 10^{4}

- the median value of the total number of steps per day...

```r
medianSteps <- median(stepsPerDay$steps)
```
...is 10765

- the corresponding figure for the mean in Magenta and the median in doted Blue on the histogram of number of steps per day 

```r
hist(stepsPerDay$steps, main = "Total number of steps per day", xlab="Number Of Steps per Day",col="grey")
abline(v=meanSteps,lty=1,lwd=2,col="Magenta")
abline(v=medianSteps,lty=2,lwd=2,col="Cyan")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)


## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsByInterval <- aggregate(steps ~ interval,activity,mean)
plot(stepsByInterval$interval,stepsByInterval$steps, type="l", xlab="Interval",ylab="average number of steps taken, averaged across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- max(stepsByInterval$steps)
maxInterval<- subset(stepsByInterval$interval, stepsByInterval$steps==maxSteps)
```
The 5-minute interval that contains the maximum number of steps is 835

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
totalNA <- nrow(subset(activity,is.na(activity$steps)))
```
The total number of missing value is 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- we use as strategy to add a day factor in our dataframe

```r
library(dplyr)
activity2 <- mutate(activity, day=weekdays(as.Date(activity$date)))
agg <- aggregate(steps ~ day, data=activity2,mean)
colnames(agg)[2]<-"meanSteps"
```
- we use as strategy to add a day factor in our dataframe. So the data frame now looks like:

```r
head(activity2)
```

```
##   steps       date interval    day
## 1    NA 2012-10-01        0 Monday
## 2    NA 2012-10-01        5 Monday
## 3    NA 2012-10-01       10 Monday
## 4    NA 2012-10-01       15 Monday
## 5    NA 2012-10-01       20 Monday
## 6    NA 2012-10-01       25 Monday
```
- then, we calculate for each day of the week, the mean number of steps and create a dataframe for this:

```r
agg
```

```
##         day meanSteps
## 1    Friday  42.91567
## 2    Monday  34.63492
## 3  Saturday  43.52579
## 4    Sunday  42.63095
## 5  Thursday  28.51649
## 6   Tuesday  31.07485
## 7 Wednesday  40.94010
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Finally we replace the NA value of the day with the corresponding mean value of the number of steps corresponding to the day in the week.

```r
activity3 <- merge(activity2,agg,by="day")
activity3 <- within(activity3, steps[is.na(steps)] <- (meanSteps[is.na(steps)]))
activity3$meanSteps <- NULL
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDay_WONA <- aggregate(steps ~ date, activity3, sum)
hist(stepsPerDay_WONA$steps, main = "Total number of steps per day", xlab="Number Of Steps per Day",col="grey")
meanSteps_WONA <- mean(stepsPerDay_WONA$steps)
medianSteps_WONA <- median(stepsPerDay_WONA$steps)
abline(v=meanSteps_WONA,lty=1,lwd=2,col="Magenta")
abline(v=medianSteps_WONA,lty=2,lwd=2,col="Cyan")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)

The impact on the estimates of the total daily number of steps can be seen in the summary of the different dataframes.


```r
summary(stepsPerDay)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
summary(stepsPerDay_WONA)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 8918  
##  2012-10-03: 1   Median :11015  
##  2012-10-04: 1   Mean   :10821  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

Mean and Median are a bit different with the optimization of the NA value, we can visualy differentitiate them on the graph, but the main different between the two datasets is on the third quantile.

## Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekend <-c("Saturday","Sunday")
activity4 <- mutate(activity3, typeofday= ifelse (weekdays(as.Date(activity3$date)) %in% weekend, "weekend","weekday"))
```

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
weekdayActivity <- subset(activity4,typeofday=="weekday")
weekendActivity <- subset(activity4,typeofday=="weekend")
stepsByIntervalweekday <- aggregate(steps ~ interval,weekdayActivity,mean)
stepsByIntervalweekend <- aggregate(steps ~ interval,weekendActivity,mean)
par(mfrow=c(2,1), mar= c(4,4,2,1))
plot(stepsByIntervalweekday$interval,stepsByIntervalweekday$steps, type="l", xlab="Interval",ylab="Number of steps",main="weekday")
plot(stepsByIntervalweekend$interval,stepsByIntervalweekend$steps, type="l", xlab="Interval",ylab="Number of steps",main="weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)

We can observe that people have more activities during the week-end. 

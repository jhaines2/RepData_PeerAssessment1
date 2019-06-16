---
title: "Course 5 week 2"
output: 
  html_document: 
    keep_md: yes
---



## Loading and preprocessing the data

**Load the data**


```r
data <- read.csv("activity.csv")
```

**Process/transform the data into a format suitable for your analysis**


```r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

**Make a histogram of the total number of steps taken each day**


```r
data2 <- group_by(data, date)
summary <- summarise(data2, steps = sum(steps, na.rm = FALSE))

png("hist1.png", width = 500, height = 500)
hist(summary$steps)
dev.off()
```

```
## png 
##   2
```

```r
hist(summary$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

**Calculate and report the mean and median total number of steps taken per day**


```r
steps_mean <- mean(summary$steps, na.rm=TRUE)
steps_median <- median(summary$steps, na.rm=TRUE)
```

The mean steps is 10766.19
The median steps is 10765

## What is the average daily activity pattern?

**Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
summary <- summarise(group_by(data, interval), steps_mean = mean(steps, na.rm=TRUE))

ggplot(summary, aes(interval, steps_mean)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggsave("line.png", width = 5, height = 5)
```

**Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
max_interval <- summary$interval[summary$steps_mean==max(summary$steps_mean)]
```

The interval with the maximum steps on average is 835

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**


```r
num_NAs <- sum(is.na(data$steps))
```

The number of NAs is 2304

**Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

Strategy is to fill NAs in "data" with the average number of steps for that interval.

**Create a new dataset that is equal to the original dataset but with the missing data filled in.**

New dataset is named "data_filled".


```r
data_filled <- data
i <- integer()
for (i in 1:nrow(data_filled)){
    if (is.na(data_filled$steps[i])){
        interval <- data_filled$interval[i]
        data_filled$steps[i] <- summary$steps_mean[summary$interval == interval]
    }
}
```

**Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.**


```r
data_filled2 <- group_by(data_filled, date)
summary_filled <- summarise(data_filled2, steps = sum(steps, na.rm = FALSE))

png("hist2.png", width = 500, height = 500)
hist(summary_filled$steps)
dev.off()
```

```
## png 
##   2
```

```r
hist(summary_filled$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
filled_mean <- mean(summary_filled$steps, na.rm=TRUE)
filled_median <- median(summary_filled$steps, na.rm=TRUE)
```

The mean steps is 10766.19
The median steps is 10766.19

**Do these values differ from the estimates from the first part of the assignment? **
The mean is the same and the median is slightly different.

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**
The impact to the mean is 0.00
The impact to the median is 1.19

## Are there differences in activity patterns between weekdays and weekends?

**Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**


```r
weekdays <- vector(mode = "character", length = nrow(data_filled))
i <- integer()
for (i in 1:nrow(data_filled)){
    if (weekdays(data_filled$date[i]) == "Saturday" | weekdays(data_filled$date[i]) == "Sunday"){
        weekdays[i] <- "weekend"
    } else {
        weekdays[i] <- "weekday"
    }
weekdays_fact <- as.factor(weekdays)
data_filled2 <- cbind(data_filled, wd = weekdays_fact)
}
```

**Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)**


```r
data_weekdays <- data_filled2[data_filled2$wd == "weekday", ]
data_weekends <- data_filled2[data_filled2$wd == "weekend", ]

summary_weekdays <- summarise(group_by(data_weekdays, interval), steps_mean = mean(steps, na.rm=TRUE))
summary_weekends <- summarise(group_by(data_weekends, interval), steps_mean = mean(steps, na.rm=TRUE))
summary_joined <- rbind(cbind(summary_weekdays, wd = "weekday"), cbind(summary_weekends, wd = "weekend"))

ggplot(summary_joined, aes(interval, steps_mean)) + geom_line() + facet_grid(wd~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggsave("frames.png", width = 5, height = 5)
```

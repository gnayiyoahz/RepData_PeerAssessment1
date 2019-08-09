---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
# Unzip the file.
unzip("./activity.zip")

# Load the data file into a data frame.
raw_data <- read.csv("./activity.csv")

# Remove the NA values.
activity_data <- raw_data[complete.cases(raw_data),]

# Take a look at activity_data.
head(activity_data)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```



## What is mean total number of steps taken per day?


```r
# Calculate the total number of steps per day.
step_per_day <- aggregate(steps ~ date, activity_data, sum)

# Take a look at step_per_day.
head(step_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
# Plot a histogram.
hist(step_per_day$steps, main = "Histogram of the Total Number of Steps Per Day", xlab = "Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate the mean.
round(mean(step_per_day$steps))
```

```
## [1] 10766
```

```r
# Calculate the median.
median(step_per_day$steps)
```

```
## [1] 10765
```
The mean total number of steps taken per day is $10,766$ and the median is $10,765$.



## What is the average daily activity pattern?


```r
# Calculate the average number of steps per interval.
step_by_interval <- aggregate(steps ~ interval, activity_data, mean)

# Take a look at step_by_inverval.
head(step_by_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
# Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken (y-axis).
plot(step_by_interval$interval, step_by_interval$steps, xlab = "5-minute Interval Index", ylab = "Average Number of Steps", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Find the 5-minute interval that contains the maximum.
step_by_interval$interval[which.max(step_by_interval$steps)]
```

```
## [1] 835
```

```r
# Find the maximum number of steps in that interval.
round(step_by_interval$steps[which.max(step_by_interval$steps)])
```

```
## [1] 206
```
The maximum number of steps is $206$ in the $835^{th}$ interval.



## Imputing missing values


```r
# Calculate and report the total number of missing values in the dataset.
na_rows <-is.na(raw_data$steps)
sum(na_rows)
```

```
## [1] 2304
```

```r
# Fill in all of the missing values in the dataset using the the mean for that 5-minute interval, etc.
filled_data <- split(raw_data, raw_data$interval)

for (i in 1:288) {
  filled_data[[i]]$steps[is.na(filled_data[[i]]$steps)]=step_by_interval$steps[i]
}

library(plyr)
new_data <- ldply(filled_data)[,-1]

# Make a histogram of the total number of steps taken each day.
new_step_per_day <- aggregate(steps ~ date, new_data, sum)
hist(new_step_per_day$steps, main = "Histogram of the Total Number of Steps Per Day", xlab = "Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Calculate the mean.
round(mean(new_step_per_day$steps))
```

```
## [1] 10766
```

```r
# Calculate the median.
median(new_step_per_day$steps)
```

```
## [1] 10766.19
```
The new mean total number of steps taken per day is $10,766$ and the new median is $10,766.19$.

The new mean and median do NOT differ from the estimates from the first part of the assignment. The imputed data does not create obvious impact.



## Are there differences in activity patterns between weekdays and weekends?


```r
# Create a function to check day in week.
checkDayInWeek <- function(date) {
    d <- weekdays(as.Date(date, '%Y-%m-%d'))
    if  (!(d == 'Saturday' || d == 'Sunday')) {
        return('Weekday') 
    } else {
        return('Weekend')
    }
}

# Create a new factor variable  with two levels – “weekday” and “weekend”.
new_data$day <- as.factor(sapply(new_data$date, checkDayInWeek))

# Take a look at new_data.
head(new_data)
```

```
##       steps       date interval     day
## 1  1.716981 2012-10-01        0 Weekday
## 2  0.000000 2012-10-02        0 Weekday
## 3  0.000000 2012-10-03        0 Weekday
## 4 47.000000 2012-10-04        0 Weekday
## 5  0.000000 2012-10-05        0 Weekday
## 6  0.000000 2012-10-06        0 Weekend
```

```r
## Calculate the average number of steps per interval by day type.
steps_by_day_type <- aggregate(steps ~ interval+day, new_data, mean)

# Make a panel plot containing a time series plotof the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
library(ggplot2)
panel_plot <- ggplot(steps_by_day_type, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day)) +
    theme_gray() +
    facet_grid(day ~ ., scales="fixed", space="fixed") +
    labs(x="5-minute Interval Indexl", y=expression("Average No. of Steps")) +
    ggtitle("No. of Steps Per Interval by Day Type")
print(panel_plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

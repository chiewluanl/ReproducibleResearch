setwd("/Users/chiewluanl/RProject/Reproducible_Research")


```r
library("plyr")
library("knitr")
library("reshape2")
library("ggplot2")

#Loading and preprocessing the data
if(!file.exists("./data")){
  dir.create("./data")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl,destfile="./data/repdata_2Fdata_2Factivity.zip",method="curl")
  unzip(zipfile="./data/repdata_2Fdata_2Factivity.zip",exdir="./data")
  path_rf <- file.path("./data")
  files<-list.files(path_rf, recursive=TRUE)
  files
}

##1.0 Load the data (i.e read.csv())
data <- read.csv('data/activity.csv')
dim(data)
```

```
## [1] 17568     3
```

```r
##2.0 Process/transform the data (if necessary) into a format suitable for your analysis
nonNAData <- data[complete.cases(data$steps),]
dim(nonNAData)
```

```
## [1] 15264     3
```

```r
#What is mean total number of steps taken per day?

##1.0 Calculate the total number of steps taken per day
numOfStepsByDate <- aggregate(nonNAData$steps, list(nonNAData$date), sum)
colnames(numOfStepsByDate) <- c("date", "steps")
numOfStepsByDate
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
##2.0 Make a histogram of the total number of steps taken each day
library(ggplot2)
hist(numOfStepsByDate$steps)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
##3.0 Calculate and report the mean and median of the total number of steps taken per day
steps_mean   <- mean(numOfStepsByDate$steps)
steps_median <- median(numOfStepsByDate$steps)
steps_mean
```

```
## [1] 10766.19
```

```r
steps_median
```

```
## [1] 10765
```

```r
#What is the average daily activity pattern?

##1.0 Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, ## averaged across all days (y-axis)
steps_per_interval <- aggregate(steps ~ interval, nonNAData, mean)
plot(steps ~ interval, data = steps_per_interval, type = "l")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
###== OR ==
stepsInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
ggplot(data=stepsInterval, aes(x=interval, y=steps)) + geom_line()
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
###==


## 2.0 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

```r
## Imputing missing values
##1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
missingStep <- data[!complete.cases(data$steps),]
nrow(missingStep)
```

```
## [1] 2304
```

```r
## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##count = 0
dataWithNAReplacement <- data

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
for (i in 1:nrow(dataWithNAReplacement)){
  if (is.na(dataWithNAReplacement$steps[i])){
    dataWithNAReplacement$steps[i] <- steps_per_interval$steps[which(steps_per_interval$interval == dataWithNAReplacement$interval[i])]
##   count = count +1
  }
}

## 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalSteps <- aggregate(steps ~ date, data = dataWithNAReplacement, sum)
hist(totalSteps$steps)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
##Ans: Same

## Are there differences in activity patterns between weekdays and weekends?
## 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
dataWithNAReplacement$day = ifelse(as.POSIXlt(as.Date(dataWithNAReplacement$date))$wday%%6 == 
                              0, "weekend", "weekday")
### For Sunday and Saturday : weekend, Other days : weekday
dataWithNAReplacement$day = factor(dataWithNAReplacement$day, levels = c("weekday", "weekend"))

## 2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

stepsInterval = aggregate(steps ~ interval + day, dataWithNAReplacement, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsInterval, 
       type = "l", ylab= "Number of steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)
...

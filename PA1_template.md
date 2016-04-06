setwd("/Users/chiewluanl/RProject/Reproducible_Research")
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

##2.0 Process/transform the data (if necessary) into a format suitable for your analysis
nonNAData <- data[complete.cases(data$steps),]
dim(nonNAData)

#What is mean total number of steps taken per day?

##1.0 Calculate the total number of steps taken per day
numOfStepsByDate <- aggregate(nonNAData$steps, list(nonNAData$date), sum)
colnames(numOfStepsByDate) <- c("date", "steps")
numOfStepsByDate

##2.0 Make a histogram of the total number of steps taken each day
library(ggplot2)
hist(numOfStepsByDate$steps)

##3.0 Calculate and report the mean and median of the total number of steps taken per day
steps_mean   <- mean(numOfStepsByDate$steps)
steps_median <- median(numOfStepsByDate$steps)
steps_mean
steps_median

#What is the average daily activity pattern?

##1.0 Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, ## averaged across all days (y-axis)
steps_per_interval <- aggregate(steps ~ interval, nonNAData, mean)
plot(steps ~ interval, data = stepsInterval, type = "l")
##== OR# ==
stepsInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
ggplot(data=stepsInterval, aes(x=interval, y=steps)) + geom_line()
###==


## 2.0 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
max_interval

## Imputing missing values
##1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
missingStep <- data[!complete.cases(data$steps),]
nrow(missingStep)

## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##count = 0
dataWithNAReplacement <- data

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
for (i in 1:nrow(dataWithNAReplacement)){
  if (is.na(dataWithNAReplacement$steps[i])){
    dataWithNAReplacement$steps[i] <- steps_per_interval$steps[[which(steps_per_interval$interval == dataWithNAReplacement$interval[i])]]
##   count = count +1
  }
}

## 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalSteps <- aggregate(steps ~ date, data = dataWithNAReplacement, sum)
hist(totalSteps$steps)
mean(totalSteps$steps)
median(totalSteps$steps)

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
       type = "l")

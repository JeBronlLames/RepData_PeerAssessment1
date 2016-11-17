
PA1_template.Rmd
Cody Hollohan
November 15, 2016


# Peer-graded Assignment: Course Project 1

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken


##Loading and preprocessing the data

```{r load_data, echo = TRUE, results = "hide", warning = FALSE, message = FALSE}
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

## 1 ## Reading data
act <- read.csv("activity.csv")
## 2 ## Filtering for non-NA values only
act_comp <- act[complete.cases(act),]
        # Converting "date" to 'date-time' class
activity <- transform(act_comp, date = ymd(act_comp$date))
```

## What is mean total number of steps taken per day?
```{r mean_data, echo = TRUE}
## 1 ## Cutting data into buckets by day and taking sums
stepsum <- aggregate(steps ~ date, data = activity, FUN = sum)
## 2 ## Generating a histogram of the total number of steps taken each day
hist(stepsum$steps, breaks = 30)
## 3 ## Mean and Median calculations for number of steps taken each day
stepmean <- mean(stepsum$steps)
stepmedian <- median(stepsum$steps)
```
The mean is `r stepmean`, and the median is `r stepmedian`.


## What is the average daily activity pattern?
```{r daily_activity, echo = TRUE}
## 1 ## Time series plot of the average number of steps taken
int_mean <- aggregate(steps ~ interval, data = activity, FUN = mean)

plot(int_mean, type = "l", col = "blue", lwd = 3, 
     main = "Time Series Plot Of Average Number Of Steps Per Interval", 
     xlab = "Interval", ylab = "Step Count")
## 2 ## The 5-minute interval that, on average, contains the max number of steps
maxindex <- which.max(int_mean$steps)
maxstep <- int_mean[104,]
```

The max interval and value are `r maxstep`.


## Imputing missing values
``` {r impute_data, echo = TRUE}
## 1 ## Calculating count of NA values
stepNA <- sum(is.na(act$steps)) # ALL NA ARE FOUND IN "steps" COLUMN
dateNA <- sum(is.na(act$date))
intNA <- sum(is.na(act$interval))
## 2 ## Strategy: Replace NA's with corresponding mean for that interval
        # This way, the overall mean of the sample is not affected.
imp_steps <- numeric()

for(i in 1:nrow(act)) {
        imp_steps[i] <- act$steps[i]
        if(is.na(imp_steps[i])) {
                imp_steps[i] <- int_mean$steps[which(int_mean$interval ==  act$interval[i])]
                                }
                      }
## 3 ## Generating new dataset equal to original with the exception of imputed values 
imp_act <- cbind(act[,-1],imp_steps)

## 4 ## Generate a histogram of the total number of steps taken each day
        # Also, calculate the mean and median
# Converting "date" column to 'date' class
imp_act_date <- transform(imp_act, date = ymd(imp_act$date))
# Cutting data into buckets by day and taking sums
imp_stepsum <- aggregate(imp_steps ~ date, data = imp_act_date, FUN = sum)
# Generating a histogram of the total number of steps taken each day
hist(imp_stepsum$imp_steps, breaks = 30)
# Mean and Median calculations for number of steps taken each day
imp_stepmean <- mean(imp_stepsum$imp_steps)
imp_stepmedian <- median(imp_stepsum$imp_steps)
```

The imputed mean and median are `r imp_stepmean`, and `r imp_stepmedian`. Consider this in comparison to the
original mean and median of `stepmean` and `stepmedian`.


## Are there differences in activity patterns between weekdays and weekends?

```{r weekday_weekend, echo = TRUE}
## 1 ## New dataframe including factor variable for c("weekday", "weekend")
# Converting "date" to day of the week
imp_act_date$day <- weekdays(imp_act_date$date)
# Generating dataframes for weekdays and weekends
weekday <- filter(imp_act_date, 
                  day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
weekend <- filter(imp_act_date, day %in% c("Saturday", "Sunday"))
# Calculating the interval means across each day        
int_mean_wd <- aggregate(imp_steps ~ interval, data = weekday, FUN = mean)
int_mean_we <- aggregate(imp_steps ~ interval, data = weekend, FUN = mean)

## 2 ## Generate a panel plot comparing the average number of steps taken for each 5min int
        # for both weekdays and weekends.
#par(mar = c(2,0.5,2,0.5), oma = c(0,0,0,0), mfrow=c(2,1))

palette(brewer.pal(8, "Set1"))
par(mar = c(2,0.5,2,0.5), oma = c(0,2.5,0,0), mfrow=c(2,1))
plot(int_mean_wd, type = "l", col = palette()[1], lwd = 3, 
     main = "Time Series Plot Of Average Number Of Steps Per Weekday Interval", 
     xlab = "Interval", ylab = "Step Count")

plot(int_mean_we, type = "l", col = palette()[2], lwd = 3, 
     main = "Time Series Plot Of Average Number Of Steps Per Weekend Interval", 
     xlab = "Interval", ylab = "Step Count")
```





Word.




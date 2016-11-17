############################################################
#### Reproducible Research - Week 2 Peer Reviewed Assgn ####
############################################################

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
## 1 ## Reading in the data
act <- read.csv("activity.csv")

## Filtering for non-NA values only
act_comp <- act[complete.cases(act),]

## Converting "date" column to 'date' class
activity <- transform(act_comp, date = ymd(act_comp$date))

## Cutting data into buckets by day and taking sums
stepsum <- aggregate(steps ~ date, data = activity, FUN = sum)

## 2 ## Generating a histogram of the total number of steps taken each day
hist(stepsum$steps, breaks = 30)

## 3 ## Mean and Median calculations for number of steps taken each day
stepmean <- mean(stepsum$steps)
stepmedian <- median(stepsum$steps)

## 4 ## Time series plot of the average number of steps taken
int_mean <- aggregate(steps ~ interval, data = activity, FUN = mean)

plot(int_mean, type = "l", col = "blue", lwd = 3, 
     main = "Time Series Plot Of Average Number Of Steps Per Interval", 
     xlab = "Interval", ylab = "Step Count")

## 5 ## The 5-minute interval that, on average, contains the max number of steps
maxindex <- which.max(int_mean$steps)
maxstep <- int_mean[104,]

## 6 ## Code to describe and show a strategy for imputing missing data
stepNA <- sum(is.na(act$steps)) # ALL NA ARE FOUND IN "steps" COLUMN
dateNA <- sum(is.na(act$date))
intNA <- sum(is.na(act$interval))

# Let's replace all NA's with the corresponding means for that interval across all days. 
# This way, the overall mean of the sample is not affected.
imp_steps <- numeric()

for(i in 1:nrow(act)) {
        imp_steps[i] <- act$steps[i]
                if(is.na(imp_steps[i])) {
                imp_steps[i] <- int_mean$steps[which(int_mean$interval == act$interval[i])]
                }
}

# Create a dataframe with the imputed "steps" values replacing the "NA" values of "steps"
imp_act <- cbind(act[,-1],imp_steps)

## 7 ## Histogram of the total number of steps taken each day after "NA" are imputed

# Converting "date" column to 'date' class
imp_act_date <- transform(imp_act, date = ymd(imp_act$date))

# Cutting data into buckets by day and taking sums
imp_stepsum <- aggregate(imp_steps ~ date, data = imp_act_date, FUN = sum)

# Generating a histogram of the total number of steps taken each day
hist(imp_stepsum$imp_steps, breaks = 30)

# Mean and Median calculations for number of steps taken each day
imp_stepmean <- mean(imp_stepsum$imp_steps)
imp_stepmedian <- median(imp_stepsum$imp_steps)

## 8 ## Panel plot comparing avg num of steps per 5min int across weekdays and weekends
imp_act_date$day <- weekdays(imp_act_date$date)

weekday <- filter(imp_act_date, 
                  day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
weekend <- filter(imp_act_date, day %in% c("Saturday", "Sunday"))
        
int_mean_wd <- aggregate(imp_steps ~ interval, data = weekday, FUN = mean)
int_mean_we <- aggregate(imp_steps ~ interval, data = weekend, FUN = mean)

par(mfrow=c(2,1))
palette(brewer.pal(8, "Set1"))

plot(int_mean_wd, type = "l", col = palette()[1], lwd = 3, 
     main = "Time Series Plot Of Average Number Of Steps Per Weekday Interval", 
     xlab = "Interval", ylab = "Step Count")
#legend("topright", legend = "weekday", 
       #lty = 0, text.col = palette()[1], bty = "n", text.font = 2)

plot(int_mean_we, type = "l", col = palette()[2], lwd = 3, 
     main = "Time Series Plot Of Average Number Of Steps Per Weekend Interval", 
     xlab = "Interval", ylab = "Step Count")
#legend("topright", legend = "weekend", 
       #lty = 0, text.col = palette()[2], bty = "n", text.font = 2)

## 9 ## All of the R code needed to reproduce the results        
        
# See all code above for 1:8

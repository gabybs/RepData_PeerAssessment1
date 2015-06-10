# Reproducible Research: Peer Assessment 1

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## The Data

The dataset for this assignment can be found in this repository as *activity.zip*.

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA).

- date: The date on which the measurement was taken in YYYY-MM-DD format.

- interval: Identifier for the 5-minute interval in which measurement was taken.

## Loading and preprocessing the data

```{r}
echo=TRUE
setwd("C:/Users/Gaby/Documents/RepData_PeerAssessment1")
library(lattice)
```

1. Load the data

```{r, echo=TRUE}
unzip("activity.zip")
data <- read.csv("activity.csv",
                 header=TRUE,
                 na.strings="NA")
head(data)
```                              
                 
```{r, echo=TRUE}
tail(data)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis.

```{r, echo=TRUE}
class(data$date)
```

```{r, echo=TRUE}
data$date <- as.Date(data$date)
class(data$date)
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.

```{r, echo=TRUE}
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day.

```{r, echo=TRUE}
hist(total_steps, 
     col="blue",
     xlab="Total number of steps taken per day",
     ylab="Frequency",
     main="Histogram of the total number of steps taken each day")
```

3. Calculate and report the mean and median of the total number of steps taken per day.

```{r, echo=TRUE}
Mean <- mean(total_steps, na.rm=TRUE)
Mean
```

```{r, echo=TRUE}
Median <- median(total_steps, na.rm=TRUE)
Median
```

On average, 9354 steps are taken per day. And the median is 10395.

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```{r, echo=TRUE}
meanSteps <- tapply(data$steps,
                    data$interval,
                    mean, na.rm=TRUE)
plot(row.names(meanSteps), 
     meanSteps,
     type = "l",
     xlab = "Time intervals (5 minutes)",
     ylab = "Mean number of steps taken (all days)",
     main = "Average steps taken at 5-minute intervals")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}
interval_num <- which.max(meanSteps)
interval_maxSteps <- names(interval_num)
interval_maxSteps
```


The corresponding 5-minute interval identifier is 835.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```{r, echo=TRUE}
totalNA <- sum(is.na(data))
totalNA
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

(Question 2 and 3 will be answerd in the same code chunk)

```{r, echo=TRUE}
na_indices <-  which(is.na(data))

imputed_values <- meanSteps[as.character(data[na_indices,3])]

names(imputed_values) <- na_indices
for (i in na_indices) {
    data$steps[i] = imputed_values[as.character(i)]
}
new.data <- data
sum(is.na(new.data))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r, echo=TRUE}
totsteps <- tapply(new.data$steps, new.data$date, sum)
hist(totsteps, col= "green",
     xlab="Total steps per day",
     ylab="Frequency",
     main="Histogram of Total Steps taken per day")
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r, echo=TRUE}
new.data$date <- as.Date(new.data$date)
new.data$weekdays <- factor(format(new.data$date, "%A"))
levels(new.data$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
head(new.data)
```

```{r, echo=TRUE}
tail(new.data)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r, echo=TRUE}
av <- aggregate(steps ~ interval + weekdays, data=new.data, mean)
xyplot(av$steps ~ av$interval | av$weekdays, 
       layout=c(1, 2),
       type="l",
       xlab="Interval",
       ylab="Number of steps")
```

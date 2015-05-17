if(!file.exists("./data/Project2")) dir.create("./data/Project2")
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","temp.zip", mode="wb")
data <- read.csv(unz("temp.zip", "activity.csv"))

#What is mean of the total number of steps taken per day?

#1. Calculate the total number of steps taken per day
dailysteps <- aggregate(data$steps, by = list(data$date), sum, na.rm=TRUE) 
names(dailysteps) <- c("Date", "Steps")


#2.Make a histogram of the total number of steps taken each day
require(ggplot2)
ggplot(dailysteps, aes(dailysteps$Steps)) +
        geom_histogram(aes(fill = ..count..), binwidth=(1000)) +
        xlab("Total number of steps taken each day") +
        ylab("Count") +
        ggtitle("TOTAL STEPS TAKEN EACH DAY")
dev.off()

#3. Calculate and report the mean and median of the total number of steps taken per day
meanSteps <- mean(dailysteps$Steps) 
medianSteps <- median(dailysteps$Steps)

#Mean the total number of steps taken per day
meanSteps

#Median of the total number of steps taken per day
medianSteps

#What is the average daily activity pattern?

#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all days (y-axis)
stepsInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval,
     type = "l",
     xlab = "Intervals - 5-minutes",
     ylab = "Average number of steps taken,averaged across all days",
     main = "AVERAGE DAILY ACTIVITY PATTERN",
     col = "red")
dev.off()

#2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxSteps <- stepsInterval[which.max(stepsInterval$steps),"interval"]
maxSteps

#Inputting missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
naSteps <- subset(data, is.na(steps))
NAs <-length(naSteps$steps)
NAs

#2. Devise a strategy for filling in all of the missing values in the dataset.
#The strategy does not need to be sophisticated. For example, you could use the
#mean/median for that day, or the mean #for that 5-minute interval, etc.

meanStepsperInterval <- function(interval){
        stepsInterval[stepsInterval$interval==interval,"steps"]
}

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
newdata <- data
flag = 0
for (i in 1:nrow(newdata)) {
        if (is.na(newdata[i,"steps"])) {
                newdata[i,"steps"] <- meanStepsperInterval(newdata[i,"interval"])
                flag = flag + 1
        }
}

#4. Make a histogram of the total number of steps taken each day and Calculate
#and report the mean and median total number of steps taken per day. Do these
#values differ from the estimates from the first part of the assignment?

#What is the impact of imputing missing data on the estimates of the total daily number of steps?
newdailysteps <- aggregate(newdata$steps, by = list(newdata$date), sum, na.rm=TRUE) 
names(newdailysteps) <- c("Date", "Steps")
ggplot(newdailysteps, aes(newdailysteps$Steps)) +
        geom_histogram(aes(fill = ..count..), binwidth=(1000)) +
        xlab("Total number of steps taken each day") +
        ylab("Count") +
        ggtitle("TOTAL STEPS TAKEN EACH DAY, WITH NA's SUBSTITUTED")
dev.off()

newmeanSteps <- mean(newdailysteps$Steps) 
newmedianSteps <- median(newdailysteps$Steps)

newmeanSteps
newmedianSteps

# Are there differences in activity patterns between weekdays and weekends?

#5.Create a new factor variable in the dataset with two levels - "weekday" and
#"weekend" indicating whether a given date is a weekday or weekend day.
newdata$day <- ifelse(as.POSIXlt(as.Date(newdata$date))$wday%%6 == 
                              0, "Weekend", "Weekday")
newdata$day <- factor(newdata$day, levels = c("Weekday", "Weekend"))

# 6. Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis).

#See the README file in the GitHub repository to see an example of what this
#plot should look like using simulated data.

#Plot made using Lattice
library("lattice")
newstepsInterval= aggregate(steps ~ interval + day, newdata, mean)
xyplot(steps ~ interval | factor(day), data = newstepsInterval, aspect = 1/2, 
       type = "l")
dev.off()

library(knitr)
knit2html ("PA1_template.md")
browseURL("PA1_template.html")
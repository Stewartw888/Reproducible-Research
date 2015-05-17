---
output:
  html_document:
    fig_caption: yes
    highlight: tango
    keep_md: yes
    theme: journal
    toc: yes
---
Reproducible Research : Assignment 1 - May 2015
====

Introduction
---

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data
---

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment
---

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis


Submitting the Assignment
---

To submit the assignment:

Commit the your completed PA1_template.Rmd file to the master branch of your git repository (you should already be on the master branch unless you created new ones)

Commit your PA1_template.md and PA1_template.html files produced by processing your R markdown file with knit2html() function in R (from the knitr package) by running the function from the console.

If your document has figures included (it should) then they should have been placed in the figure/ directory by default (unless you overrided the default). Add and commit the figure/ directory to yoru git repository so that the figures appear in the markdown file when it displays on github.

Push your master branch to GitHub.

Submit the URL to your GitHub repository for this assignment on the course web site.

In addition to submitting the URL for your GitHub repository, you will need to submit the 40 character SHA-1 hash (as string of numbers from 0-9 and letters from a-f) that identifies the repository commit that contains the version of the files you want to submit. You can do this in GitHub by doing the following

Going to your GitHub repository web page for this assignment

Click on the "?? commits" link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say "10 commits".

You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the "copy to clipboard" button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.


Questions and solutions
---


```{r}
if(!file.exists("./data/Project2")) dir.create("./data/Project2")
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","temp.zip", mode="wb")
data <- read.csv(unz("temp.zip", "activity.csv"))
```

*What is mean of the total number of steps taken per day?*

*1. Calculate the total number of steps taken per day*
```{r}
dailysteps <- aggregate(data$steps, by = list(data$date), sum, na.rm=TRUE) 
names(dailysteps) <- c("Date", "Steps")
```

*2.Make a histogram of the total number of steps taken each day*
```{r}
require(ggplot2)
ggplot(dailysteps, aes(dailysteps$Steps)) +
        geom_histogram(aes(fill = ..count..), binwidth=(1000)) +
        xlab("Total number of steps taken each day") +
        ylab("Count") +
        ggtitle("TOTAL STEPS TAKEN EACH DAY")
dev.off()
```

*3. Calculate and report the mean and median of the total number of steps taken per day*
```{r}
meanSteps <- mean(dailysteps$Steps) 
medianSteps <- median(dailysteps$Steps)
```

*Mean the total number of steps taken per day*
```{r}
meanSteps
```

*Median of the total number of steps taken per day*
```{r}
medianSteps
```

*What is the average daily activity pattern?*

*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*
```{r}}
stepsInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval,
     type = "l",
     xlab = "Intervals - 5-minutes",
     ylab = "Average number of steps taken,averaged across all days",
     main = "AVERAGE DAILY ACTIVITY PATTERN",
     col = "red")
dev.off()
```

*2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*
```{r}}
maxSteps <- stepsInterval[which.max(stepsInterval$steps),"interval"]
maxSteps
```

*Inputting missing values*

*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*
```{r}
naSteps <- subset(data, is.na(steps))
NAs <-length(naSteps$steps)
NAs
```

*2. Devise a strategy for filling in all of the missing values in the dataset.*
*The strategy does not need to be sophisticated. For example, you could use the #mean/median for that day, or the mean #for that 5-minute interval, etc.*
```{r}
meanStepsperInterval <- function(interval){
        stepsInterval[stepsInterval$interval==interval,"steps"]
}
```

*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*
```{r}
newdata <- data
flag = 0
for (i in 1:nrow(newdata)) {
        if (is.na(newdata[i,"steps"])) {
                newdata[i,"steps"] <- meanStepsperInterval(newdata[i,"interval"])
                flag = flag + 1
        }
}
```

*4. Make a histogram of the total number of steps taken each day and Calculate #and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?*

*What is the impact of imputing missing data on the estimates of the total daily number of steps?*
```{r}
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
```

*Are there differences in activity patterns between weekdays and weekends?*

*5.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*
```{r}
newdata$day <- ifelse(as.POSIXlt(as.Date(newdata$date))$wday%%6 == 
                              0, "Weekend", "Weekday")
newdata$day <- factor(newdata$day, levels = c("Weekday", "Weekend"))
```

*6. Make a panel plot containing a time series plot (i.e. type = "l") of the # 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*

*See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.*

*Plot made using Lattice*
```{r}
library("lattice")
newstepsInterval= aggregate(steps ~ interval + day, newdata, mean)
xyplot(steps ~ interval | factor(day), data = newstepsInterval, aspect = 1/2, 
       type = "l")
dev.off()
```

```{r}
library(knitr)
knit2html ("PA1_template.md")
browseURL("PA1_template.html")
```
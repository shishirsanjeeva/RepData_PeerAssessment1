Loading and preprocessing the data
==================================

    if(file.exists("activity.csv")!=TRUE){
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
    unzip("activity.zip")
    }
    stepdata <- read.csv("activity.csv", header = TRUE)
    head(stepdata)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Calculate total number of steps taken each day. Make a histogram of
    the total number of steps taken each day

<!-- -->

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()

    ## `summarise()` ungrouping output (override with `.groups` argument)

    png(filename="Plot_1.png")
    hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)
    dev.off()

    ## png 
    ##   2

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    mean(databydate$tsteps)

    ## [1] 10766.19

    median(databydate$tsteps)

    ## [1] 10765

What is the average daily activity pattern?
===========================================

1.  Make a time series plot of the 5-minute interval (x-axis) and the
    average number of steps taken, averaged across all days (y-axis)

<!-- -->

    library(ggplot2)
    databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 

    ## `summarise()` ungrouping output (override with `.groups` argument)

    png(filename="Plot_2.png")
    ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()
    dev.off()

    ## png 
    ##   2

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]

    ## # A tibble: 1 x 2
    ##   interval tsteps
    ##      <int>  <dbl>
    ## 1      835   206.

Imputing missing values
=======================

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    sum(is.na(stepdata$steps))

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- -->

    replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
    head(meandata)

    ## # A tibble: 6 x 3
    ## # Groups:   interval [6]
    ##    steps date       interval
    ##    <dbl> <chr>         <int>
    ## 1 1.72   2012-10-01        0
    ## 2 0.340  2012-10-01        5
    ## 3 0.132  2012-10-01       10
    ## 4 0.151  2012-10-01       15
    ## 5 0.0755 2012-10-01       20
    ## 6 2.09   2012-10-01       25

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

    names(FullSummedDataByDay)[1] ="date"
    names(FullSummedDataByDay)[2] ="totalsteps"

Summary of new data : mean & median

    summary(FullSummedDataByDay,15)

    ##      date             totalsteps   
    ##  Length:61          Min.   :   41  
    ##  Class :character   1st Qu.: 9819  
    ##  Mode  :character   Median :10766  
    ##                     Mean   :10766  
    ##                     3rd Qu.:12811  
    ##                     Max.   :21194

4 Making a histogram

    png(filename="Plot_3.png")
    hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
    dev.off()

    ## png 
    ##   2

Compare the mean and median of Old and New data

    oldmean <- mean(databydate$tsteps, na.rm = TRUE)
    newmean <- mean(FullSummedDataByDay$totalsteps)
    oldmean

    ## [1] 10766.19

    newmean

    ## [1] 10766.19

    oldmedian <- median(databydate$tsteps, na.rm = TRUE)
    newmedian <- median(FullSummedDataByDay$totalsteps)
    oldmedian

    ## [1] 10765

    newmedian

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    meandata$date <- as.Date(meandata$date)
    meandata$weekday <- weekdays(meandata$date)
    meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

1.  Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    library(ggplot2)
    meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
    names(meandataweekendweekday) <- c("weekend", "interval", "steps")
    png(filename="Plot_4.png")
    ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
      facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
      ggtitle("Comparison of Average Number of Steps in Each Interval")
    dev.off()

    ## png 
    ##   2

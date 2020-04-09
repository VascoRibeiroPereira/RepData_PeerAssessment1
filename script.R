#################################################
#                                               #
# Unclean version of the code with tests... O.O #
#                                               #
#################################################

### Loading and preprocessing the data
unzip("activity.zip")
df <- read.csv("activity.csv", sep = ",", na.strings = "NA")
sT <- knitr::kable(summary(df))

### What is mean total number of steps taken per day?

library(ggplot2)
dfC <- df[complete.cases(df),] ## subset of the df complete cases
dfCsteps <- aggregate(steps ~ date, dfC, sum)

ggptotalSteps <- ggplot(dfCsteps, (aes(steps))) + 
        geom_histogram(binwidth = 4000, col = "cadetblue", fill = "aquamarine") +
        labs(x = "Steps", y = "Frequency", title = "Total number of steps taken each day") +
        theme(plot.title = element_text(hjust = 0.5))

## Mean total steps/day
meanTotalSteps <- mean(dfCsteps$steps)

## Median total steps/day
medianTotalSteps <- median(dfCsteps$steps)

### What is the average daily activity pattern?

library(dplyr)
library(ggpmisc)

dfCstpitv <- aggregate(steps ~ interval, dfC, mean)

ggpmeanSteps <- ggplot(dfCstpitv, (aes(interval, steps))) + 
        geom_line() +
        stat_peaks(geom ="rug", col = "steelblue1", ignore_threshold = .9) +
        stat_peaks(geom="point", col = "steelblue1", ignore_threshold = .9) +
        stat_peaks(geom="text", col = "steelblue1", ignore_threshold = .9, vjust = -.5) +
        coord_cartesian(ylim = c(0, 225)) +
        ggtitle("Time series of interval and average steps across all days") +
        theme(plot.title = element_text(color = "steelblue3", hjust = 0.5))

MaxStpItrvl <- filter(dfCstpitv, steps == max(dfCstpitv$steps)) ## as shown in the graph

### Imputing missing values
colsNA <- names(df)[grepl("NA", df)] ## only the first column "steps" have NA values
rowsNA <- sum(is.na(df$steps)) ## sum of all is.na = TRUE
NAweight <- paste(as.integer(mean(is.na(df$steps))*100), "%", sep = "") ## % of NA


library(lubridate)

intervals.Day <- length(table(df$interval))

OctAverage.interval <- as.integer(aggregate(steps ~ month(date), dfCsteps, mean)[1,2]/intervals.Day)
NovAverage.interval <- as.integer(aggregate(steps ~ month(date), dfCsteps, mean)[2,2]/intervals.Day)

df.infer <- df ## create a copy of the original df to complete
df.infer$date <- as.Date(df.infer$date) ## remove the factor

for (i in 1:length(df.infer[,1])) {
        if (weekdays(df.infer[i,2]) == 10) {
                if (is.na(df.infer[i,1]) == TRUE) {
                df.infer[i,1] <- OctAverage.interval ## mean october
                }
        }
        if (month(df.infer[i,2]) == 11) {
                if (is.na(df.infer[i,1]) == TRUE) {
                df.infer[i,1] <- NovAverage.interval ## mean november
                }
        }
}

#### missing data and 0 can be considered the same??
dfsteps.infer <- aggregate(steps ~ date, df.infer, sum)

ggptotalSteps.infer <- ggplot(dfsteps.infer, (aes(steps))) + 
        geom_histogram(binwidth = 4000, col = "cadetblue", fill = "aquamarine") +
        labs(x = "Steps", y = "Frequency", title = "Total number of steps taken each day") +
        theme(plot.title = element_text(hjust = 0.5))

## Mean total steps/day
meanTotalSteps.infer <- mean(dfsteps.infer$steps)
#10751.74

## Median total steps/day
medianTotalSteps.infer <- median(dfsteps.infer$steps)
#10656


## Difference proportion to the original data
meanSimilarity <- paste(round(meanTotalSteps.infer*100/meanTotalSteps, 2), "%", sep = "")
medianSimilarity <- paste(round(medianTotalSteps.infer*100/medianTotalSteps, 2), "%", sep = "")



## Are there differences in activity patterns between weekdays and weekends?

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

df.infer <- mutate(df.infer, day = weekdays(as.Date(df.infer$date)))

for (i in 1:length(df.infer$day)) {
if (df.infer$day[i] %in% weekday){
        df.infer$day[i] <- "weekday"
} else {
        df.infer$day[i] <- "weekend"
        }
}
##df.infer$day <- as.factor(df.infer$day)

df.inferDays <- aggregate(steps ~ interval+day, df.infer, mean)


ggpmeanSteps.inferDay <- ggplot(df.inferDays, (aes(interval, steps))) + 
        geom_line() + facet_wrap(~ day, nrow = 2, ncol = 1) +
        labs(y = "Number of steps", x = "Interval")

###########
##
## Repeating NA imput. As shown, the weekdays and weekends seem to have differences that may be good taking in account, for example.
##
#########
newDF <- df ## copy the data

stpsbyWday <- aggregate(steps ~ weekdays(as.Date(date)), dfC, mean) ## mean number of steps per day
names(stpsbyWday) <- c("day", "steps")

newDF$date <- as.Date(newDF$date)

for (i in 1:length(newDF[,1])) {
        if (is.na(newDF[i,1]) == TRUE) {
                tempDay <- (weekdays(newDF[i,2]))
                newDF[i,1] <- stpsbyWday$steps[grepl(tempDay, stpsbyWday$day)]
        }
}

dfsteps.newDF <- aggregate(steps ~ date, newDF, sum)

## Mean total steps/day newDF
meanTotalStepsNEW.DF <- mean(dfsteps.newDF$steps)
#10821.21

## Median total steps/day newDF
medianTotalStepsNEW.DF <- median(dfsteps.newDF$steps)
#11015


## Difference proportion to the original data
meanSimilarity.new <- paste(round(meanTotalStepsNEW.DF*100/meanTotalSteps, 2), "%", sep = "")
medianSimilarity.new <- paste(round(medianTotalStepsNEW.DF*100/medianTotalSteps, 2), "%", sep = "")

## The plot is more evenly affetcted in this imput approach

ggptdfsteps.newDF <- ggplot(dfsteps.newDF, (aes(steps))) + 
        geom_histogram(binwidth = 4000, col = "cadetblue", fill = "aquamarine") +
        labs(x = "Steps", y = "Frequency", title = "Total number of steps taken each day") +
        theme(plot.title = element_text(hjust = 0.5))


## NEW ANSWER: Are there differences in activity patterns between weekdays and weekends?

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

newDF <- mutate(newDF, day = weekdays(as.Date(newDF$date)))

for (i in 1:length(newDF$day)) {
        if (newDF$day[i] %in% weekday){
                newDF$day[i] <- "weekday"
        } else {
                newDF$day[i] <- "weekend"
        }
}

newDF_Days <- aggregate(steps ~ interval+day, newDF, mean)


ggp.newDF_Days <- ggplot(newDF_Days, (aes(interval, steps))) + 
        geom_line() + facet_wrap(~ day, nrow = 2, ncol = 1) +
        labs(y = "Number of steps", x = "Interval")

library(knitr)
knit2html("PA1_template.Rmd")
rmarkdown::render("PA1_template.Rmd")


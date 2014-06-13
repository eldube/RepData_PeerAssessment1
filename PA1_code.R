## PART I
datafile <- "activity.csv"
## Download and unzip file from provided URL
if(!file.exists(datafile)) {
  fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  filename <- "repdata-data-activity.zip"
  download.file(fileurl, destfile = filename, method = "internal")
  unzip(filename)
}

#stop if downloading failed or activity.csv file not in working directory
if(!file.exists(datafile)) {
  stop("Error: activity.csv is not in the working directory.")
}

##open project data from unzipped file source


projData <- read.csv(datafile, colClasses = c("numeric", "Date", "numeric"))

## change integer interval to 4 digit string intervals, and date as Date()
projData$intervalFactor <-   factor(sprintf("%04d", projData$interval))
projData$date <- as.Date (projData$date, "%Y-%m-%d")

## PART 2

## calculate mean total number of steps
meanTotalNumberofSteps <- mean(projData$steps, na.rm=TRUE) 

## calculate meadian total number of steps
medianTotalNumberofSteps <- median(projData$steps, na.rm=TRUE)


print(c (meanTotalNumberofSteps,medianTotalNumberofSteps) )

## DRAW HISTOGRAM
library(ggplot2)
hist(projData$steps, col="blue", main = "Total Number of Steps", 
     xlab = "Total Number of Steps per day")

#qplot( steps, data= dayTypepatternData, binwidth=50, main = "Total Number of Steps", 
#      xlab = "Total Number of Steps per day")

## PART 3
## plot daily activity pattern

## Average number of steps across each interval
##old avgDailyActivity<- tapply (projData$steps, projData$stringInterval, mean, na.rm=TRUE)


## create data frame for intervals and averages for each interval
##old avgpatternData <- data.frame(interval= names(avgDailyActivity), average = avgDailyActivity)
library(plyr)
avgpatternData <- ddply(projData, "intervalFactor", transform, average = mean(steps, na.rm=TRUE))


## plot intervals against averages

plot(avgpatternData$interval, avgpatternData$average, type="l", 
     ylab = "Averge steps per day ", xlab="Interval", 
     main= "Daily Activity Pattern - part 3")

## get interval with maximum number of steps, on average
maxInterval <- avgpatternData[(avgpatternData$average>= max(avgpatternData$average)),]
print( maxInterval[1,]$interval)

##======================== correctly done ===============================================

##PART 4



## 2. Fill in missing number of steps using median for the day
## there is no data for 2012, so replace number of steps all with overall average = 37
## get medians for each day, where median cannot be calculated (no data for the day),
## replace with overall median value = 0.



##3. From above, we observe the median is 0 for all days, 
## Transform dataframe by replacing NAs in number of steps by average number of steps
## in the corresponding time interval 
filledProjData <-  ddply(projData, .(intervalFactor), transform, intervalMedian = median(steps, na.rm=TRUE))
filledProjData$steps <- with( filledProjData, ifelse( is.na(steps), intervalMedian , steps) )

##======================== correctly done ===============================================
##4. 

## DRAW HISTOGRAM
hist(filledProjData$steps, col="blue", main = "Total Number of Steps", 
     xlab = "Total Number of Steps per day")

# calculate mean total number of steps
meanTotalNumberofSteps <- mean(filledProjData$steps, na.rm=TRUE) 

## calculate meadian total number of steps
medianTotalNumberofSteps <- median(filledProjData$steps, na.rm=TRUE)


print(c (meanTotalNumberofSteps,medianTotalNumberofSteps) )

## comparison : means is lower (32.4799), median same (==0)


## Part 5

dayTypepatternData <- filledProjData
## add day of the week variable, and day type variable
dayTypepatternData$day <- weekdays(filledProjData$date)
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
dayTypepatternData$daytype <- factor(ifelse(dayTypepatternData$day %in% weekend, "weekend", "weekday"))

dayTypepatternData$daytype = with(dayTypepatternData, factor(daytype, levels = rev(levels(daytype))))


## claculate interval averages grouped by day type 
dayTypepatternData <-  ddply(dayTypepatternData, .(intervalFactor,daytype), transform, average = median(steps, na.rm=TRUE))
library(ggplot2)
require(grid)
g <- qplot( interval, average, data= dayTypepatternData, geom="line")+ facet_wrap(~daytype, ncol=1)+
  theme(panel.margin = unit(1.5, "lines")) + theme(strip.text.y = element_text(colour = 'blue', size = 16))
print(g)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
# Set setwd() to working directory containing data
setwd("C:/Users/nkd29/Desktop/Nikhils_Stuff/01_Career/03_Courses/Coursera_R_Data_Science_Johns_Hopkins/05_Repoducible_Research/Project_1/RepData_PeerAssessment1")

list.files()

data <- tbl_df(read.csv('activity.csv'))
# data <- na.omit(data)
# View(data)

# Q1: What is mean total number of steps taken per day? For this part of the assignment, you can ignore the missing values in the dataset.
# 1. Calculate the total number of steps taken per day
# 2. Make a histogram of the total number of steps taken each day
# 3. Calculate and report the mean and median of the total number of steps taken per day

# total steps
tot_steps <- sum(data$steps,na.rm = TRUE)
tot_steps

# histogram
ggplot(na.omit(data), aes(x=steps)) + geom_histogram()

sum(data$steps==0, na.rm = TRUE)   # matches histogram

# mean and median
mean(data$steps, na.rm = TRUE)
median(data$steps, na.rm = TRUE)


# Q2: What is the average daily activity pattern?
#   Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of 
#  the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


# time series plot
data_by_interval <- group_by(na.omit(data),interval)
avg_steps <- summarize(data_by_interval,mean(steps))

ggplot(avg_steps,aes(x=interval,y=`mean(steps)`)) + geom_line()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum 
# number of steps?
max_avg_steps <- max(avg_steps$`mean(steps)`)
max_avg_steps

max_avg_int <- avg_steps$interval[match(max_avg_steps, avg_steps$`mean(steps)`)]
max_avg_int



# Q3: Impute NA values (missing values), 
# Note that there are a number of days/intervals where there are missing values 
# (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias
# into some calculations or summaries of the data.


# get total number of NA values
tot_na <- sum(is.na(data$steps))
tot_na

# replace NA values with avg for that specific interval 
# data_rep <- data %>% mutate(steps=replace_na(steps,"bazingo"))


data2 <- data

# add index column
data2 <- data2 %>% mutate(int_avg_index=match(data$interval, avg_steps$interval)) 

# use index to lookup calculated averages for each interval. Put into new column
data2$int_mean <- avg_steps$`mean(steps)`[data2$int_avg_index]

# replace only NA values with the looked up averages
data2$steps[which(is.na(data2$steps))] <- data2$int_mean

# plot again: histogram
ggplot(data2, aes(x=steps)) + geom_histogram()

# mean and median: NO DIFFERENCE BETWEEN IMPUTED MISSING DATA VS. ORIGINAL DATA
mean(data2$steps)
median(data2$steps)

mean(data$steps,na.rm = TRUE)
median(data$steps)


# Q4: Are there differences in activity patterns between weekdays and weekends?

data2$day <- wday(ymd(data2$date),label=TRUE)

# data2$is_wknd <- ifelse(data2$day=)
data2$wknd <- ifelse(data2$day == "Sat"|data2$day=="Sun","weekend","weekday")

data2_by_int_wday <- data2 %>% filter(wknd=="weekday") %>% group_by(interval) %>% summarise(mean(steps))
data2_by_int_wknd <- data2 %>% filter(wknd=="weekend") %>% group_by(interval) %>% summarise(mean(steps))

data2_by_int_wday
data2_by_int_wknd

p_wday <- ggplot(data2_by_int_wday,aes(x=interval,y=`mean(steps)`)) + geom_line()

p_wknd <- ggplot(data2_by_int_wknd,aes(x=interval,y=`mean(steps)`)) + geom_line()

ggarrange(p_wday,p_wknd,ncol=1,nrow=2)

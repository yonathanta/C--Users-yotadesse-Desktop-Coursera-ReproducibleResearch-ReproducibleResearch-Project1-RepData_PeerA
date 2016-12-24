library(lattice)
#Loading data
thefile<- "C:/Users/yotadesse/Desktop/Coursera/ReproducibleResearch/ReproducibleResearch/Project1/RepData_PeerAssessment1/Activity.zip"
data<- read.csv(unzip(thefile, "activity.csv"))

#Removing missing values
data1<-na.omit(data)

## What is mean total number of steps taken per day?

#1. Make a histogram of the total number of steps taken each day
total_steps_perday<-tapply(data1$steps,data1$date,sum)
total_steps_perday<-as.data.frame(total_steps_perday); colnames(total_steps_perday)<-c("steps")

hist(total_steps_perday$steps, col="Brown", main="Total steps per day", xlab="")

#2. Calculate and report the mean and median total numbe of steps taken per day

mean(total_steps_perday$steps, na.rm=T)
median(total_steps_perday$steps, na.rm=T)


##What is the average daily activity pattern?

#1. Make a time series plot of the 5-minute interval(x-axis and the average number of steps taken, averaged accross all days(y-axis))

#  meandata<- na.omit(tapply(data1$steps, data1$interval, mean))
# plot(meandata, type="l", xaxt='n')

plot(tapply(data1$steps, data1$interval, mean), type="l", xaxt = 'n')
axis(1, labels = c('0:00', '5:00', '10:00', '15:00', '20:00'), at = c(0, 50, 100, 150, 200))

#2. Which 5-minute interval, on average across all the days is the dataset, contains the maximum number of steps?

data1$date[max(tapply(data1$steps, data1$interval, mean))]

# the above needs improvement

## Imputing missing values

# 1. Calculate and report the total number of missing values 
#    in the dataset (i.e. the total number of rows with NAs)

sum(is.na(data))

#2. my strategies for filling the missing values are: 
        #i) to use global average if the number of missing values
        #    per day is more than half of the available data for the day
        #2) to use the average for the day is the number of missing values is less than half
    
perday<-split(data,data$date)
newdata<-data.frame()
for(i in 1:length(perday)){
        #imputing using mean
        datatemp<-perday[[i]]
        #if the missing values are more than half of the daily observations
        #I impute the missing values with the global(overall) mean
        {if (sum(is.na(datatemp$steps))>nrow(datatemp)/2){
                missing<-is.na(datatemp$steps)
                datatemp$steps[missing]<- mean(as.numeric(data$steps), na.rm=TRUE)
        }else {
                # if the missing values are less than half of the daily observation
                #I impute the missing values with the average of the available daily observation excluding the NA
                missing<- is.na(datatemp$steps)
                datatemp$steps[missing]<- mean(as.numeric(datatemp$steps), na.rm=TRUE)
                }}
        
        newdata<- rbind(newdata,datatemp)
}

#3. the 'newdata' at the end of the above for loop contains the original dataset with the missing data filled
sum(is.na(newdata))

#4 

# total_steps_perday_newdata<-tapply(newdata$steps,newdata$interval,sum)
# total_steps_perday_newdata<-as.data.frame(total_steps_perday_newdata); colnames(total_steps_Perday_newdata)<-c("steps")
# 
# hist(as.numeric(total_steps_perday_newdata$steps), col="darkgreen", main="Total steps per day", xlab="")
hist(tapply(newdata$steps,newdata$date,sum), col="lightblue", main="Total steps per day after filling the missing values", xlab="")
# check this graph. It looks significantly different from the above
        mean(tapply(newdata$steps,newdata$interval,sum))
        median(tapply(newdata$steps,newdata$interval,sum))


## Are there differences in activity patterns between weekdays and weedends?

#1. create a new variable

newdata$dtfactor<-""

newdata[(weekdays(as.Date(newdata$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]$dtfactor<-"weekday"
newdata[(weekdays(as.Date(newdata$date)) %in% c("Saturday","Sunday" )),]$dtfactor<-"weekend"
agg_newdata<- aggregate(newdata$steps, by=list(newdata$interval, newdata$dtfactor), mean)
colnames(agg_newdata)<-c("interval","datefactor", "meansteps")
#2. make a pannel ploting containing a time series plot
# newdata1<-tapply(newdata$steps, newdata$interval, mean)

xyplot(meansteps~interval|datefactor, data=agg_newdata, layout=c(1,2), type="l", ylab="Number of steps")


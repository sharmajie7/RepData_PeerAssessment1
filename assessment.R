
---
        title: "Activity Analysis"
output: html_document
---
        
        ```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```
## Loading and preprocessing the data

```{r}
activity<-read.csv("activity.csv")

```


**Mean total number of steps taken per day**
        ```{r histogram,fig.height=4}
steps_day<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
hist(steps_day,col="sky blue",main = "Total number of steps taken per day",xlab="Number of steps",ylab="Frequency")
mean_step<-mean(steps_day)
median_step<-median(steps_day)
```
The mean is `r mean_step` and the median is `r median_step` 



##Average daily activity pattern

```{r}
library(ggplot2)
avg_steps<-aggregate(x = list(steps = activity$steps), by = list(interval = activity$interval), FUN = mean, na.rm = TRUE)
ggplot(data=avg_steps,aes(x=interval,y=steps))+geom_line()+xlab("5 minute time interval")+ylab("Steps taken each day")
```


**Interval with the maximum number of steps**
        ```{r}
avg_steps[which.max(avg_steps$steps),]
```



##Imputing missing values

**Finding column which contains the NA values** 
        
        ```{r}
list_na <- colnames(activity)[ apply(activity, 2, anyNA) ]
```
The column with missing values is `r list_na`


**Number of missing Values**
        ```{r}

num_na<-sum(is.na(activity))

```
The number of missing values is `r num_na`


**Exclude missing values**
        ```{r}

na_drop<-na.omit(activity)		
na_drop
```


**Replacing the rows with missing values with the mean of steps**
        
        ```{r}
library(dplyr)
average_missing <- apply(activity[,colnames(activity) %in% list_na,drop=F],2,FUN= mean, na.rm =  TRUE)
average_missing

activity_replace <- activity %>%  mutate(replace_mean_step  = ifelse(is.na(steps), average_missing[1], steps)) 

activity_new = subset(activity_replace,select=-c(steps)) 
c_Act<-colnames(activity_new)
```
The new dataset includes `r c_Act`



**Histogram**
        ```{r histogram2}
steps_day_new<-tapply(activity_new$replace_mean_step,activity_new$date,sum,na.rm=TRUE)
par(mar=c(4,4,1,1))
hist(steps_day_new,col="dark blue",main = "Total number of steps taken per day",xlab="Number of steps",ylab="Frequency")
mean_step_new<-mean(steps_day_new)
median_step_new<-median(steps_day_new)
```
The new mean is `r mean_step_new`  
The new median is `r median_step_new`


##Activity patterns on weekdays and weekends

**Indicate whether a given data is a weekday or weekend-day**
        
        ```{r}
activity_new$date <- as.Date(activity_new$date)
#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
activity_new$wDay <- factor((weekdays(activity_new$date) %in% weekdays1), 
                            levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

```


**Time Series plot according to the type of day,i.e Weekday or Weekend**
        
        ```{r timeseries}
wDay.df <- activity_new %>% group_by(interval,wDay) %>% summarise(mean.step=mean(replace_mean_step))


wDay_pattern <- ggplot(wDay.df, aes(x=interval, y=mean.step, color=wDay)) + 
        facet_grid(wDay~.) +
        geom_line() + 
        labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
wDay_pattern
```                  

-----------------------------------------------------------------------------------
        
        
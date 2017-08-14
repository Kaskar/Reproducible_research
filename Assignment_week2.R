library(reshape2)
setwd("/Users/kasperkarlsson/_Stanford/Courses/Reproducible_Research/")
data = read.csv("activity.csv")
data_mean <- aggregate(steps ~ date, data, mean)
colnames(data_mean) <- c("date","step_mean")
head(data_mean)
data_nona <- subset(data,data$steps != "NA")
length(data$steps)
length(data_nona$steps)
head(data_nona)
sum(data_nona$steps)
new_data_sum <- aggregate(steps ~ date, data_nona, sum)
colnames(new_data_sum) <- c("date","step_sum")
hist(new_data_sum$step_sum)
new_data_mean <- aggregate(steps ~ date, data_nona, mean)
colnames(new_data_mean) <- c("date","step_mean")
new_data_median <- aggregate(steps ~ date, data_nona, median)
colnames(new_data_median) <- c("date","step_median")

new_data_summean <- merge(new_data_sum,new_data_mean,by.x = "date")
new_data_combined <-merge(new_data_summean,new_data_median,by.x = "date")
new_data_combined

plot(new_data_combined$step_mean, type="l")

new_data_fivemin_mean <- aggregate(steps ~ interval, data, mean)
colnames(new_data_fivemin_mean) <- c("interval","step_mean")
head(new_data_fivemin_mean)

plot(new_data_fivemin_mean$interval,new_data_fivemin_mean$steps, type="l")
top <- subset(new_data_fivemin_mean, steps==max(new_data_fivemin_mean$steps))
top$interval

head(data,500)
sum(complete.cases(data)) 
sum(!complete.cases(data)) 

data_nona <- merge(data, new_data_fivemin_mean, by  = "interval")
head(data_nona)

?weekdays()
head(data_nona)
data_nona$day <- weekdays(as.Date(data_nona$date))
data_nona$weekday <- ifelse(data_nona$day=="Saturday", "Weekend",
                     ifelse(data_nona$day=="Sunday", "Weekend",
                                  "Weekday"))

new_data2 <- aggregate(steps ~ interval+weekday, data_nona, mean)
head(new_data2)
plot(new_data2$interval,new_data2$steps,type="l",col = "red")
?plot()

new_data3 <- melt(new_data2, id.vars = c("interval","weekday"))
head(new_data2)
head(new_data3)
ggplot(new_data2, aes(x=interval,y=steps,group=weekday,color=weekday))+
  geom_line(size = 0.5,alpha = 0.9) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = 'white'))



data_nona$steps[is.na(data_means$steps)] <- data_means$step_mean[is.na(data_means$steps)]
data_nona_test <- subset(data_means,data_means$steps != "NA")
length(data$steps)
length(data_nona$steps)
length(data_mean_nona$steps)

test1 <- subset(data,date != "2012-10-01")
test2 <- subset(data,steps != "NA")
test3 <- subset(data,steps == "Na")
head(test3)
head(data)
length(data$steps)
length(test1$steps)
length(test2$steps)
head(test1,100)
data_mean

test4 <-aggregate(steps ~ date, test2, FUN = length)
test4


new_data <- aggregate(steps ~ interval, data, mean)
colnames(new_data) <- c("interval","step_mean")
head(new_data)
plot(new_data$interval,new_data$step_mean, type="l")


data_nona <- merge(data, new_data, by  = "interval")
data_nona$steps[is.na(data_nona$steps)] <- data_nona$step_mean[is.na(data_nona$steps)]
head(data_nona)

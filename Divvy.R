library(tidyverse)
library(lubridate)

# Train ------------------------------------------------------
trips_2013 <- read_csv("Divvy_Trips_2013.csv")
names(trips_2013)[names(trips_2013) == "birthday"] <- "birthyear"
trips_2013$starttime <- as_date(trips_2013$starttime)
trips_2013$stoptime <- as_date(trips_2013$stoptime)
trips_2014_Q1Q2 <- read_csv("Divvy_Trips_2014_Q1Q2.csv")
trips_2014_Q3_07 <- read_csv("Divvy_Trips_2014-Q3-07.csv")
trips_2014_Q3_0809 <- read_csv("Divvy_Trips_2014-Q3-0809.csv")
trips_2014_Q4 <- read_csv("Divvy_Trips_2014-Q4.csv")
trips_2015_Q1 <- read_csv("Divvy_Trips_2015-Q1.csv")
trips_2015_Q2 <- read_csv("Divvy_Trips_2015-Q2.csv")
trips_2015_Q3_07 <- read_csv("Divvy_Trips_2015_07.csv")
trips_2015_Q3_08 <- read_csv("Divvy_Trips_2015_08.csv")
trips_2015_Q3_09 <- read_csv("Divvy_Trips_2015_09.csv")
trips_2015_Q4 <- read_csv("Divvy_Trips_2015_Q4.csv")
trips_2016_Q1 <- read_csv("Divvy_Trips_2016_Q1.csv")
trips_2016_Q2_04 <- read_csv("Divvy_Trips_2016_04.csv")
trips_2016_Q2_05 <- read_csv("Divvy_Trips_2016_05.csv")
trips_2016_Q2_06 <- read_csv("Divvy_Trips_2016_06.csv")
trips_2016_Q3 <- read_csv("Divvy_Trips_2016_Q3.csv")
trips_2016_Q4 <- read_csv("Divvy_Trips_2016_Q4.csv")
trips <- rbind(trips_2014_Q1Q2, trips_2014_Q3_07, trips_2014_Q3_0809, trips_2014_Q4, 
               trips_2015_Q1, trips_2015_Q2, trips_2015_Q3_07, trips_2015_Q3_08, trips_2015_Q3_09, trips_2015_Q4, 
               trips_2016_Q1, trips_2016_Q2_04, trips_2016_Q2_05, trips_2016_Q2_06)
trips$starttime <- as_date(mdy_hm(trips$starttime))
trips$stoptime <- as_date(mdy_hm(trips$stoptime))
trips_2016_Q3Q4 <- rbind(trips_2016_Q3, trips_2016_Q4)
trips_2016_Q3Q4$starttime <- as_date(mdy_hms(trips_2016_Q3Q4$starttime))
trips_2016_Q3Q4$stoptime <- as_date(mdy_hms(trips_2016_Q3Q4$stoptime))
trips <- rbind(trips_2013, trips, trips_2016_Q3Q4)

names(trips)[names(trips) == "starttime"] <- "Date"
daily <- trips %>%
  group_by(Date) %>% 
  summarise(Trips = n())
ggplot(daily, aes(Date, Trips)) + geom_line()

daily <- daily %>% 
  mutate(Wday = wday(Date, label = TRUE))
ggplot(daily, aes(Wday, Trips)) + geom_boxplot()

trips[trips$usertype == "Dependent",]$usertype <- "Subscriber"
daily2 <- trips %>% 
  group_by(Date, usertype) %>% 
  summarise(Trips = n()) %>% 
  mutate(Wday = wday(Date, label = TRUE))
ggplot(daily2, aes(Wday, Trips, color = usertype)) + geom_boxplot()

daily$Quarter <- 1
daily$Quarter[month(daily$Date) %in% 4:6] <- 2
daily$Quarter[month(daily$Date) %in% 7:9] <- 3
daily$Quarter[month(daily$Date) %in% 10:12] <- 4
daily$Quarter <- as.factor(daily$Quarter)

weather <- read_csv("chicagoweather.csv")
weather$Date <- as_date(dmy(weather$Date))
weather$Precipitation[weather$Precipitation == "T"] <- 0
weather$Precipitation <- as.numeric(weather$Precipitation)
weather[which(is.na(weather$Events)),]$Events <- "None"
weather$Events <- as.factor(weather$Events)
levels(weather$Events)
weather$Events <- gsub("Fog|Fog\\r\\t,\\rRain|Rain", "Rain", weather$Events)
weather$Events <- gsub("Fog\\r\\t,\\rRain\\r\\t,\\rSnow|Fog\\r\\t,\\rSnow|Rain\\r\\t,\\rSnow|Snow", "Snow", weather$Events)
weather$Events <- gsub("Fog\\r\\t,\\rRain\\r\\t,\\rThunderstorm|Rain\\r\\t,\\rHail\\r\\t,\\rThunderstorm|Rain\\r\\t,\\rThunderstorm|Thunderstorm", "Thunderstorm", weather$Events)
weather$Events <- as.factor(weather$Events)
levels(weather$Events)

train <- weather %>% 
  left_join(daily, by = "Date")

View(train[which(is.na(train$Trips)),])
which(is.na(train$Trips))
train[195, "Wday"] <- "Tues"
train[196, "Wday"] <- "Wed"
train[which(is.na(train$Quarter)),]$Quarter <- 1
train[which(is.na(train$Trips)),]$Trips <- 0


# Train2 -----------------------------------------------------
trips <- rbind(trips_2014_Q1Q2, trips_2014_Q3_07, trips_2014_Q3_0809, trips_2014_Q4, 
               trips_2015_Q1, trips_2015_Q2, trips_2015_Q3_07, trips_2015_Q3_08, trips_2015_Q3_09, trips_2015_Q4, 
               trips_2016_Q1, trips_2016_Q2_04, trips_2016_Q2_05, trips_2016_Q2_06)
trips$starttime <- as_date(mdy_hm(trips$starttime))
trips$stoptime <- as_date(mdy_hm(trips$stoptime))
trips_2016_Q3Q4 <- rbind(trips_2016_Q3, trips_2016_Q4)
trips_2016_Q3Q4$starttime <- as_date(mdy_hms(trips_2016_Q3Q4$starttime))
trips_2016_Q3Q4$stoptime <- as_date(mdy_hms(trips_2016_Q3Q4$stoptime))
trips <- rbind(trips, trips_2016_Q3Q4)

names(trips)[names(trips) == "starttime"] <- "Date"
daily <- trips %>%
  group_by(Date) %>% 
  summarise(Trips = n()) %>% 
  mutate(Wday = wday(Date, label = TRUE))

daily$Quarter <- 1
daily$Quarter[month(daily$Date) %in% 4:6] <- 2
daily$Quarter[month(daily$Date) %in% 7:9] <- 3
daily$Quarter[month(daily$Date) %in% 10:12] <- 4
daily$Quarter <- as.factor(daily$Quarter)

weather <- filter(weather, year(weather$Date) %in% c(2014, 2015, 2016))
train <- weather %>% 
  left_join(daily, by = "Date")
which(is.na(train$Trips))
train[7, "Wday"] <- "Tues"
train[8, "Wday"] <- "Wed"
train[which(is.na(train$Quarter)),]$Quarter <- 1
train[which(is.na(train$Trips)),]$Trips <- 0

write.csv(train, file = "train.csv", row.names = FALSE)

# Test -------------------------------------------------------
weather_2017 <- read_csv("chicagoweather2017.csv")
weather_2017$Date <- as_date(dmy(weather_2017$Date))
weather_2017[which(is.na(weather_2017$Precipitation)),]$Precipitation <- 0
weather_2017$Precipitation <- as.numeric(weather_2017$Precipitation)
weather_2017[which(is.na(weather_2017$Events)),]$Events <- "None"
weather_2017$Events <- as.factor(weather_2017$Events)
levels(weather_2017$Events)
weather_2017$Events <- gsub("Fog|Fog , Rain|Rain", "Rain", weather_2017$Events)
weather_2017$Events <- gsub("Fog , Rain , Snow|Fog , Snow|Rain , Snow|Snow", "Snow", weather_2017$Events)
weather_2017$Events <- gsub("Fog , Rain , Thunderstorm|Fog , Snow , Thunderstorm|Rain , Hail , Thunderstorm|Rain , Thunderstorm|Thunderstorm|Snow , Thunderstorm", "Thunderstorm", weather_2017$Events)
weather_2017$Events <- as.factor(weather_2017$Events)
levels(weather_2017$Events)

trips_2017_Q1 <- read_csv("Divvy_Trips_2017_Q1.csv")
trips_2017_Q2 <- read_csv("Divvy_Trips_2017_Q2.csv")
trips_2017 <- rbind(trips_2017_Q1, trips_2017_Q2)

trips_2017$Date <- as_date(mdy_hms(trips_2017$start_time))
daily3 <- trips_2017 %>% 
  group_by(Date) %>% 
  summarise(Trips = n()) %>% 
  mutate(Wday = wday(Date, label = TRUE))
ggplot(daily3, aes(Date, Trips)) + geom_line()

daily3$Quarter <- 1
daily3$Quarter[month(daily3$Date) %in% 4:6] <- 2
daily3$Quarter[month(daily3$Date) %in% 7:9] <- 3
daily3$Quarter[month(daily3$Date) %in% 10:12] <- 4
daily3$Quarter <- as.factor(daily3$Quarter)
levels(daily3$Quarter) <- levels(train$Quarter)

test <- weather_2017 %>% 
  left_join(daily3, by = "Date")

write.csv(test, file = "test.csv", row.names = FALSE)

# Model ------------------------------------------------------
lm.fit <- lm(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday * Quarter, train)
summary(lm.fit)
sqrt(mean((test$Trips - predict(lm.fit, test))^2))
test$yhat <- predict(lm.fit, test)

(x <- ggplot(test) + 
  geom_line(aes(Date, Trips)) +
  geom_point(aes(Date, yhat)))

par(mfrow=c(2,2))
plot(lm.fit)

lm.fit2 <- lm(log2(Trips+1) ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, train)
summary(lm.fit2)
plot(lm.fit2)

library(leaps)
regfit.full <- regsubsets(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, train)
reg.summary <- summary(regfit.full)
reg.summary$rsq
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
which.min(reg.summary$cp)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
which.min(reg.summary$bic)

library(tree)
set.seed(11)
tree.divvy <- tree(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, train)
summary(tree.divvy)
par(mfrow=c(1, 1))
plot(tree.divvy)
text(tree.divvy, pretty = 0)
cv.divvy <- cv.tree(tree.divvy)
plot(cv.divvy$size, cv.divvy$dev, type='b')
sqrt(mean((predict(tree.divvy, newdata = test) - test$Trips)^2))
test$yhat <- predict(tree.divvy, newdata = test)

library(rpart)
tree.divvy2 <- rpart(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, data = train, method = "anova")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree.divvy2)
test$yhat <- predict(tree.divvy2, newdata = test)
sqrt(mean((predict(tree.divvy2, newdata = test) - test$Trips)^2))
tree.divvy3 <- rpart(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, data = train, method = "anova", control = rpart.control(minsplit=10,cp=0.005))
fancyRpartPlot(tree.divvy3)
sqrt(mean((predict(tree.divvy3, newdata = test) - test$Trips)^2))
test$yhat <- predict(tree.divvy3, newdata = test)

library(randomForest)
set.seed(11)
bag.divvy <- randomForest(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, train, mtry=9, importance=TRUE)
sqrt(mean((predict(bag.divvy, newdata = test) - test$Trips)^2))
test$yhat <- predict(bag.divvy, newdata = test)
rf.divvy <- randomForest(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, train, mtry=3, importance=TRUE)
sqrt(mean((predict(rf.divvy, newdata = test) - test$Trips)^2))
test$yhat <- predict(rf.divvy, newdata = test)
importance(rf.divvy)
varImpPlot(rf.divvy)

library(party)
set.seed(11)
cf.divvy <- cforest(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, train, controls = cforest_unbiased(ntree = 2000, mtry = 3))
sqrt(mean((predict(cf.divvy, test, OOB=TRUE, type = "response") - test$Trips)^2))
test$yhat <- predict(cf.divvy, test, OOB=TRUE, type = "response")

library(gbm)
set.seed(11)
boost.divvy <- gbm(Trips ~ Temperature_Avg + Dew_Point_Avg + Humidity_Avg + Sea_Level_Press_Avg + Visibility_Avg + Wind_Avg + Precipitation + Events + Wday + Quarter, train, distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.divvy)
sqrt(mean((predict(boost.divvy, newdata = test,n.trees=5000) - test$Trips)^2))
test$yhat <- predict(boost.divvy, newdata=test, n.trees=5000)

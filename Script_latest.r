library(data.table)
library(dplyr)
library(rpart)

train <- fread("train.csv")
test <- fread("test.csv")

train2 <- train
test2 <- test

#Replace the only 2 missing values with 0, as it is the mode of ConversionPayOut
train2$ConversionPayOut[which(is.na(train2$ConversionPayOut))]<- 0

#Extract DayOfWeek,HourOfDay,MinuteOfHour from both train and test
train2 <- train2 %>% mutate(DayOfWeek=as.factor(weekdays(as.Date(ClickDate))))
train2 <- train2 %>% mutate(HourOfDay=as.factor(format(as.POSIXct(ClickDate,format="%Y-%m-%d %H:%M:%S"),"%H")))
train2 <- train2 %>% mutate(MinuteOfHour=as.factor(format(as.POSIXct(ClickDate,format="%Y-%m-%d %H:%M:%S"),"%M")))

test2 <- test2 %>% mutate(DayOfWeek=as.factor(weekdays(as.Date(ClickDate))))
test2 <- test2 %>% mutate(HourOfDay=as.factor(format(as.POSIXct(ClickDate,format="%Y-%m-%d %H:%M:%S"),"%H")))
test2 <- test2 %>% mutate(MinuteOfHour=as.factor(format(as.POSIXct(ClickDate,format="%Y-%m-%d %H:%M:%S"),"%M")))

#Convert Country,TrafficType,DayOfWeek,HourOfDay,MinuteOfHour to factors
train2$Country <- as.factor(train2$Country)
train2$TrafficType <- as.factor(train2$TrafficType)
train2$DayOfWeek <- as.factor(train2$DayOfWeek)
train2$HourOfDay <- as.factor(train2$HourOfDay)
train2$MinuteOfHour <- as.factor(train2$MinuteOfHour)

#Build a decision tree based on only these factors
dtree <-rpart(ConversionPayOut~Country+TrafficType+Fraud+DayOfWeek+HourOfDay+MinuteOfHour,train2,weight=train2$weights,control=rpart.control(cp=0.000000000001,maxdepth=6))

#Converting fraud predictions to 0
train2$predictions <- predict(dtree,train2[,c("Country","TrafficType","Fraud","DayOfWeek","HourOfDay","MinuteOfHour")])
train2$predictions <- ifelse(train2$Fraud==1,0,train2$predictions)

#Calculating the base weighted rmse with all predictions as zeros
weight_rmse_base <- sqrt(mean(test2$weights*(0-test2$ConversionPayOut)^2))*1000#to compare improvement
weight_rmse <- sqrt(mean(train2$weights*(train2$predictions-train2$ConversionPayOut)^2))*1000

#Predict on test with the built decision tree
test2$predictions <- predict(dtree,test2[,c("Country","TrafficType","Fraud","DayOfWeek","HourOfDay","MinuteOfHour")])
test2$predictions[which(test$Fraud==1)] <-0

#Computing the advertisement metrics
advertise_metrics <- train %>% group_by(advertiserCampaignId)%>% summarize(total_clicks_ad=n(),total_ip_ad=n_distinct(UserIp),conversion_per_ad=sum(ConversionStatus),mean_ad_payout=mean(ConversionPayOut))

#Computing the advertisement metrics
publisher_metrics <- train %>% group_by(publisherId)%>% summarize(total_clicks_pb=n(),total_ip_pb=n_distinct(UserIp),conversion_per_pb=sum(ConversionStatus),mean_pb_payout=mean(ConversionPayOut))

#Computing userip metrics
userip_metrics <- train %>% group_by(UserIp)%>% summarize(total_clicks_userip=n(),conversion_per_userip=sum(ConversionStatus),mean_userip_payout=mean(ConversionPayOut))

#Join publsher metrics on to the main dataset
train3 <- train2 %>% left_join(publisher_metrics) %>% left_join(advertise_metrics)%>%left_join(userip_metrics)

#Sample around 10% of the data and build a model with the new data frame 
train3_samp_train <- train3[sample(1:nrow(train3),6000000),]
train3_samp_test <- train3[sample(1:nrow(train3),6000000),]

#Build a decision tree with the new factors
dtree <- rpart(ConversionPayOut~Country+TrafficType+Fraud+DayOfWeek+HourOfDay+MinuteOfHour+total_clicks_pb+total_ip_pb+conversion_per_pb+mean_pb_payout,train3_samp_train,weight=train3_samp_train$weights,control=rpart.control(cp=0.000000000001,maxdepth=6))
train3_samp_test$predictions <- predict(dtree,train3_samp_test[,c("Country","TrafficType","Fraud","DayOfWeek","HourOfDay","MinuteOfHour","total_clicks_pb","total_ip_pb","conversion_per_pb","mean_pb_payout")])
train3_samp_test$predictions[which(train3_samp_test$Fraud==1)] <-0

#Calculating base weighted RMSE and weighted RMSE of predictions
#This did not improve the predictions
weight_rmse_base_samp <- sqrt(mean(train3_samp_test$weights*(0-train3_samp_test$ConversionPayOut)^2))*1000
weight_rmse_samp <- sqrt(mean(train3_samp_test$weights*(train3_samp_test$predictions-train3_samp_test$ConversionPayOut)^2))*1000

#Sample around 10% of the data and build a model with the new data frame
train3_samp_train <- train3[sample(1:nrow(train3),6000000),]
train3_samp_test <- train3[sample(1:nrow(train3),6000000),]

#Build a decision tree with the new factors
dtree<- rpart(ConversionPayOut~Country+TrafficType+Fraud+DayOfWeek+HourOfDay+MinuteOfHour+total_clicks_userip+conversion_per_userip+mean_userip_payout,train3_samp_train,weight=train3_samp_train$weights,control=rpart.control(cp=0.000000000001,maxdepth=6))
train3_samp_test$predictions <- predict(dtree,train3_samp_test[,c("Country","TrafficType","Fraud","DayOfWeek","HourOfDay","MinuteOfHour","total_clicks_userip","conversion_per_userip","mean_userip_payout")])
train3_samp_test$predictions[which(train3_samp_test$Fraud==1)] <-0

#Calculating base weighted RMSE and weighted RMSE of predictions
#This did not improve the predictions
weight_rmse_base_samp <- sqrt(mean(train3_samp_test$weights*(0-train3_samp_test$ConversionPayOut)^2))*1000
weight_rmse_samp <- sqrt(mean(train3_samp_test$weights*(train3_samp_test$predictions-train3_samp_test$ConversionPayOut)^2))*1000


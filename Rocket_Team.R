library(tidyverse)
library(lubridate)
library(summarytools)
library(ggmap)
library(mlr) # you might need to install more packages depending on what learner you use in mlr!
library(dplyr)
library(naniar)
library(timeDate)
library(data.table)
library(readxl)
#install.packages("caret")
library(caret)

options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # set ggplot theme for cleaner plotting

set.seed(2020)

## Loading the Data
setwd("/Users/serkankd/Documents/WiSe 19-20/Business Analytics/Case_Study/AC_data_2020_bS8xg9j/")
df<-data.frame(read_csv('train.csv'))


#NA values which means outside of chicago in Pickup Community Area 
df[is.na(df$pickup_community_area),]$pickup_community_area<- 0
df[is.na(df$dropoff_community_area),]$dropoff_community_area<- 0

#Missed values
gg_miss_var(df)
head(df)


## seperate date and time
df$trip_start_timestamp <- as.POSIXct(df$trip_start_timestamp,format="%Y:%m:%d %H:%M:%S", tz= "GMT", origin ="1970-01-01")
df$start_time <- format(df$trip_start_timestamp,"%H:%M:%S" )
start_times <- strsplit(df$start_time, "[:]")
df$start_time_hour <- unlist(lapply(start_times, `[[`, 1))
df$start_time_hour <- as.numeric(df$start_time_hour)
df$start_time_min <- unlist(lapply(start_times, `[[`, 2))
df$start_time_min <- as.numeric(df$start_time_min)

head(df)



#df$trip_end_timestamp <- as.POSIXlt(df$trip_end_timestamp,format="%Y:%m:%d %H:%M:%S", tz= "GMT", origin ="1970-01-01")
#df$end_time <- format(df$trip_end_timestamp,"%H:%M:%S")
#end_times <- strsplit(df$end_time, ":")
#df$end_time_hour <- unlist(lapply(end_times, `[[`, 1))
#df$end_time_hour <- as.numeric(df$end_time_hour)
#df$end_time_min <- unlist(lapply(end_times, `[[`, 2))
#df$end_time_min <- as.numeric(df$end_time_min)

#seperate dates
start_date <- format(as.POSIXct(df$trip_start_timestamp,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
end_date<-format(as.POSIXct(df$trip_end_timestamp,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")

df$start_date <-  start_date
df$start_date <-  as.Date(df$start_dat, "%Y:%m:%d")

df$end_date <- end_date
df$end_date <- as.Date(df$end_date, "%Y:%m:%d")

#split start_date and end_date as month,day,year
df %>% dplyr::mutate(start_year = lubridate::year(df$start_date), start_month = lubridate::month(df$start_date),
                     start_day = lubridate::day(df$start_date))->df
df %>% dplyr::mutate(end_year = lubridate:: year(df$end_date), end_month = lubridate::month(df$end_date),
                     end_day = lubridate::day(df$end_date)) ->df

df <- as.data.table(df)
df <- df[, -c("taxi_id", "trip_start_timestamp" , "trip_end_timestamp")]
head(df)




## import holiday day of Chicago 
holiday_df<-data.frame(read_excel('holiday.xlsx'))
holiday_df$Date <-  as.Date(holiday_df$Date, "%Y-%m-%d")

#assing holidays
#df$type<-NULL
#head(df)
#df[df$start_date  %in%  holiday_df$Date,]$type<-"holiday"
#select bussiness days
#df[isWeekday(df$start_date)& ( (start_time>=07:00:00 & start_time<=09:00:00)|(start_time>=16:00:00 & start_time<=18:00:00)),]$type

#working days
df$day_type <-NA
df$day_type <- as.character(df$day_type)
df[isWeekday(df$start_date),]$day_type <- "workingday"

df[df$start_date  %in%  holiday_df$Date,]$day_type<- "holiday"
df[isWeekend(df$start_date),]$day_type<-"weekend"

df$day_type <- as.factor(df$day_type)
str(df)
#dmy <- dummyVars(" ~ day_type", data = df)
#trsf <- data.frame(predict(dmy, newdata = df))
#trsf
#df <- cbind(df, trsf)

#Trip_minute and 
head(df)
df<-df %>%mutate(trip_minut=trip_seconds/60 )
df<-df %>%mutate(speed=trip_miles /trip_minut )


# roush hours
df$rush<-0
df[(isWeekday(df$start_date) & ( (7<=df$start_time_hour&df$start_time_hour<=8 )|(df$start_time_hour==9 &df$start_time_min==0))|
      (16<=df$start_time_hour&df$start_time_hour<=17 )|(df$start_time_hour==18 &df$start_time_min==0))&  df$day_type =="workingday" ,]$rush<-1

###### CAlculation fare_per_yard
##adding trip_yard  converting from trip_miles* 1760 

df<- df%>%mutate(trip_yards=trip_miles*1760)
df<-df%>%mutate(fare_per_yard= fare/trip_yards)

## ############################################## IMPORT ZONE

CA_df<-data.frame(read_csv('CA_data.csv'))

CA_df$hardship_index <- as.numeric(CA_df$hardship_index)
CA_df$zone_by_economy <- as.numeric(cut(CA_df$hardship_index, 7))
CA_df[, c(1,16)]

zone_0 <- 0
CA_df <- rbind(CA_df, zone_0)

cdf<-df
##adding pick_up_zone and drop_of_zone
cdf$drop_of_zone<-NA
cdf$pick_up_zone<-NA

#preparing for merging 
head(CA_df)
head(cdf)

CA_df$pickup_community_area<-CA_df$community_area_number
CA_df$dropoff_community_area<-CA_df$community_area_number


merged_df<-merge(x=cdf,y=CA_df,by="pickup_community_area")
##asinging values zone to pick_UP_zone
merged_df$pick_up_zone<-merged_df$zone_by_economy
head(merged_df)

merged_df<-merged_df %>%select(-dropoff_community_area.y,-avg_gas_usage_therms,-avg_elec_usage_kwh,
     -predominant_non_english_language_percent, -life_expectancy_2010, -life_expectancy_2000, - life_expectancy_1990,
    -hardship_index,-per_capita_income,-percent_aged_under_18_or_over_64,-percent_aged_over_24_without_high_school_diploma,
    -percent_aged_over_15_unemployed,-percent_households_below_poverty,-percent_of_housing_crowded, -community_area_name,
    -community_area_number,-zone_by_economy)

head(merged_df)
##asinging values zone to drop of _zone

colnames(merged_df)[colnames(merged_df)=="dropoff_community_area.x"]<-"dropoff_community_area"
merged_df<-merge(x=merged_df,y=CA_df,by="dropoff_community_area")

merged_df$drop_of_zone<-merged_df$zone_by_economy

head(merged_df)
merged_df<-merged_df %>%select(-avg_gas_usage_therms,-avg_elec_usage_kwh,
                               -predominant_non_english_language_percent, -life_expectancy_2010, -life_expectancy_2000, - life_expectancy_1990,
                               -hardship_index,-per_capita_income,-percent_aged_under_18_or_over_64,-percent_aged_over_24_without_high_school_diploma,
                               -percent_aged_over_15_unemployed,-percent_households_below_poverty,-percent_of_housing_crowded, -community_area_name,
                               -community_area_number,-zone_by_economy,-pickup_community_area.y)

colnames(merged_df)[colnames(merged_df)=="pickup_community_area.x"]<-"pickup_community_area"
head(merged_df)

#merged_df<-merged_df %>% mutate(X_pickup=cos(pickup_centroid_latitude)*cos(pickup_centroid_longitude), y_pickup=cos(pickup_centroid_latitude)*sin(pickup_centroid_longitude)
#                                , z_pickup=sin(pickup_centroid_latitude),
#                                X_dropoff=cos(dropoff_centroid_latitude)*cos(dropoff_centroid_longitude), y_dropoff=cos(dropoff_centroid_latitude)*sin(dropoff_centroid_longitude)
#                                , z_dropoff=sin(dropoff_centroid_latitude))

#merged_df<-merged_df%>%mutate(euclidean_distance=sqrt( ( X_pickup-X_dropoff )^2 + (y_pickup-y_dropoff)^2 + ( z_pickup- z_dropoff)^2 ))


table(df$rush)

dt <- as.data.table(merged_df[, c(3,1,2,35,34,33,5,32,29,30,10,11,28,18,19,31)])
head(dt)
summary(dt)
str(dt)

dt$id <- as.factor(dt$id)
dt$dropoff_community_area <- as.factor(dt$dropoff_community_area)
dt$pickup_community_area <- as.factor(dt$pickup_community_area)
dt$drop_of_zone <- as.factor(dt$drop_of_zone)
dt$pick_up_zone <- as.factor(dt$pick_up_zone)
dt$payment_type <- as.factor(dt$payment_type)
dt$start_time_hour <- as.factor(dt$start_time_hour)
dt$start_time_min <- as.factor(dt$start_time_min)
dt$day_type <- as.factor(dt$day_type)
table(dt$rush)

#create validation set and predict on it
## set the seed to make your partition reproducible
# split data into training and test
N <- nrow(dt)
## 75% of the sample size
smp_size <- floor(0.75 * N)

set.seed(123)
train_ind <- sample(N, size = smp_size)

dt_train<- dt[train_ind, ]
dt_val <- dt[-train_ind, ]

# remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
str(dt_train)
table(dt_train$rush)
# 0: 119741,  1: 30259, nrow = 150000
summary(dt_train)

dt_train$fare_per_yard <-  remove_outliers(dt_train$fare_per_yard)
dt_train$speed <- remove_outliers(dt_train$speed)
dt_train$trip_total <- remove_outliers(dt_train$trip_total)
dt_train$trip_minut <-  remove_outliers(dt_train$trip_minut)
dt_train$trip_yards <- remove_outliers(dt_train$trip_yards)
summary(dt_train)

dt_train <- drop_na(dt_train)
dim(dt_train)
# nrow:94972, ncol = 16
table(dt_train$rush)
# 0: 73952 , 1:21020
head(dt_train)

# simplest model
#small.model <- glm(formula = rush ~ fare_per_yard +
#                speed + trip_total + day_typeweekend + day_typeworkingday ,
#              family = binomial(link = "logit"), data=dt_train)

#summary(small.model)

#standardization of the numeric variables

dt_train$fare_per_yard <- (dt_train$fare_per_yard - mean(dt_train$fare_per_yard))/ sqrt(var(dt_train$fare_per_yard))
dt_train$speed <- (dt_train$speed - mean(dt_train$speed))/ sqrt(var(dt_train$speed))
dt_train$trip_total <- (dt_train$trip_total - mean(dt_train$trip_total))/ sqrt(var(dt_train$trip_total))
dt_train$trip_minut <- (dt_train$trip_minut - mean(dt_train$trip_minut))/ sqrt(var(dt_train$trip_minut))
dt_train$trip_yards <- (dt_train$trip_yards - mean(dt_train$trip_yards))/ sqrt(var(dt_train$trip_yards))


str(dt_train)
summary(dt_train)
form_null <- formula("rush ~ 1")
form_full <- formula("rush ~ pick_up_zone + drop_of_zone +fare_per_yard + speed + trip_total +
                              day_type") 
form_inter <- formula("rush ~ (pick_up_zone + drop_of_zone  +fare_per_yard + speed + trip_total +
                              day_type)^2") 

model0 <- glm(rush ~ 1, data=dt_train, family=binomial(link="logit"))

model_bic <- step(model0, scope=list(lower = form_null, upper=form_inter),
                  direction = "both",
                  k = log(nrow(dt_train)))
getCall(model_bic)

#model_aic <- step(model0, scope=list(lower = form_null, upper=form_inter),
#                  direction = "both")
#getCall(model_aic)


# Part 1
# 1. estimate h_{ii}
my.hat=hatvalues(model_bic)

# How many coefficients? => p
p=length(model_bic$coefficients)
n=nrow(dt_train)

# boundary
upper=2*p/n
my.color=rep(1,length(my.hat))

# find high leverage points
my.color[my.hat>upper]=2

# 2. 
# Pearson residuals
# link for types: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.summaries.html
my.pres<-resid(model_bic,type="pearson")
range.pres<-max(abs(my.pres)) # needed for the plot

# Deviance residuals
my.deviance=resid(model_bic,type="deviance")
range.deviance<-max(abs(my.deviance)) # needed for the plot

# Adjusted residuals
my.adjusted <-my.pres/sqrt(1-my.hat)
range.adjusted <-max(abs(my.adjusted)) # needed for the plot

# The residuals seem to be randomly scattered around 0.
# (Note: The data set contains many (or only?) observations with kredit=0 for higher observation
# numbers. This explains the high amount of negative residuals for higher observation numbers.)

# 3. Cooks distance
my.cook=my.pres^2 *my.hat/(1-my.hat)^2 

plot(my.cook,pch=16,main="Cook's distance",xlab="observation number",ylab="distance",cex.lab=1.3,cex.axis=1.3)

abline(1,0)
which(my.cook>0.5)
which(my.hat>upper)
which(my.adjusted>2)


model_out <- model_bic

summary(model_out)
library(DescTools)
Cstat(model_out)
# can also create confusion matrix as follows
testres <- model_out$fitted.values
truestat <- dt_train$rush
tab <- table(truestat, testres)
#tab

library(Epi)
ROC(testres, truestat, MI=FALSE, AUC = TRUE) # also try adding plot="sp"

# Residual deviance test
Dev=model_out$deviance
Dev
qchisq(0.95,df=model_out$df.residual)
Dev > qchisq(0.95,df=model_out$df.residual)

# Likelihood Ratio Test
anova(model_out, test = "LRT")

########### ###
#Validation set


################
# 5. prediction
preds=predict(model_out, type="response", newdata=dt_val)

# To compare predicted values with the true values of the response variable we have to use a threshold function.

# predicted probabilites for winning on test-dataset, using the training-data model "mylogit" 

# construct confusion matrix (s. GLM lecture)

## Assumption:  if prediction value > 0.5 we classify as 1 (player A wins) and if prediction value < 0.5 we classify as 0 (player A loses)
## check this modelling with the command "round(preds)" and compare to "preds"

###################################
dt_val = dt_val %>% mutate(pred = round(preds)) 
# can also create confusion matrix as follows
tab <- table(true=dt_val$rush, prediction=round(preds))
tab

truepos <- tab[2,2]
trueneg <- tab[1,1]
falseneg <- tab[2,1]
falsepos <- tab[1,2]
totpos <- truepos + falseneg
totneg <- trueneg + falsepos
sens <- truepos/totpos
spec <- trueneg/totneg
BAC <- (sens+spec)/2
BAC

## calculate error-rate of Logit-Model ##
incorrectPredictionCount = nrow(dt_val %>% filter(rush!=pred))
totalPredictions = nrow(dt_val)
errorRate = incorrectPredictionCount/totalPredictions
errorRate

accuracy <- 1 -errorRate
accuracy


###############
# model aic
# 5. prediction
preds=predict(model_aic, type="response", newdata=dt_val)

# To compare predicted values with the true values of the response variable we have to use a threshold function.

# predicted probabilites for winning on test-dataset, using the training-data model "mylogit" 

# construct confusion matrix (s. GLM lecture)

## Assumption:  if prediction value > 0.5 we classify as 1 (player A wins) and if prediction value < 0.5 we classify as 0 (player A loses)
## check this modelling with the command "round(preds)" and compare to "preds"

###################################
dt_val = dt_val %>% mutate(pred = round(preds)) 
# can also create confusion matrix as follows
tab <- table(true=dt_val$rush, prediction=round(preds))
tab

truepos <- tab[2,2]
trueneg <- tab[1,1]
falseneg <- tab[2,1]
falsepos <- tab[1,2]
totpos <- truepos + falseneg
totneg <- trueneg + falsepos
sens <- truepos/totpos
spec <- trueneg/totneg
BAC <- (sens+spec)/2
BAC



# Calculation of various statistics
#( tot=colSums(tab) )                            # Number of players w/ each test result
#( truepos=unname(rev(cumsum(rev(tab[2,])))) )   # Number of true positives
#( falsepos=unname(rev(cumsum(rev(tab[1,])))) )  # Number of false positives
#( totpos=sum(tab[2,]) )                         # The total number of positives (one number)
#( totneg=sum(tab[1,]) )                         # The total number of negatives (one number)
#(sens=truepos/totpos)                           # Sensitivity (fraction true positives)
#(omspec=falsepos/totneg)                        # 1 ??? specificity (false positives)
#sens=c(sens,0); omspec=c(omspec,0)              # Numbers when we classify all as normal


## calculate error-rate of Logit-Model ##
#incorrectPredictionCount = nrow(dt_val %>% filter(rush!=pred))
#totalPredictions = nrow(dt_val)
#errorRate = incorrectPredictionCount/totalPredictions
#errorRate

#accuracy <- 1 -errorRate
#accuracy





####################
# Test set
df_test<-data.frame(read_csv('test.csv'))


#NA values which means outside of chicago in Pickup Community Area 
df_test[is.na(df_test$pickup_community_area),]$pickup_community_area<- 0
df_test[is.na(df_test$dropoff_community_area),]$dropoff_community_area<- 0

#Missed values
gg_miss_var(df_test)
head(df_test)



#df_test$trip_start_timestamp <- as.POSIXct(df_test$trip_start_timestamp,format="%Y:%m:%d %H:%M:%S", tz= "GMT", origin ="1970-01-01")

#seperate dates
start_date <- format(as.POSIXct(df_test$trip_start_timestamp,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
end_date<-format(as.POSIXct(df_test$trip_end_timestamp,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")

df_test$start_date <-  start_date
df_test$start_date <-  as.Date(df_test$start_dat, "%Y:%m:%d")

df_test$end_date <- end_date
df_test$end_date <- as.Date(df_test$end_date, "%Y:%m:%d")

#split start_date and end_date as month,day,year
df_test %>% dplyr::mutate(start_year = lubridate::year(df_test$start_date), start_month = lubridate::month(df_test$start_date),
                     start_day = lubridate::day(df_test$start_date))->df_test
df_test %>% dplyr::mutate(end_year = lubridate:: year(df_test$end_date), end_month = lubridate::month(df_test$end_date),
                     end_day = lubridate::day(df_test$end_date)) ->df_test

df_test <- as.data.table(df_test)
df_test <- df_test[, -c("taxi_id", "trip_start_timestamp" , "trip_end_timestamp")]
head(df_test)




## import holiday day of Chicago 
holiday_df_test<-data.frame(read_excel('holiday.xlsx'))
holiday_df_test$Date <-  as.Date(holiday_df_test$Date, "%Y-%m-%d")

#assing holidays
#df_test$type<-NULL
#head(df_test)
#df_test[df_test$start_date  %in%  holiday_df_test$Date,]$type<-"holiday"
#select bussiness days
#df_test[isWeekday(df_test$start_date)& ( (start_time>=07:00:00 & start_time<=09:00:00)|(start_time>=16:00:00 & start_time<=18:00:00)),]$type

#working days
df_test$workingday<-0
df_test[isWeekday(df_test$start_date),]$workingday<-1

#holiday 
df_test$holiday<-0
df_test[df_test$start_date  %in%  holiday_df_test$Date,]$holiday<-1

#Weekend 
df_test$weekend<-0
df_test[isWeekend(df_test$start_date),]$weekend<-1

#Trip_minute and 
head(df_test)
df_test<-df_test %>%mutate(trip_minut=trip_seconds/60 )
df_test<-df_test %>%mutate(speed=trip_miles /trip_minut )


###### CAlculation fare_per_yard
##adding trip_yard  converting from trip_miles* 1760 

df_test<- df_test%>%mutate(trip_yards=trip_miles*1760)
df_test<-df_test%>%mutate(fare_per_yard= fare/trip_yards)

## ############################################## IMPORT ZONE

CA_df_test<-data.frame(read_csv('CA_data.csv'))

CA_df_test$hardship_index <- as.numeric(CA_df_test$hardship_index)
CA_df_test$zone_by_economy <- as.numeric(cut(CA_df_test$hardship_index, 7))
CA_df_test[, c(1,16)]

zone_0 <- 0
CA_df_test <- rbind(CA_df_test, zone_0)

cdf_test<-df_test
##adding pick_up_zone and drop_of_zone
cdf_test$drop_of_zone<-NA
cdf_test$pick_up_zone<-NA

#preparing for merging 
head(CA_df_test)
head(cdf_test)

CA_df_test$pickup_community_area<-CA_df_test$community_area_number
CA_df_test$dropoff_community_area<-CA_df_test$community_area_number


merged_df_test<-merge(x=cdf_test,y=CA_df_test,by="pickup_community_area")
##asinging values zone to pick_UP_zone
merged_df_test$pick_up_zone<-merged_df_test$zone_by_economy
head(merged_df_test)

merged_df_test<-merged_df_test %>%select(-dropoff_community_area.y,-avg_gas_usage_therms,-avg_elec_usage_kwh,
                               -predominant_non_english_language_percent, -life_expectancy_2010, -life_expectancy_2000, - life_expectancy_1990,
                               -hardship_index,-per_capita_income,-percent_aged_under_18_or_over_64,-percent_aged_over_24_without_high_school_diploma,
                               -percent_aged_over_15_unemployed,-percent_households_below_poverty,-percent_of_housing_crowded, -community_area_name,
                               -community_area_number,-zone_by_economy)

head(merged_df_test)
##asinging values zone to drop of _zone

colnames(merged_df_test)[colnames(merged_df_test)=="dropoff_community_area.x"]<-"dropoff_community_area"
merged_df_test<-merge(x=merged_df_test,y=CA_df_test,by="dropoff_community_area")

merged_df_test$drop_of_zone<-merged_df_test$zone_by_economy

head(merged_df_test)
merged_df_test<-merged_df_test %>%select(-avg_gas_usage_therms,-avg_elec_usage_kwh,
                               -predominant_non_english_language_percent, -life_expectancy_2010, -life_expectancy_2000, - life_expectancy_1990,
                               -hardship_index,-per_capita_income,-percent_aged_under_18_or_over_64,-percent_aged_over_24_without_high_school_diploma,
                               -percent_aged_over_15_unemployed,-percent_households_below_poverty,-percent_of_housing_crowded, -community_area_name,
                               -community_area_number,-zone_by_economy,-pickup_community_area.y)

colnames(merged_df_test)[colnames(merged_df_test)=="pickup_community_area.x"]<-"pickup_community_area"
head(merged_df_test)

dt_test <- as.data.table(merged_df_test[, c(3,1,2,33,34,32,29,9,10,25,26,27)])
head(dt_test)
summary(dt_test)
str(dt_test)

dt_test$id <- as.factor(dt_test$id)
dt_test$dropoff_community_area <- as.factor(dt_test$dropoff_community_area)
dt_test$pickup_community_area <- as.factor(dt_test$pickup_community_area)
dt_test$drop_of_zone <- as.factor(dt_test$drop_of_zone)
dt_test$pick_up_zone <- as.factor(dt_test$pick_up_zone)
dt_test$payment_type <- as.factor(dt_test$payment_type)
dt_test$start_time_hour <- as.factor(dt_test$start_time_hour)
dt_test$start_time_min <- as.factor(dt_test$start_time_min)

################
# 5. prediction
preds=predict(model_out, type="response", newdata=dt_test)

# To compare predicted values with the true values of the response variable we have to use a threshold function.

# predicted probabilites for winning on test-dataset, using the training-data model "mylogit" 

# construct confusion matrix (s. GLM lecture)

## Assumption:  if prediction value > 0.5 we classify as 1 (player A wins) and if prediction value < 0.5 we classify as 0 (player A loses)
## check this modelling with the command "round(preds)" and compare to "preds"

###################################
dt_test = dt_test %>% mutate(pred = round(preds)) 
dt_test %>% group_by(target, pred) %>% summarise(count=n())

# can also create confusion matrix as follows
tab <- table(true=dt_test$target, prediction=round(preds))
tab

## calculate error-rate of Logit-Model ##
incorrectPredictionCount = nrow(dt_test %>% filter(target!=pred))
totalPredictions = nrow(dt_test)
errorRate = incorrectPredictionCount/totalPredictions
errorRate

accuracy <- 1 -errorRate
accuracy




library(readr)
library(dplyr)
library(tidyverse)
library(mice)
library(plyr)
library(ggplot2)
library(mice)
library(gplots)
library(knitr)
library(glmnet)
library(usethis)
library(caret)
#rm(list = ls()) 

#data dictionary https://github.com/nytimes/covid-19-data/tree/master/prisons 
data <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/prisons/facilities.csv')

data %>% 
  select(latest_inmate_population, max_inmate_population_2020, total_inmate_cases,total_inmate_deaths,total_officer_cases,total_officer_deaths) %>% 
  summary()

#A new variable, indicating the percentage of inmates who had covid (positivity_percentage) was added.
prison_with_percentage <- data %>% 
  mutate(positivity_percentage = total_inmate_cases/max_inmate_population_2020)

head(prison_with_percentage)

colnames(prison_with_percentage)

summary(prison_with_percentage$positivity_percentage)

hist(prison_with_percentage$positivity_percentage)

Facility_Size <- prison_with_percentage %>% 
  mutate(facility_sizes =
           cut(max_inmate_population_2020,breaks = c(0,500,1500,3000,Inf)))  %>% 
  mutate(facility_sizes = factor(facility_sizes, labels = 
                                   c("small","medium","large","XL"))) 

table(Facility_Size$facility_sizes, useNA="ifany")

table(Facility_Size$facility_type, Facility_Size$facility_sizes, useNA = "ifany") %>% 
  kable()

new_data <- data %>% 
  filter(facility_type == "Federal prison" | facility_type== "State prison", 
         is.na(max_inmate_population_2020)==FALSE) %>% 
  mutate(positivity_percentage = total_inmate_cases/max_inmate_population_2020)

#remove unnecessary columns
clean_data <-dplyr::select(new_data, -c("nyt_id","facility_name","facility_city","total_officer_cases","total_officer_deaths","note","facility_lat","facility_lng","facility_county_fips","facility_county","facility_state","total_inmate_deaths"))
colnames(clean_data)


# linear regression begin
# dummy coding and prep for lm()
facility_type<-factor(facility_type, c(0,1), labels=c('Federal prison', 'State prison'))
#fit linear regression model into data
model_1 <- lm(positivity_percentage ~ latest_inmate_population + facility_type,
              data = clean_data)
#plot 4 graphs and check to see if model follows the linear regression assumption 
plot(model_1)
#model output 
summary(model_1)
#calculate the RMSE for lm() model and use it to compare with regularized regression 
# the smaller the RMSE, the better model fit 
sqrt(mean(model_1$residuals^2))

#What if we do a one-vraiable model to compare the RMSE and R^2 with 
model <- lm(positivity_percentage ~ facility_type, data = clean_data)
summary(model)

#What if we do a one-vraiable model to compare the RMSE and R^2 with 
model_3 <- lm(positivity_percentage ~ latest_inmate_population, data = clean_data)
summary(model_3)

#cleaning up the NAs in perperation for the regularized regression 
colSums(is.na(clean_data))
clean_data$latest_inmate_population[is.na(clean_data$latest_inmate_population)] <- round(mean(clean_data$latest_inmate_population, na.rm = TRUE))
sum(is.na(clean_data$latest_inmate_population))
head(clean_data)

#regularized regression 
#split data into train and test set 
set.seed(3456) 
trainIndex <- createDataPartition(clean_data$positivity_percentage, p=0.7,list = FALSE, times = 1)
train <- clean_data[ trainIndex,] 
test <- clean_data[-trainIndex,] 

#preparing for glmnet() since it doesn't take Y~X format
X_train <- model.matrix(positivity_percentage~., train)[,-1]
Y_train <- train$positivity_percentage

X_test <- model.matrix(positivity_percentage~., test)[,-1]
Y_test <- test$positivity_percentage

#use cv.glmnet to estimate lambda.min and lambda 1se, the optimal value of lambda
ridge<- cv.glmnet(x=X_train, y=Y_train, alpha = 0)

plot(ridge)

#lambda for this min MSE
ridge$lambda.min 
log(ridge$lambda.min)
##largest value of lambda that is 1 standard error from lambda min 
ridge$lambda.1se
log(ridge$lambda.1se)

#fit model to train dataset using lambda.min 
#use this to compare with the model with lambda.1se later
ridge.model.min <- glmnet(X_train,Y_train,alpha = 0, lambda = ridge$lambda.min)
ridge.model.min
#show regression coefficients 
coef(ridge.model.min)

#fit model to train dataset using lambda.1se 
#use this to compare with the model with lambda.min
ridge.model.1se <- glmnet(X_train,Y_train,alpha = 0, lambda = ridge$lambda.1se)
ridge.model.1se

#RMSE on train dataset
ridge.preds.train <- predict(ridge.model.1se, newx = X_train)
ridge.train.rmse <- rmse(Y_train, ridge.preds.train)
ridge.train.rmse

#RMSE on test dataset
ridge.preds.test <- predict(ridge.model.1se, newx = X_test)
ridge.test.rmse <- rmse(Y_test, ridge.preds.test)
ridge.test.rmse

#LASSO 
set.seed(3456)
cv.lasso <- cv.glmnet(X_train,Y_train,nfolds = 10)

plot(cv.lasso)

#lambda for this min MSE
cv.lasso$lambda.min 
log(cv.lasso$lambda.min)
#largest value of lambda that is 1 standar error from lambda min 
cv.lasso$lambda.1se
log(cv.lasso$lambda.1se)

#fit model to train dataset using lambda.min 
#use this to compare with the model with lambda.1se later
model.min <- glmnet(X_train,Y_train,alpha = 1, lambda = cv.lasso$lambda.min)
model.min

#show regression coefficients 
coef(model.min)

#fit model to train dataset using lambda.1se 
#use this to cmpare with the model with lambda.min (see line XX)
model.1se <- glmnet(X_train,Y_train,alpha = 1, lambda = cv.lasso$lambda.1se)
model.1se

#show regression coefficients
coef(model.1se)


#RMSE on train dataset
preds.train <- predict(model.1se, newx = X_train)
train.rmse <- rmse(Y_train, preds.train)
train.rmse

#RMSE on test dataset
preds.test <- predict(model.1se, newx = X_test)
test.rmse <- rmse(Y_test, preds.train)
test.rmse



#################### END ################################
head(raw_data, n=3)
colnames(raw_data)
summary(raw_data)
dim(raw_data)
head(raw_data, n=3)
str(raw_data)
class(raw_data)
mode(raw_data)
dim(raw_data)
###data cleaning begins
#based on our research question, not all columns are needed from the raw dataset 
#remove columns the following columns 
#total_officer_cases, total_officer_deaths, notes, facility_lat,facility_lng, facility_fips 
clean_data <-dplyr::select(raw_data, -c("total_officer_cases","total_officer_deaths","note","facility_lat","facility_lng","facility_county_fips"))

#are there any NAs? 
colSums(is.na(clean_data))

#since the NAs appear under integer variables "latest_inmate_population" and "max_inmate_population_2020"
# I created a new object called "integer_data" - prepping for the mice library 
#resource - https://rforpoliticalscience.com/2020/07/28/impute-missing-values-with-mice-package-in-r/
char_data <- clean_data[,sapply(clean_data,is.character)]
integer_data <- clean_data[,sapply(clean_data,is.numeric)]

#is there a way to measure the performance of mice? 
#resource - https://data.library.virginia.edu/getting-started-with-multiple-imputation-in-r/ 
imputed_data <- mice(integer_data, method = 'cart')
mice_integer_data <- complete(imputed_data)

#check to see if there's any NAs
colSums(is.na(mice_integer_data))
dim(mice_integer_data)
#combine the two dataset back "integer_data" and "char_data" 
clean_data2 <-cbind(mice_integer_data, char_data)
colnames(clean_data2)

######

# a way to measure the performance of mice 
#resource - https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 
# resource - https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/ 

#######

### regression analysis begins 
#dependent variable - total_inmate_cases 
#independent variables - latest_inmate_population, facility_type, facility_state
#setting up for lm() 
#change categorical to factor 
#resource - https://swcarpentry.github.io/r-novice-inflammation/12-supp-factors/index.html
clean_data2$facility_type.f <- factor(clean_data2$facility_type)
head(clean_data2$facility_type.f)
clean_data2$facility_type.f[1:10]

clean_data2$facility_state.f <- factor(clean_data2$facility_state)
head(clean_data2$facility_state.f)
clean_data2$facility_state.f[1:10]

# a distribution of total_inmate_cases by facility_type.f
# how to remove Marshalls ???? 
barplot(table(clean_data2$facility_type.f),las=2)
plot(x = clean_data2$facility_type.f, y = clean_data2$total_inmate_cases, las=2)
clean_data2$facility_type.f <- droplevels(clean_data2$facility_type.f)        
plot(x = clean_data2$facility_type.f, y = clean_data2$total_inmate_cases, las=2)


#test
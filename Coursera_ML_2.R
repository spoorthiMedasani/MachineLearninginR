##############################ASSIGNMENT 1#######################################
#IMPORT DATA 
train <- read.csv(file="kc_house_train_data.csv",header=TRUE,sep=",")
test <- read.csv(file="kc_house_test_data.csv",header=TRUE,sep=",")
full <- read.csv(file="kc_house_data.csv",header=TRUE,sep=",")

#SIMPLE LINEAR REGRESSION
model = lm(price ~ sqft_living, data = train)
model

print(summary(model),digits=5)

#TO FIND RSS OF THE REGRSSION
sum(model$residuals^2)

#PREDICTED OUTPUT
pred <- predict(model,test)
write.csv(pred,"pred1.csv")
pred1 <- read.csv(file="pred1.csv",header=TRUE,sep=",")

pred1$X <- NULL

test$Pred1 <- pred1$x

#TO FIND RSS OF 1st DATASET
test1 <- data.frame(actual=test$price,predicted=test$Pred1)
test1$resid <- (test1$actual - test1$predicted)^2

sum(test1$resid)

#######################################################################
model2 = lm(price ~ bedrooms, data = train)
model2

print(summary(model2),digits=5)

sum(model2$residuals^2)

pred2 <- predict(model2,test)
write.csv(pred2,"pred2.csv")
pred2 <- read.csv(file="pred2.csv",header=TRUE,sep=",")

pred2$X <- NULL

test$Pred2 <- pred2$x
test2 <- data.frame(actual=test$price,predicted=test$Pred2)
test2$resid <- (test2$actual - test2$predicted)^2

sum(test2$resid)

####################################ASSIGNMENT 2#################################

full <- read.csv(file="kc_house_data.csv",header=TRUE,sep=",")
train <- read.csv(file="kc_house_train_data.csv",header=TRUE,sep=",")
test <- read.csv(file="kc_house_test_data.csv",header=TRUE,sep=",")

train$bedrooms_squared <- train$bedrooms^2
train$bed_bath_rooms <- (train$bedrooms * train$bathrooms)
train$log_sqft_living <- log(train$sqft_living)
train$lat_plus_long <- train$lat + train$long

test$bedrooms_squared <- test$bedrooms^2
test$bed_bath_rooms <- (test$bedrooms * test$bathrooms)
test$log_sqft_living <- log(test$sqft_living)
test$lat_plus_long <- test$lat + test$long

mean(test$bedrooms_squared)
mean(test$bed_bath_rooms)
mean(test$log_sqft_living)
mean(test$lat_plus_long)

#MODEL 1 - TRAINING DATASET
model1_train = lm(price ~ train$sqft_living+train$bedrooms+train$bathrooms+train$lat+train$long, data = train)

print(summary(model1_train),digits=5)
#MODEL 2 - TRAINING DATASET
model2_train = lm(price ~ train$sqft_living+train$bedrooms+train$bathrooms+train$lat+train$long+train$bed_bath_rooms, data = train)

print(summary(model2_train),digits=5)
#MODEL 3 - TRAINING DATASET
model3_train = lm(price ~ train$sqft_living+train$bedrooms+train$bathrooms+train$lat+train$long+train$bedrooms_squared+train$log_sqft_living+train$lat_plus_long, data = train)

print(summary(model3_train),digits=5)

#MODEL 1 - TESTING DATASET
model1_test = lm(price ~ test$sqft_living+test$bedrooms+test$bathrooms+test$lat+test$long, data = test)

print(summary(model1_test),digits=5)
#MODEL 2 - TESTING DATASET
model2_test = lm(price ~ test$sqft_living+test$bedrooms+test$bathrooms+test$lat+test$long+test$bed_bath_rooms, data = test)

print(summary(model2_test),digits=5)
#MODEL 3 - TESTING DATASET
model3_test = lm(price ~ test$sqft_living+test$bedrooms+test$bathrooms+test$lat+test$long+test$bedrooms_squared+test$log_sqft_living+test$lat_plus_long, data = test)

print(summary(model3_test),digits=5)

#RSS ON TRAIN
  #MODEL1
train$model1 <- predict(model1_train,train)
train$model1_sq <- (train$price - train$model1)^2
sum(train$model1_sq)
  #MODEL2
train$model2 <- predict(model2_train,train)
train$model2_sq <- (train$price - train$model2)^2
sum(train$model2_sq)
  #MODEL3
train$model3 <- predict(model3_train,train)
train$model3_sq <- (train$price - train$model3)^2
sum(train$model3_sq)

#RSS ON TEST
  #MODEL1
test$model1 <- predict(model1_train,test)
test$model1_sq <- (test$price - test$model1)^2
sum(test$model1_sq)
  #MODEL2
test$model2 <- predict(model2_test,test)
test$model2_sq <- (test$price - test$model2)^2
sum(test$model2_sq)
  #MODEL3
test$model3 <- predict(model3_test,test)
test$model3_sq <- (test$price - test$model3)^2
sum(test$model3_sq)

##################
train <- read.csv(file="kc_house_train_data.csv",header=TRUE,sep=",")
test <- read.csv(file="kc_house_test_data.csv",header=TRUE,sep=",")

actual <- train$price
weight <- c(-47000,1)

features <- train$sqft_living
n <- nrow(train)
unity <- rep(1,n)
input <- cbind(unity,features)
input_t <- t(input)

prediction <- function(input,weight)
  {
  predicted <- weight %*% input_t
}

error <- function(actual,predicted)
{
  error <- (actual-predicted)
}

slope <- 1
intercept <- -47000
step_size <- 7e-12
tolerance <- 2.5e7
weight <- c(-47000,1)
gradient_magnitude <- 0

for(i in 1:1000)
  {
  predicted <- prediction(input = input_t,weight = weight)
  error <- actual - predicted
  intercept <- intercept - (step_size*(-2*sum(error)))
  slope <- slope - (step_size*(-2*sum(error * features)))
  gradient_magnitude <- ((intercept^2)+(slope^2))
  weight <- c(intercept,slope)
}


############################################################
actual <- train$price
weight <- c(-100000,1,1)

feature1 <- train$sqft_living
feature2 <- train$sqft_living15

n <- nrow(train)
unity <- rep(1,n)
input <- cbind(unity,feature1,feature2)
input_t <- t(input)

prediction <- function(input,weight)
{
  predicted <- weight %*% input
}

error <- function(actual,predicted)
{
  error <- (actual-predicted)
}

slope1 <- 1
slope2 <- 1
intercept <- -100000
step_size <- 4e-12
tolerance <- 1e9
weight <- c(-100000,1,1)
gradient_magnitude <- (tolerance+1)

while(tolerance < gradient_magnitude)
{
  predicted <- prediction(input = input_t,weight = weight)
  error <- actual - predicted
  intercept <- intercept - (step_size*(-2*sum(error)))
  slope1 <- slope1 - (step_size*(-2*sum(error * feature1)))
  slope2 <- slope2 - (step_size*(-2*sum(error * feature2)))
  gradient_magnitude <- ((intercept^2)+(slope1^2)+(slope2^2))
  weight <- c(intercept,slope1,slope2)
}

######################## WEEK 3 ###############################

train <- read.csv(file="wk3_kc_house_train_data.csv",header=TRUE,sep=",")
test <- read.csv(file="wk3_kc_house_test_data.csv",header=TRUE,sep=",")
valid <- read.csv(file="wk3_kc_house_valid_data.csv",header=TRUE,sep=",")
set1 <- read.csv(file="wk3_kc_house_set_1_data.csv",header=TRUE,sep=",")
set2 <- read.csv(file="wk3_kc_house_set_2_data.csv",header=TRUE,sep=",")
set3 <- read.csv(file="wk3_kc_house_set_3_data.csv",header=TRUE,sep=",")
set4 <- read.csv(file="wk3_kc_house_set_4_data.csv",header=TRUE,sep=",")
full <- read.csv(file="kc_house_data.csv",header=TRUE,sep=",")

features_power <- function(feature,raisedto)
{
  features <- feature
  for (i in 2:raisedto)
  {
    dummy <- feature^i
    features <- cbind(features,dummy)
  }
  for (i in 2:raisedto)
  {
    colnames(features)[i] <- assign(paste("dummy"),i)
  }
  colnames(features) <- paste("feature", 1:raisedto, sep = "")
  return(features)
}

#set1
set1_raised <- features_power (feature = set1$sqft_living,raisedto = 15)
set1_raised <- data.frame(set1_raised)
set1_raised$price <- set1$price

model1 <- lm(price ~ feature15,set1_raised)

summary(model1,digits=5)

#set2
set2_raised <- features_power(feature = set2$sqft_living,raisedto = 15)
set2_raised <- data.frame(set2_raised)
set2_raised$price <- set2$price

model2 <- lm(price ~ feature15,set2_raised)

summary(model2,digits=5)

#set3
set3_raised <- features_power (feature = set3$sqft_living,raisedto = 15)
set3_raised <- data.frame(set3_raised)
set3_raised$price <- set3$price

model3 <- lm(price ~ feature15,set3_raised)

summary(model3,digits=5)

#set4
set4_raised <- features_power (feature = set4$sqft_living,raisedto = 15)
set4_raised <- data.frame(set4_raised)
set4_raised$price <- set4$price

model4 <- lm(price ~ feature15,set4_raised)


#Plots
plot(set1_raised$feature15,set1_raised$price)
abline(model1)
plot(set2_raised$feature15,set2_raised$price)
abline(model2)
plot(set3_raised$feature15,set3_raised$price)
abline(model3)
plot(set4_raised$feature15,set4_raised$price)
abline(model4)

#validation
train_power <- features_power(feature = train$sqft_living,raisedto = 15)
train_power <- data.frame(train_power)
train_power$price <- train$price

valid_power <- features_power(feature = valid$sqft_living,raisedto = 15)
valid_power <- data.frame(valid_power)
valid_power$price <- valid$price 

test_power <- features_power(feature = test$sqft_living,raisedto = 15)
test_power <- data.frame(test_power)
test_power$price <- test$price 

head(train_power,n=10)
head(valid_power,n=10)
head(test_power,n=10)

##########################################


##################################WEEK 4####################################
####ASSIGNMENT 1#####

train <- read.csv(file="kc_house_data.csv",header=TRUE,sep=",")

features_power <- function(feature,raisedto)
{
  features <- feature
  for (i in 2:raisedto)
  {
    dummy <- feature^i
    features <- cbind(features,dummy)
  }
  for (i in 2:raisedto)
  {
    colnames(features)[i] <- assign(paste("dummy"),i)
  }
  colnames(features) <- paste("feature", 1:raisedto, sep = "")
  return(features)
}

train_power <- features_power(feature=train$sqft_living,raisedto = 15)
train_power <- data.frame(train_power)
train_power$price <- train$price

library(MASS)
ridge <- lm.ridge(train_power$price ~ train_power$feature1+train_power$feature2+train_power$feature3+train_power$feature4+train_power$feature5
                                      +train_power$feature6+train_power$feature7+train_power$feature8+train_power$feature9+train_power$feature10
                                      +train_power$feature11+train_power$feature12+train_power$feature13+train_power$feature14+train_power$feature15, lambda=1.5e-5)

ridge
summary(ridge)

set1 <- read.csv(file="wk3_kc_house_set_1_data.csv",header=TRUE,sep=",")
set2 <- read.csv(file="wk3_kc_house_set_2_data.csv",header=TRUE,sep=",")
set3 <- read.csv(file="wk3_kc_house_set_3_data.csv",header=TRUE,sep=",")
set4 <- read.csv(file="wk3_kc_house_set_4_data.csv",header=TRUE,sep=",")

set1_raised <- features_power(feature=set1$sqft_living,raisedto = 15)
set1_raised <- data.frame(set1_raised)
set1_raised$price <- set1$price

set2_raised <- features_power(feature=set2$sqft_living,raisedto = 15)
set2_raised <- data.frame(set2_raised)
set2_raised$price <- set2$price

set3_raised <- features_power(feature=set3$sqft_living,raisedto = 15)
set3_raised <- data.frame(set3_raised)
set3_raised$price <- set3$price

set4_raised <- features_power(feature=set4$sqft_living,raisedto = 15)
set4_raised <- data.frame(set4_raised)
set4_raised$price <- set4$price

ridge1 <- lm.ridge(set1_raised$price ~ set1_raised$feature1+set1_raised$feature2+set1_raised$feature3+set1_raised$feature4+set1_raised$feature5
                  +set1_raised$feature6+set1_raised$feature7+set1_raised$feature8+set1_raised$feature9+set1_raised$feature10
                  +set1_raised$feature11+set1_raised$feature12+set1_raised$feature13+set1_raised$feature14+set1_raised$feature15, lambda=1e-9)

ridge2 <- lm.ridge(set2_raised$price ~ set2_raised$feature1+set2_raised$feature2+set2_raised$feature3+set2_raised$feature4+set2_raised$feature5
                   +set2_raised$feature6+set2_raised$feature7+set2_raised$feature8+set2_raised$feature9+set2_raised$feature10
                   +set2_raised$feature11+set2_raised$feature12+set2_raised$feature13+set2_raised$feature14+set2_raised$feature15, lambda=1e-9)

ridge3 <- lm.ridge(set3_raised$price ~ set3_raised$feature1+set3_raised$feature2+set3_raised$feature3+set3_raised$feature4+set3_raised$feature5
                   +set3_raised$feature6+set3_raised$feature7+set3_raised$feature8+set3_raised$feature9+set3_raised$feature10
                   +set3_raised$feature11+set3_raised$feature12+set3_raised$feature13+set3_raised$feature14+set3_raised$feature15, lambda=1e-9)

ridge4 <- lm.ridge(set4_raised$price ~ set4_raised$feature1+set4_raised$feature2+set4_raised$feature3+set4_raised$feature4+set4_raised$feature5
                   +set4_raised$feature6+set4_raised$feature7+set4_raised$feature8+set4_raised$feature9+set4_raised$feature10
                   +set4_raised$feature11+set4_raised$feature12+set4_raised$feature13+set4_raised$feature14+set4_raised$feature15, lambda=1e-9)

ridge1
ridge2
ridge3
ridge4

ridge1 <- lm.ridge(set1_raised$price ~ set1_raised$feature1+set1_raised$feature2+set1_raised$feature3+set1_raised$feature4+set1_raised$feature5
                   +set1_raised$feature6+set1_raised$feature7+set1_raised$feature8+set1_raised$feature9+set1_raised$feature10
                   +set1_raised$feature11+set1_raised$feature12+set1_raised$feature13+set1_raised$feature14+set1_raised$feature15, lambda=1.23e2)

ridge2 <- lm.ridge(set2_raised$price ~ set2_raised$feature1+set2_raised$feature2+set2_raised$feature3+set2_raised$feature4+set2_raised$feature5
                   +set2_raised$feature6+set2_raised$feature7+set2_raised$feature8+set2_raised$feature9+set2_raised$feature10
                   +set2_raised$feature11+set2_raised$feature12+set2_raised$feature13+set2_raised$feature14+set2_raised$feature15, lambda=1.23e2)

ridge3 <- lm.ridge(set3_raised$price ~ set3_raised$feature1+set3_raised$feature2+set3_raised$feature3+set3_raised$feature4+set3_raised$feature5
                   +set3_raised$feature6+set3_raised$feature7+set3_raised$feature8+set3_raised$feature9+set3_raised$feature10
                   +set3_raised$feature11+set3_raised$feature12+set3_raised$feature13+set3_raised$feature14+set3_raised$feature15, lambda=1.23e2)

ridge4 <- lm.ridge(set4_raised$price ~ set4_raised$feature1+set4_raised$feature2+set4_raised$feature3+set4_raised$feature4+set4_raised$feature5
                   +set4_raised$feature6+set4_raised$feature7+set4_raised$feature8+set4_raised$feature9+set4_raised$feature10
                   +set4_raised$feature11+set4_raised$feature12+set4_raised$feature13+set4_raised$feature14+set4_raised$feature15, lambda=1.23e2)

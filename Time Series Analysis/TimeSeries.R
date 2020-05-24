

library(caret)

data = read.csv('Final_hourly_Electricity_transposed.csv')
head(data)
data = data[-1]
head(data)
y = data[,24]
head(y)
data_x = data[,1:23]
head(data_x)
# Using pre-sliced data
myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)
#data[is.na(data['X23'])]['X23']
# Linear regression
glmFitTime <- train(X23 ~ .,
                    data = data,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl, na.action=na.exclude)
glmFitTime
summary(glmFitTime)
y_hat = predict(glmFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with linear regression

# Support Vector Regression
svmFitTime <- train(X23 ~ .,
                    data = data,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl, na.action = na.exclude)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with support vector regression

# Neural Network
nnFitTime <- train(X23 ~ .,
                   data = data,
                   method = "avNNet",
                   preProc = c("center", "scale"),
                   trControl = myCvControl,
                   linout = T,
                   trace = F,
                   MaxNWts = 10 * (ncol(data) + 1) + 10 + 1,
                   maxit = 500, na.action = na.exclude)
nnFitTime
summary(nnFitTime)
y_hat = predict(nnFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with neural networks

# You can experiment with other methods, here is where you can find the methods caret supports:
# https://topepo.github.io/caret/available-models.html

# Compare models
resamps <- resamples(list(lm = glmFitTime,
                          svn = svmFitTime,
                          nn = nnFitTime))
summary(resamps)


# Now working with the time-series modeling

library('tseries')
library(forecast)


t=read.csv("Final_hourly_Electricity.csv",header=T)
t=t[-1]
tSeries = ts(t[['val']],start=c(2011),freq=365.25*24)
head(tSeries)


plot(tSeries)
#ltseries<-log(tSeries)
#head(ltseries)
#plot(ltseries)
#plot(stl(ltseries,s.window="periodic"))
#dim(tSeries)
plot(stl(tSeries,s.window="periodic"))


#install.packages("forecast")
#install.packages('tseries') 
#install.packages("forecast", dependencies = TRUE)
library("forecast")

## Your Holt-Winters error
ar <- Arima(tSeries,order=c(7,0,7))
summary(ar)
#ar <- arima(tSeries,order=c(7,0,7))
mean(100*abs(fitted(ar) - tSeries)/tSeries)
# Your Arima error












###################################################################################
#############################1 Day ######################################################

data = read.csv('day_wise_data_transformed.csv')
head(data)
data = data[-1]
head(data)
y = data[,24]
head(y)
data_x = data[,1:23]
head(data_x)
# Using pre-sliced data
myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)
#data[is.na(data['X23'])]['X23']
# Linear regression
glmFitTime <- train(X23 ~ .,
                    data = data,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl, na.action=na.exclude)
glmFitTime
summary(glmFitTime)
y_hat = predict(glmFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with linear regression

# Support Vector Regression
svmFitTime <- train(X23 ~ .,
                    data = data,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl, na.action = na.exclude)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with support vector regression

# Neural Network
nnFitTime <- train(X23 ~ .,
                   data = data,
                   method = "avNNet",
                   preProc = c("center", "scale"),
                   trControl = myCvControl,
                   linout = T,
                   trace = F,
                   MaxNWts = 10 * (ncol(data) + 1) + 10 + 1,
                   maxit = 500, na.action = na.exclude)
nnFitTime
summary(nnFitTime)
y_hat = predict(nnFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with neural networks

# You can experiment with other methods, here is where you can find the methods caret supports:
# https://topepo.github.io/caret/available-models.html

# Compare models
resamps <- resamples(list(lm = glmFitTime,
                          svn = svmFitTime,
                          nn = nnFitTime))
summary(resamps)


# Now working with the time-series modeling

library('tseries')
library(forecast)


t=read.csv("day_wise_data.csv",header=T)
t=t[-1]
tSeries = ts(t[['val']],start=c(2011),freq=365.25)
head(tSeries)


plot(tSeries)
#ltseries<-log(tSeries)
#head(ltseries)
#plot(ltseries)
#plot(stl(ltseries,s.window="periodic"))
dim(tSeries)
plot(stl(tSeries,s.window="periodic"))


#install.packages("forecast")
#install.packages('tseries') 
install.packages("forecast", dependencies = TRUE)
library("forecast")

hw = ets(tSeries,model="MAM")
mean(100*abs(fitted(hw) - tSeries)/tSeries)
# Your Holt-Winters error
summary(hw)
ar <- Arima(tSeries,order=c(7,0,7))
summary(ar)
#ar <- arima(tSeries,order=c(7,0,7))
mean(100*abs(fitted(ar) - tSeries)/tSeries)
# Your Arima error






##################################################################################################################
##################################################################################################################
##################################################################################################################
#################################################      15 mins ###################################################


library(caret)

data = read.csv('transposed_data_15mins.csv')
head(data[1:1000,])
data = data[1:1000, ]
data = data[-1]
head(data)
y = data[,7]
head(y)
data_x = data[,1:6]
head(data_x)
# Using pre-sliced data
myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)
#data[is.na(data['X23'])]['X23']
# Linear regression
glmFitTime <- train(X7 ~ .,
                    data = data,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl, na.action=na.exclude)
glmFitTime
summary(glmFitTime)
y_hat = predict(glmFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with linear regression

# Support Vector Regression
svmFitTime <- train(X7 ~ .,
                    data = data,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl, na.action = na.exclude)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with support vector regression

# Neural Network
nnFitTime <- train(X7 ~ .,
                   data = data,
                   method = "avNNet",
                   preProc = c("center", "scale"),
                   trControl = myCvControl,
                   linout = T,
                   trace = F,
                   MaxNWts = 10 * (ncol(data) + 1) + 10 + 1,
                   maxit = 500, na.action = na.exclude)
nnFitTime
summary(nnFitTime)
y_hat = predict(nnFitTime, newdata = data_x)
mean(100*abs(y_hat-y)/y)
# Your error with neural networks

# You can experiment with other methods, here is where you can find the methods caret supports:
# https://topepo.github.io/caret/available-models.html

# Compare models
resamps <- resamples(list(lm = glmFitTime,
                          svn = svmFitTime,
                          nn = nnFitTime))
summary(resamps)


# Now working with the time-series modeling

library('tseries')
library(forecast)


t=read.csv("MT123electricity_15mins_1000rows.csv",header=T)
t=t[-1]
head(t)
tSeries = ts(t[['Value']],start=c(2011),freq=365.25*24*4)
head(tSeries)


plot(tSeries)
#ltseries<-log(tSeries)
#head(ltseries)
#plot(ltseries)
#plot(stl(ltseries,s.window="periodic"))
dim(tSeries)
plot(stl(tSeries,s.window="periodic"))


#install.packages("forecast")
#install.packages('tseries') 
#install.packages("forecast", dependencies = TRUE)
library("forecast")

hw = ets(tSeries,model="MAM")
mean(100*abs(fitted(hw) - tSeries)/tSeries)
# Your Holt-Winters error
summary(hw)
ar <- Arima(tSeries,order=c(7,0,7))
summary(ar)
#ar <- arima(tSeries,order=c(7,0,7))
mean(100*abs(fitted(ar) - tSeries)/tSeries)
# Your Arima error



set.seed(5580)
car<- read.csv("car.csv",header=TRUE)

require(randomForest)
library(dplyr)
library(rpart)
library(caret)
library(rpart.plot)




## 75% of the sample size
smp_size <- floor(0.75 * nrow(car))

## set the seed to make your partition reproducible

train_ind <- sample(seq_len(nrow(car)), size = smp_size)

car_test <- car[-train_ind, ]
car_train <- car[train_ind, ]
numeric_car_test <- numeric_car[-train_ind, ]
numeric_car_train <- numeric_car[train_ind, ]
#new_car_train<-new_car[-train_ind, ]
#new_car_train<-new_car[train_ind, ]
# Segragating 4 categories.



d_control<-trainControl(method='repeatedcv',number = 10,repeats=3)


# Random Forest
car_forest=randomForest(shouldBuy ~ .,data=car_train)
car.forest.tuned=randomForest(shouldBuy ~ .,data=car_train ,mtry=6)
rf_tuning <- train(shouldBuy ~., data = car, method = "rf",metric="Accuracy",trControl=d_control)


tuned.forest <- tuneRF(
  x          = car_train[,-7],
  y          = car_train$shouldBuy,
  ntreeTry   = 500,
  mtryStart  = 4,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE)


confusionMatrix(predict(car.forest,car_test),car_test$shouldBuy)
confusionMatrix(predict(car.forest.tuned,car_test),car_test$shouldBuy)
confusionMatrix(predict(rf_tuning,car_test),car_test$shouldBuy)


plot(car.forest)
plot(rf_tuning)
plot(car.forest.tuned)

#Desion Tree

ID3 <- train(shouldBuy ~., data = car, method = "rpart",parms = list(split = "information"))
ID3With_Tunig <- train(shouldBuy ~., data = car, method = "rpart",parms = list(split = "gini"),tuneLength=10)

ID3_pred<-predict(ID3,newdata = car_test)
ID3_pred_tune<-predict(ID3With_Tunig,newdata = car_test)

confusionMatrix(ID3_pred,car_test$shouldBuy)
confusionMatrix(ID3_pred_tune,car_test$shouldBuy)

prp(ID3$finalModel, box.palette = "Blue", tweak = 1.2)
prp(ID3With_Tunig$finalModel, box.palette = "Blue", tweak = 1.2)


library(pROC)
carTree=rpart(shouldBuy ~ .,data=car,method='class',control=rpart.control(minsplit=60),trControl=d_control)

carTree
accc<-list()
i=10
j<-0
index<-list()
while (i<1000 ){
  carTree<-rpart(shouldBuy ~ .,data=car,method='class',control=rpart.control(minsplit=i))
  carpred<-predict(carTree,newdata = car_test,type='class')
  accc[j]<-list(i=confusionMatrix(carpred,car_test$shouldBuy)$overall['Accuracy'])
  index[j]<-i
  #accc<-c(accc,c((confusionMatrix(carpred,car_test$shouldBuy)$overall['Accuracy'])*100,i))
     i<-i+10;
    j<-j+1
    print(j)

    }
plot(accc,index)
carpred=predict(carTree,newdata = car_test,type='class')
confusionMatrix(carpred,car_test$shouldBuy)
carTree<-rpart(shouldBuy ~.,data=car,method='class',control=rpart.control(minsplit=90))


multiclass.roc(car_test$shouldBuy,carpred_Probibility, plot = TRUE, print.auc = TRUE)

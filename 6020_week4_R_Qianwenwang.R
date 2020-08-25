#PARTA

#Imoprt packages 
library(randomForest)
library(MASS)
library(ipred)
library(ggplot2)
library(cowplot)
library(mlbench)
library(caret)

fgl<- read.csv("Downloads/fgl.csv")
data(fgl)
set.seed(17)
fgl.rf <-randomForest(type ~., data =fgl,
                      mtry =2, importance = TRUE,
                      do.trace =100)

print(fgl.rf)

#Model Comparison

set.seed(131)
error.RF <- numeric(10)
for(i in 1:10) error.RF[i] <- errorest(type ~., data =fgl,
           model =randomForest, mtry = 2)$error
summary(error.RF)

library(e1071)
set.seed(563)
error.svm <- numeric(10)
for (i in 1:10) error.svm[i] <-
  errorest(type ~.,data = fgl,
           model = svm, cost =10, gamma =1.5)$error

summary(error.svm)

par(mfrow =c(2,2))
for (i in 1:4)
  plot(sort(fgl.rf$importance[,i], dec = TRUE))


###########################################################
#PARTB
data <- read.csv("Downloads/heart.csv")
head(data)

#check data 
str(data)
summary(data)

# 
data$target<- ifelse(test=data$target==0, yes="healthy", no="Unhealthy")
data$target<- as.factor(data$target)

#EDA
barplot(table(data$target))
plot_correlation(data, type = 'continuous','Review.Date')

#Split train and test data
set.seed(42)
index <-  which( (1:nrow(data))%%3 == 0 )

train <- data[-index,]

test <- data[index,]


#fit a model 
Model<-randomForest(target~., data = train, proximity=TRUE)
Model
plot(Model)

Model <- randomForest(target ~., data= train, ntree = 1000, proximity= TRUE)
Model
plot(Model)

#grid search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(100)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(target~., data=data, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

#error
oob_error_data<- data.frame(Trees=rep(1:nrow(Model$err.rate), times=3), Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(Model$err.rate)), Error=c(Model$err.rate[, "OOB"], Model$err.rate[, "healthy"], Model$err.rate[, "Unhealthy"]))

ggplot(data=oob_error_data, aes(x=Trees, y=Error))+geom_line(aes(color=Type))


varImpPlot(Model)


#Creating a vector that can hold ten values.
oob_values<- vector(length=10)

#Testing of the different numbers of variables at each step.
for(i in 1:10){temp_model <- randomForest(target~., data=data, 
                                          mtry=i, ntree=1000)
oob_values[i] <- temp_model$err.rate[nrow(temp_model$err.rate), 1]}

oob_values

library(e1071)
set.seed(563)
error.svm <- numeric(10)
for (i in 1:10) error.svm[i] <-
  errorest(target ~.,data = data,
           model = svm, cost =10, gamma =1)$error

summary(error.svm)








#



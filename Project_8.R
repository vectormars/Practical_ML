rm(list=ls())
cat("\014")

# Data
setwd("C:/Users/Jie Xue/Google Drive/Data Science/Course 8. Practical Machine Learning/Project 8")
### Loading the training data set into my R session replacing all missing with "NA"
Train<-read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!",""))
Test<-read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!",""))                
dim(Train)
## How classe distributed
Train.Class<-Train$classe
classe.freq<-table(Train.Class)
classe.freq<-as.data.frame(classe.freq)

## Bar plot with ggplot
library("ggplot2")
p<-ggplot(classe.freq,aes(x=Train.Class,y=Freq),fill=Train.Class)+
    geom_bar(stat="identity", fill="steelblue") +
    geom_text(aes(label=Freq), vjust=-0.3,size=3.5)+
    ylab("Classe Frequency") +
    xlab("Classe type") 
print(p)


# Pre-Processing
## Remove columns woth more than 50% NAs
Train <- Train[, colSums(is.na(Train)) < nrow(Train) * 0.5]
Test <- Test[, colSums(is.na(Test)) < nrow(Test) * 0.5]

## remove all Near Zero Variance variables
library(lattice)
library(caret)
NZV <- nearZeroVar(Train, saveMetrics= TRUE)
Train <- Train[,!NZV$nzv]
Test <- Test[,!NZV$nzv]

## remove unnecessary columns 1 to 6
Train<-Train[,-c(1:6) ]
Test<-Test[,-c(1:6) ]


## Partition data into 60% and 40%
set.seed(123)
DTrain<-createDataPartition(Train$classe, p=0.7, list=FALSE)
Train.T<-Train[DTrain,]
Train.CV<-Train[-DTrain,]


# Build prediction model
## First, Try decision tree
Model.DT<-train(classe ~ ., method="rpart",data=Train.T) 

Prediction.DT <- predict(Model.DT, Train.CV)


## Test results on our subTesting data set:
confusionMatrix(Prediction.DT, Train.CV$classe)



## Second, try random forest
library(randomForest)
Model.RF <- randomForest(classe~.,data=Train.T)
Prediction.RF <- predict(Model.RF, Train.CV)
confusionMatrix(Prediction.RF, Train.CV$classe)

## Importance 
importance <- varImp(Model.RF)
RN<-rownames(importance)
importance<-cbind(RN,importance)
library(dplyr)
importance<-arrange(importance,desc(Overall))
importance<-filter(importance,Overall>200)
p<-ggplot(importance,aes(x=RN,y=Overall),fill=Train.Class)+
    geom_bar(stat="identity", fill="steelblue") +
    geom_text(aes(label=Overall),hjust=0,size=3.5)+
    coord_flip()+
    ylab("Overall") +
    xlab("Important Features") 
print(p)






# Using the test data
Prediction.Test <- predict(Model.RF, Test)
Prediction.Test


# Submission


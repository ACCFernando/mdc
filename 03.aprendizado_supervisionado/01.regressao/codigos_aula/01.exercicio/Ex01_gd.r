#########################################################
#  MDC017 - Aprendizado Supervisionado I                #
#  Exercicio 01 - House Pricing - Descida do Gradiente  #
######################################################### 

source("gradDescentR.SupportFunctions.R")
source("gradDescentR.Methods.R")
source("gradDescentR.TopLevelFunctions.R")

library(ggplot2)
library(reshape2)

set.seed(40)

dataTrain <- read.csv("housePricing_train_set.csv", header=TRUE, stringsAsFactors=TRUE)
dataVal <- read.csv("housePricing_val_set.csv", header=TRUE, stringsAsFactors=TRUE)

dim(dataTrain)
summary(dataTrain)

dim(dataVal)
summary(dataVal)

any(is.na(dataTrain))
any(is.na(dataVal))

## Removing Categorical
dataTrain[,10] <- NULL
dataVal[,10] <- NULL

summary(dataTrain)
summary(dataVal)

# MinMax normalization
min_features <- apply(dataTrain[,1:8], 2, min)
max_features <- apply(dataTrain[,1:8], 2, max)
diff <- max_features - min_features

dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, min_features, "-")
dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, diff, "/")
summary(dataTrain)

dataVal[,1:8] <- sweep(dataVal[,1:8], 2, min_features, "-")
dataVal[,1:8] <- sweep(dataVal[,1:8], 2, diff, "/")
summary(dataVal)

## Training with GD ##
## It's expected that the last column of the training data is the target 
gd <- gradDescentR.learn(dataTrain, learningMethod = "GD",
                         featureScaling=FALSE, seed=42, 
                         control=list(alpha=0.01, maxIter=1000))
gd$model

#gd <- GD(dataTrain, alpha = 0.01, maxIter = 1000, seed = 42)

## In prediction, the number of columns must be the same as in the training. Since
## the last column on training is the target and we do not assume labels on validtaion/test
## we are going to add a columns of 0s

valPred <- predict(gd, cbind(dataVal[,1:8], 0))
valPred <- valPred[,ncol(valPred)]

trainPred <- predict(gd, cbind(dataTrain[,1:8], 0))
trainPred <- trainPred[,ncol(trainPred)]

MAE <- function(preds, labels){
  mae_values <- sum(abs(preds-labels))/length(preds)
  return(mae_values)
}

MSE <- function(preds, labels){
  mse_values <- sum((preds-labels)**2)/length(preds)
  return(mse_values)
}

mae_train_baseline <- MAE(trainPred, dataTrain$median_house_value)
mae_train_baseline

mae_val_baseline <- MAE(valPred, dataVal$median_house_value)
mae_val_baseline

## Changing number of iterations ##
iterations <- c(100, 200, 500, 1000, 5000)
MaePerNumIterations <- data.frame(NumIter=numeric(length(iterations)),
                                 TrainMAE=numeric(length(iterations)),
                                 ValMAE=numeric(length(iterations)))


i <- 1
for(iter in iterations){
  gd <- gradDescentR.learn(dataTrain, learningMethod = "GD",
                           featureScaling=FALSE, seed=42, 
                           control=list(alpha=0.01, maxIter=iter))
  
  valPred <- predict(gd, cbind(dataVal[,1:8], 0))
  valPred <- valPred[,ncol(valPred)]
  
  trainPred <- predict(gd, cbind(dataTrain[,1:8], 0))
  trainPred <- trainPred[,ncol(trainPred)]
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  mae_val <- MAE(valPred, dataVal$median_house_value)
  
  MaePerNumIterations[i,] <- c(i, mae_train, mae_val)
  i <- i + 1

}

MaePerNumIterationsMelt <- melt(MaePerNumIterations, id="NumIter")  # convert to long format
p <- ggplot(data=MaePerNumIterationsMelt, aes(x=NumIter, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Number of Iterations", 
                                                                        limits=as.character(c("100", "200", "500", "1000", "5000")))
p + theme(legend.position = c(0.85, 0.85), legend.title = element_blank())

## Changing learning rate ##
Learning_rates <- c(1e-4, 1e-3, 1e-2, 0.1, 1.0)
MaePerLR <- data.frame(LR=numeric(length(Learning_rates)),
                                  TrainMAE=numeric(length(Learning_rates)),
                                  ValMAE=numeric(length(Learning_rates)))

i <- 1
for(lr in Learning_rates){
  gd <- gradDescentR.learn(dataTrain, learningMethod = "GD",
                           featureScaling=FALSE, seed=42, 
                           control=list(alpha=lr, maxIter=1000))
  
  valPred <- predict(gd, cbind(dataVal[,1:8], 0))
  valPred <- valPred[,ncol(valPred)]
  
  trainPred <- predict(gd, cbind(dataTrain[,1:8], 0))
  trainPred <- trainPred[,ncol(trainPred)]
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  mae_val <- MAE(valPred, dataVal$median_house_value)
  
  MaePerLR[i,] <- c(i, mae_train, mae_val)
  i <- i + 1
  
}

MaePerLRMelt <- melt(MaePerLR, id="LR")  # convert to long format
p <- ggplot(data=MaePerLRMelt, aes(x=LR, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Number of Iterations", 
                                                                          limits=c("1e-4", "1e-3", "1e-2", "0.1", "1.0"))
p + theme(legend.position = c(0.7, 0.85), legend.title = element_blank())


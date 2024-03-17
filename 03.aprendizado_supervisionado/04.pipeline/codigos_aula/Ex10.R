#############################################
#  MDC014 - Aprendizado Supervisionado I    #
#  Exercise 10 - Economic Region Prediction #
#          Open Set Analysis                #  
############################################# 

library(glmnet)
library(caret)
set.seed(12)

calculaMatrizConfusaoRelativa <- function(cm, num_classes=5){
    
    # Aplicamos a transposicao para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    for(i in 1:num_classes){
        cm_relative[i,] = round(cm_absolute[i,]/sum(cm_absolute[i,]), digits=2)
    }
    
    return(cm_relative)  
}

trainSet <- read.csv("Known_training_set.csv", stringsAsFactors = T)

dim(trainSet)
any(is.na(trainSet))
summary(trainSet)


#### Normalization ####
min_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, min)
min_features

max_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, max)
max_features

diff <- max_features - min_features
diff

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, min_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, diff, "/")
summary(trainSet)

### Hypothesis Definition ###
hypothesis <- formula(target ~ .)

############ Training South America vs Rest #############
positiveTrainSet <- trainSet[trainSet$continent == "South America",]
negativeTrainSet <- trainSet[trainSet$continent != "South America",]

# removing the continent, since we know that it is "South america"
positiveTrainSet$continent <- NULL

# Also removing from negative data, since we know it is not "South America"
negativeTrainSet$continent <- NULL 

## Assigning target 1 for South America
positiveTrainSet$target <- 1

## Assigning target 0 for Rest
negativeTrainSet$target <- 0

## Training One-vs-Rest
train <- rbind(positiveTrainSet, negativeTrainSet)
table(train$target)

x_train <- model.matrix(hypothesis, train)
y_train <- train$target

logReg01 <-  glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, alpha=0, lambda = 1e-6)


############ Training Central America vs Rest ############
positiveTrainSet <- trainSet[trainSet$continent == "Central America",]
negativeTrainSet <- trainSet[trainSet$continent != "Central America",]

# removing the continent, since we know that it is "Central America"
positiveTrainSet$continent <- NULL

# Also removing from negative data, since we know it is not "Central America"
negativeTrainSet$continent <- NULL 

## Assigning target 1 for Central America
positiveTrainSet$target <- 1

## Assigning target 0 for Rest
negativeTrainSet$target <- 0

## Training One-vs-Rest
train <- rbind(positiveTrainSet, negativeTrainSet)
table(train$target)

x_train <- model.matrix(hypothesis, train)
y_train <- train$target

logReg02 <-  glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, alpha=0, lambda = 1e-6)


############ Training Eastern Africa vs Rest ############
positiveTrainSet <- trainSet[trainSet$continent == "Eastern Africa",]
negativeTrainSet <- trainSet[trainSet$continent != "Eastern Africa",]

# removing the continent, since we know that it is "Eastern Africa"
positiveTrainSet$continent <- NULL

# Also removing from negative data, since we know it is not "Eastern Africa"
negativeTrainSet$continent <- NULL 

## Assigning target 1 for Eastern Africa
positiveTrainSet$target <- 1

## Assigning target 0 for Rest
negativeTrainSet$target <- 0

## Training One-vs-Rest
train <- rbind(positiveTrainSet, negativeTrainSet)
table(train$target)

x_train <- model.matrix(hypothesis, train)
y_train <- train$target

logReg03 <-  glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, alpha=0, lambda = 1e-6)


############ Training South-Eastern Asia vs Rest ############
positiveTrainSet <- trainSet[trainSet$continent == "South-Eastern Asia",]
negativeTrainSet <- trainSet[trainSet$continent != "South-Eastern Asia",]

# removing the continent, since we know that it is "South-Eastern Asia"
positiveTrainSet$continent <- NULL

# Also removing from negative data, since we know it is not "South-Eastern Asia"
negativeTrainSet$continent <- NULL 

## Assigning target 1 for South-Eastern Asia
positiveTrainSet$target <- 1

## Assigning target 0 for Rest
negativeTrainSet$target <- 0

## Training One-vs-Rest
train <- rbind(positiveTrainSet, negativeTrainSet)
table(train$target)

x_train <- model.matrix(hypothesis, train)
y_train <- train$target

logReg04 <-  glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, alpha=0, lambda = 1e-6)


############ Training Western Europe vs Rest ############
positiveTrainSet <- trainSet[trainSet$continent == "Western Europe",]
negativeTrainSet <- trainSet[trainSet$continent != "Western Europe",]

# removing the continent, since we know that it is "Western Europe"
positiveTrainSet$continent <- NULL

# Also removing from negative data, since we know it is not "Western Europe"
negativeTrainSet$continent <- NULL 

## Assigning target 1 for Western Europe
positiveTrainSet$target <- 1

## Assigning target 0 for Rest
negativeTrainSet$target <- 0

## Training One-vs-Rest
train <- rbind(positiveTrainSet, negativeTrainSet)
table(train$target)

x_train <- model.matrix(hypothesis, train)
y_train <- train$target

logReg05 <-  glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, alpha=0, lambda = 1e-6)


########### Predicting on Validation Set ############
valSet <- read.csv("Known_validation_set.csv", stringsAsFactors = T)
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, min_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, diff, "/")
summary(valSet)

# Putting -1, since some value must be on "target" column
# In fact, the value of this column will be predicted now
valSet$target <- -1

### We will store the groud truth of each sample on validation
### in this variable:
gt_valSet <- valSet$continent
valSet$continent <- NULL

# Validation
x_val <- model.matrix(hypothesis, valSet)

valPred01 <- predict(logReg01, newx = x_val, type="response")
valPred02 <- predict(logReg02, newx = x_val, type="response")
valPred03 <- predict(logReg03, newx = x_val, type="response")
valPred04 <- predict(logReg04, newx = x_val, type="response")
valPred05 <- predict(logReg05, newx = x_val, type="response")

### To count the votes, we will define a one-hot encoding template
### for each class. Then, we will get the minimum Manhattan distance
### between the predicted vector of probabilities and the templates.
### The lowest distance is the predicted class
valPred <- cbind(valPred01, valPred02, valPred03, valPred04, valPred05)
valPred

classVectors <- diag(5)
classes_names <- c("South America", "Central America",
                   "Eastern Africa","South-Eastern Asia", 
                   "Western Europe")

colnames(classVectors) <- classes_names

# Printing class vectors
classVectors

valDists <- c()
for (idx in 1:nrow(valPred)) {
    distances <- apply(classVectors, 1, 
                       function(x){ 
                           dist(rbind(valPred[idx,],x), method="manhattan")
                       })
    valDists <- rbind(valDists, distances)
}

colnames(valDists) <- classes_names

valDists[1:5,]
#### Let is get the minimum distance in each row. The respective class
#### is the predicted class.
valClass <- colnames(valDists)[apply(valDists, 1, which.min)]

#### Confusion Matrix #### 
cm <- confusionMatrix(data = as.factor(valClass), 
                      reference = as.factor(gt_valSet))

cm_relative <- calculaMatrizConfusaoRelativa(cm, num_classes=5)
cm_relative

# Balanced Accuracy by taking the mean of sensitivities (TPR's)
acc_bal <- mean(diag(cm_relative))
acc_bal

#####################################
#   Open Set - Threshold Decision   #
#####################################

# Load a set of unknown classes to search for the best
# open-set threshold
valSet_OpenSet <- read.csv("Unknown_validation_set.csv", stringsAsFactors = T)
valSet_OpenSet[,1:(ncol(valSet_OpenSet)-1)] <- sweep(valSet_OpenSet[,1:(ncol(valSet_OpenSet)-1)], 2, min_features, "-")
valSet_OpenSet[,1:(ncol(valSet_OpenSet)-1)] <- sweep(valSet_OpenSet[,1:(ncol(valSet_OpenSet)-1)], 2, diff, "/")
summary(valSet_OpenSet)

# Putting -1, since some value must be on "target" column
# In fact, the value of this column will be predicted now
valSet_OpenSet$target <- -1

### We will store the groud truth of each sample on validation
### in this variable:
gt_valSet_OpenSet <- valSet_OpenSet$continent
valSet_OpenSet$continent <- NULL

# Validation
x_val_openset <- model.matrix(hypothesis, valSet_OpenSet)

valPredOpenSet01 <- predict(logReg01, newx = x_val_openset, type="response")
valPredOpenSet02 <- predict(logReg02, newx = x_val_openset, type="response")
valPredOpenSet03 <- predict(logReg03, newx = x_val_openset, type="response")
valPredOpenSet04 <- predict(logReg04, newx = x_val_openset, type="response")
valPredOpenSet05 <- predict(logReg05, newx = x_val_openset, type="response")

### To count the votes, we will define a one-hot encoding template
### for each class. Then, we will get the minimum Manhattan distance
### between the predicted vector of probabilities and the templates.
### The lowest distance is the predicted class
valPredOpenSet <- cbind(valPredOpenSet01, valPredOpenSet02, valPredOpenSet03, 
                        valPredOpenSet04, valPredOpenSet05)

colnames(valPredOpenSet) <- c("model 1", "model 2", "model 3", "model 4", "model 5")
valPredOpenSet[1:3,]

# Let's take the biggest probability for each example on Open Set
# and on known Validation Set
unknown_MaxProb <- apply(valPredOpenSet, 1, max)
unknown_MaxProb[1:3]

known_MaxProb <- apply(valPred, 1, max)


# We are going to search for a threshold to separate known and 
# unknown classes. We are assuming that a known case will have 
# a probability big enough to its corresponding class, as long as
# the unknown classes will not belong to any of the known classes. 
# So we would expect that an open set case will have low probabilities
# for all of the classes.
thresholdSearching <- seq(0.5,0.9,length=10)
thresholdSearching

# For each threhsold we will test it for classifying known and 
# unknown classes

# MaxProbs >= threshold ----> Known Class
# MaxProbs < threshold  ----> Unknown Class

accK <- c()
accUk <- c()
for (idx in 1:length(thresholdSearching)) {
    t <- thresholdSearching[idx]
    
    accK[idx] <- sum(known_MaxProb >= t) / length(known_MaxProb)
    accUk[idx] <- sum(unknown_MaxProb < t) / length(unknown_MaxProb)
}

acc <- (accK + accUk) / 2.0

#Plot das acur?cias
plot(accK, type="o", col="blue", xaxt="n", xlab="t", ylab="Acc", ylim = c(0.0,1.0), main="ACC x t")
axis(1, at=1:length(thresholdSearching), labels=thresholdSearching, cex.axis=0.5, las=2)

points(accUk, col="red", pch="*")
lines(accUk, col="red",lty=2)

points(acc, col="green", pch="*")
lines(acc, col="green",lty=3)

legend(7, 0.2, legend=c("Known Acc", "Unknown Acc", "Mean Acc"), 
       col=c("blue","red", "green"), pch=c("__"), cex=0.7, pt.cex = 1)

# Let's choose the best threshold
openSetThreshold <- 0.75


##################################################################
# Final Performance on a Test Set with known and unknown classes #
##################################################################
testSet <- read.csv("OpenSet_test_set.csv")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, min_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, diff, "/")
summary(testSet)

# Putting -1, since some value must be on "target" column
# In fact, the value of this column will be predicted now
testSet$target <- -1

### We will store the groud truth of each sample on validation
### in this variable:
gt_testSet <- testSet$continent

## We will change the name of the unknown classes for "yUnknown". 
## Then we will have a ground truth to the classes that are not 
## presented on the expected classes. So, we will have the labels
## for the known classes and for the unknown ones. 
gt_testSet[!(gt_testSet %in% classes_names)] <- "yUnknown"
gt_testSet <- as.factor(gt_testSet)

# Remove the continent
testSet$continent <- NULL

# Validation
x_test <- model.matrix(hypothesis, testSet)

testPred01 <- predict(logReg01, newx = x_test, type="response")
testPred02 <- predict(logReg02, newx = x_test, type="response")
testPred03 <- predict(logReg03, newx = x_test, type="response")
testPred04 <- predict(logReg04, newx = x_test, type="response")
testPred05 <- predict(logReg05, newx = x_test, type="response")

### To count the votes, we will define a one-hot encoding template
### for each class. Then, we will get the minimum Manhattan distance
### between the predicted vector of probabilities and the templates.
### The lowest distance is the predicted class
testPred <- cbind(testPred01, testPred02, testPred03, 
                  testPred04, testPred05)

# Taking best probability per sample on test set
test_MaxProb <- apply(testPred, 1, max)

# Reviewing the class vectors expected for each class
classVectors

testDists <- c()
for (idx in 1:nrow(testPred)) {
    distances <- apply(classVectors, 1, 
                       function(x){ 
                           dist(rbind(testPred[idx,],x), method="manhattan")
                       })
    testDists <- rbind(testDists, distances)
}

# If the max probability is bigger than the threshold, we classify it
# as known class. Otherwise, it will be predicted as unknown class.
testClass <- c()
for (idx in 1:nrow(testPred)) {
    testClass[idx] <- ifelse(test_MaxProb[idx] >= openSetThreshold,
                             classes_names[which.min(testDists[idx,])],
                             "yUnknown")
}

# Calculando a matrix de confusao
cm <- confusionMatrix(data=as.factor(testClass), reference=as.factor(gt_testSet))

cm_relative <- calculaMatrizConfusaoRelativa(cm, num_classes=6)
cm_relative

# Acuracia balanceada individual
diag(cm_relative)

# Acuracia balanceada geral
acc_bal <- mean(diag(cm_relative))
acc_bal

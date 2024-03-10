###########################################
# MDC017 - Aprendizado Supervisionado 01  # 
# Exercicio 06 - Um-contra-todos          #
###########################################

library(glmnet)
library(caret)

# Calcula a matriz de confusao relativa 
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposicao para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
    cm_relative[4,] = round(cm_absolute[4,]/sum(cm_absolute[4,]), digits=2)
    cm_relative[5,] = round(cm_absolute[5,]/sum(cm_absolute[5,]), digits=2)
   
    return(cm_relative)  
}

trainSet <- read.csv("OvA_training_set.csv", stringsAsFactors = T)
valSet <- read.csv("OvA_validation_set.csv", stringsAsFactors = T)

merge(trainSet, valSet)

dim(trainSet)
dim(valSet)

any(is.na(trainSet))
any(is.na(valSet))

summary(trainSet)
summary(valSet)

merge(trainSet, valSet)

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

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, min_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, diff, "/")
summary(valSet)

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
colnames(classVectors) <- c("South America", "Central America",
                            "Eastern Africa","South-Eastern Asia", 
                            "Western Europe")

classVectors

valDists <- c()
for (idx in 1:nrow(valPred)) {
    dist <- apply(classVectors, 1, 
                  function(x){ 
                      dist(rbind(valPred[idx,],x), method="manhattan")
                  })
    valDists <- rbind(valDists, dist)
}

colnames(valDists) <- c("South America", "Central America",
                        "Eastern Africa","South-Eastern Asia", 
                        "Western Europe")
valDists[1:5,]
#### Let is get the minimum distance in each row. The respective class
#### is the predicted class.
valClass <- colnames(valDists)[apply(valDists, 1, which.min)]
valClass
#### Confusion Matrix #### 
cm <- confusionMatrix(data = as.factor(valClass), 
                      reference = as.factor(gt_valSet))

# 5 x 5
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# Balanced Accuracy by taking the mean of sensitivities (TPR's)
acc_bal <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3] +
                      cm_relative[4,4] + cm_relative[5,5])/5
acc_bal


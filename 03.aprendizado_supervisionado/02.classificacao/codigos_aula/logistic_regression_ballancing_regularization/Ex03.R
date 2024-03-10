##########################################
# MDC017 - Aprendizado Supervisionado 01 #
# Exercicio 03 - Regressao Logistica     #
##########################################

#install.packages("glmnet")
#install.packages("caret")
#install.packages("pROC")

source("support_functions.R")

set.seed(13)


library(glmnet)
library(caret)
library(pROC)
library(ggplot2)
library(reshape2)

### Carregando as bases de dados ### 
trainSet <- read.csv("cholesterol_training_set.csv")
valSet <- read.csv("cholesterol_validation_set.csv")
testSet <- read.csv("cholesterol_test_set.csv")

summary(trainSet)
summary(valSet)
summary(testSet)

dim(trainSet)
dim(valSet)
dim(testSet)

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)


trainSet$class <- as.factor(trainSet$class)
valSet$class <- as.factor(valSet$class)
testSet$class <- as.factor(testSet$class)

### Verifica Frequencia de cada uma das classes ###
table(trainSet$class)
table(valSet$class)
table(testSet$class)

## Normalizacao Z-norma 
mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)
mean_features

sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)
sd_features

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")
summary(trainSet)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")
summary(testSet)

############ Training Models ############
feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]
feature_names

hypothesis <- getHypothesis(feature_names, 2)
hypothesis


help(glmnet)
x_train <- model.matrix(hypothesis, trainSet)
x_train
y_train <- trainSet$class
y_train


model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE,
                maxit = 1e+05, alpha=0, lambda = 1e-2)

### Verificando os thetas aprendidos ###
model$beta
model$a0 # valor do theta0 (intercept)

trainPred <- predict(model, newx = x_train, type="response")
trainPred

#converting to class
trainClassPred <- trainPred

#### THRESHOLD ####
# Threshold = 0.5 
trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0
trainClassPred

#### Balanced Loss
lossN = getLoss(trainSet$class[trainSet$class == 0], trainPred[trainSet$class == 0])
lossP = getLoss(trainSet$class[trainSet$class == 1], trainPred[trainSet$class == 1])
lossN
lossP
(lossN+lossP)/2


cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$class), 
                      positive='1')


cm$table # Devemos transpo-la para deixar os labels nas linhas 
         # e as predicoes nas colunas. Bem como devemos deixa-la
         # em valores relativos de forma que as linhas somem 100%.
         # A funcao "calculaMatrizConfusaoRelativa" ja realiza
         # ambos os procedimentos. 

# SEMPRE construam e reportem a matriz de confusao relativa!
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

### Predicao no conjunto de validacao ###
x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(model, newx = x_val, type="response")

#valPred

#converting to class
valClassPred <- valPred


#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0
#valClassPred

# threshold = 0.9
#valClassPred[valPred >= 0.9] <- 1
#valClassPred[valPred < 0.9] <- 0


##### Let's see how well we did
#Loss 
lossN = getLoss(valSet$class[valSet$class == 0], valPred[valSet$class == 0])
lossP = getLoss(valSet$class[valSet$class == 1], valPred[valSet$class == 1])
lossN
lossP
loss_baseline <- (lossN+lossP)/2
loss_baseline

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')


cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline

# Curva ROC para o baseline. Lembre-se que ela deve ser plotada 
# para o conjunto de validacao, eventualmente para o treinamento mas
# NUNCA para o conjunto de teste
ROC <- roc(valSet$class, valPred[,1], direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")

############# Polynomial analysis ###########
LossPerDegree <- data.frame(degree=numeric(7), 
                           TrainLoss=numeric(7),
                           ValLoss=numeric(7))

AccPerDegree <- data.frame(degree=numeric(7), 
                           TrainACC=numeric(7),
                           ValACC=numeric(7))

feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]

## Polynomial Analysis
### Be careful! Higher polynomial degrees might not converge!
for(i in 1:7){  
    
    print(i)
    hypothesis <- getHypothesis(feature_names, i)
    
    # Applying hypothesis and training the model
    x_train <- model.matrix(hypothesis, trainSet)
    y_train <- trainSet$class
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-2)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$class[trainSet$class == 0], trainPred[trainSet$class == 0])
    lossP = getLoss(trainSet$class[trainSet$class == 1], trainPred[trainSet$class == 1])
    mean_loss_train <- (lossN+lossP)/2
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$class), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
 
    # Validation
    x_val <- model.matrix(hypothesis, valSet)
    y_val <- valSet$class
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$class[valSet$class == 0], valPred[valSet$class == 0])
    lossP = getLoss(valSet$class[valSet$class == 1], valPred[valSet$class == 1])
    mean_loss_val <- (lossN+lossP)/2
    mean_loss_val
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$class), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    LossPerDegree[i,] <- c(i, mean_loss_train, mean_loss_val)
    AccPerDegree[i,] <- c(i, acc_bal_train, acc_bal_val) 
   
}

############# Plotting Loss ############
LossPerDegreeMelt <- melt(LossPerDegree, id="degree")  # convert to long format
p <- ggplot(data=LossPerDegreeMelt, aes(x=degree, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("Loss") + scale_x_discrete(name ="Degree", 
                                                                          limits=as.character(1:5))
p + theme(legend.position = c(0.4, 0.9), legend.title = element_blank())

############ Ploting Acc Balanced ############
AccPerDegreeMelt <- melt(AccPerDegree, id="degree")  # convert to long format
p <- ggplot(data=AccPerDegreeMelt, aes(x=degree, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("ACC") + scale_x_discrete(name ="Degree", 
                                                                          limits=as.character(1:5))
p + theme(legend.position = c(0.2, 0.1), legend.title = element_blank())


#### Testing #### 
i<- which.max(AccPerDegree$ValACC)
i

hypothesis <- getHypothesis(feature_names, i)

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$class
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-2)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$class
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$class[testSet$class == 0], testPred[testSet$class == 0])
lossP = getLoss(testSet$class[testSet$class == 1], testPred[testSet$class == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test 


cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_test_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_test_bal


############ Combining Features ###########
cor(trainSet[1:(ncol(trainSet)-1)])

f01 <- formula(class ~ .)

f02 <- formula(class ~ . + (LBXTR+LBDHDD+LBXGLT+LBXAPB)^2)

f03 <- formula(class ~ . + (LBXTR+LBDHDD+LBXGLT+LBXAPB)^3)

f04 <- formula(class ~ . + (LBXTR+LBDHDD+LBXGLT+LBXAPB)^4)


formulas <- c(f01, f02, f03, f04)

LossPerCombination <- data.frame(combination=numeric(length(formulas)), 
                            TrainLoss=numeric(length(formulas)),
                            ValLoss=numeric(length(formulas)))

AccPerCombination <- data.frame(combination=numeric(length(formulas)), 
                           TrainACC=numeric(length(formulas)),
                           ValACC=numeric(length(formulas)))

i <- 1
for(f in formulas){  
    
    
    # Applying hypothesis and training the model
    x_train <- model.matrix(f, trainSet)
    y_train <- trainSet$class
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-3)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$class[trainSet$class == 0], trainPred[trainSet$class == 0])
    lossP = getLoss(trainSet$class[trainSet$class == 1], trainPred[trainSet$class == 1])
    mean_loss_train <- (lossN+lossP)/2
    print(mean_loss_train)
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$class), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    
    # Validation
    x_val <- model.matrix(f, valSet)
    y_val <- valSet$class
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$class[valSet$class == 0], valPred[valSet$class == 0])
    lossP = getLoss(valSet$class[valSet$class == 1], valPred[valSet$class == 1])
    mean_loss_val <- (lossN+lossP)/2
    
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$class), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    LossPerCombination[i,] <- c(i, mean_loss_train, mean_loss_val)
    AccPerCombination[i,] <- c(i, acc_bal_train, acc_bal_val) 
    
    i <- i + 1
}

############# Plotting Loss ############
LossPerCombinationMelt <- melt(LossPerCombination, id="combination")  # convert to long format
p <- ggplot(data=LossPerCombinationMelt, aes(x=combination, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:length(formulas)))
p + theme(legend.position = c(0.7, 0.50), legend.title = element_blank())

############ Ploting Acc Balanced ############
AccPerCombinationMelt <- melt(AccPerCombination, id="combination")  # convert to long format
p <- ggplot(data=AccPerCombinationMelt, aes(x=combination, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("ACC Balanced") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:length(formulas)))
p + theme(legend.position = c(0.7, 0.50), legend.title = element_blank())



#### Testing #### 
i<- which.max(AccPerCombination$ValACC)
i

f <- formulas[[i]]
x_train <- model.matrix(f, trainSet)
y_train <- trainSet$class
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-3)


x_test <- model.matrix(f, testSet)
y_test <- testSet$class
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$class[testSet$class == 0], testPred[testSet$class == 0])
lossP = getLoss(testSet$class[testSet$class == 1], testPred[testSet$class == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test


cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test



#### Varying features in the model ####

# Checking parameters and the respective values
model$beta

number_of_parameters = length(model$beta)
sorted_indexes = order(abs(model$beta[2:number_of_parameters]), decreasing = TRUE)
best_features = rownames(model$beta)[2:number_of_parameters][sorted_indexes]
best_features

LossPerCombination <- data.frame(combination=numeric(number_of_parameters-1), 
                                 TrainLoss=numeric(number_of_parameters-1),
                                 ValLoss=numeric(number_of_parameters-1))

AccPerCombination <- data.frame(combination=numeric(number_of_parameters-1), 
                                TrainACC=numeric(number_of_parameters-1),
                                ValACC=numeric(number_of_parameters-1))

i <- 1
for(i in 1:(number_of_parameters-1)){  
    
    
    # Applying hypothesis and training the model
    f <- write_hypotesis_from_string(best_features[1:i])
    
    x_train <- model.matrix(f, trainSet)
    y_train <- trainSet$class
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-3)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$class[trainSet$class == 0], trainPred[trainSet$class == 0])
    lossP = getLoss(trainSet$class[trainSet$class == 1], trainPred[trainSet$class == 1])
    mean_loss_train <- (lossN+lossP)/2
    print(mean_loss_train)
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$class), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    
    # Validation
    x_val <- model.matrix(f, valSet)
    y_val <- valSet$class
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$class[valSet$class == 0], valPred[valSet$class == 0])
    lossP = getLoss(valSet$class[valSet$class == 1], valPred[valSet$class == 1])
    mean_loss_val <- (lossN+lossP)/2
    
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$class), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    LossPerCombination[i,] <- c(i, mean_loss_train, mean_loss_val)
    AccPerCombination[i,] <- c(i, acc_bal_train, acc_bal_val) 
    
    i <- i + 1
}

############# Plotting Loss ############
LossPerCombinationMelt <- melt(LossPerCombination, id="combination")  # convert to long format
p <- ggplot(data=LossPerCombinationMelt, aes(x=combination, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:(number_of_parameters-1)))
p + theme(legend.position = c(0.7, 0.50), legend.title = element_blank())

############ Ploting Acc Balanced ############
AccPerCombinationMelt <- melt(AccPerCombination, id="combination")  # convert to long format
p <- ggplot(data=AccPerCombinationMelt, aes(x=combination, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("ACC Balanced") + scale_x_discrete(name ="Combination", 
                                                                                   limits=as.character(1:(number_of_parameters-1)))
p + theme(legend.position = c(0.7, 0.50), legend.title = element_blank())



#### Testing #### 
i<- which.max(AccPerCombination$ValACC)
best_combination <- write_hypotesis_from_string(best_features[1:i])
best_combination

x_train <- model.matrix(best_combination, trainSet)
y_train <- trainSet$class
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-3)


x_test <- model.matrix(best_combination, testSet)
y_test <- testSet$class
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$class[testSet$class == 0], testPred[testSet$class == 0])
lossP = getLoss(testSet$class[testSet$class == 1], testPred[testSet$class == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test


cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test



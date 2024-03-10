##########################################
# MDC017 - Aprendizado Supervisionado 01 #
# Exercicio 05 - Balanceamento           #
##########################################

library(glmnet)
library(caret)

source("support_functions.R")
source("DMwR.R")

set.seed(13)

trainSet <- read.csv("cholesterol_training_set.csv", stringsAsFactors = TRUE)
valSet <- read.csv("cholesterol_validation_set.csv", stringsAsFactors = TRUE)

summary(trainSet)
summary(valSet)

dim(trainSet)
dim(valSet)

merge(trainSet, valSet)

trainSet$class <- as.factor(trainSet$class)
valSet$class <- as.factor(valSet$class)

### Verifica Frequencia de cada uma das classes ###
table(trainSet$class)
table(valSet$class)

# Z-norm normalization
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


############ Training Models ############
# Mesma hipotese ao longo de todo este exercicio: polinomio de grau 1.
# Assim verificamos o impacto exclusivamente do balanceamento sem interfer?ncia
# de modelos polinomiais ou de combinacao de features e sem regularizacao (lambda
# com valor muito proximo a zero, no caso, 1e-6)
feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]
hypothesis <- getHypothesis(feature_names, 1)
hypothesis 

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$class

model <-  glmnet(x_train, y_train,  family="binomial", maxit=1e+5,
                 standardize = FALSE, alpha=0, lambda = 1e-6)

trainPred <- predict(model, newx = x_train, type="response")

#converting to class
trainClassPred <- trainPred

#### THRESHOLD ####
# Threshold = 0.5 
trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
acc_bal_train_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_train_baseline

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

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')


cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline


####### Balanceamento por ponderacao da funcao de erro #######
classes_frequency = table(trainSet$class)
classes_frequency

relative_classes_frequency = classes_frequency/sum(classes_frequency)
relative_classes_frequency

w_positive = 1 - relative_classes_frequency[2]
w_negative = 1 - relative_classes_frequency[1]

w_positive
w_negative

# Inicializando com zeros o vetor de pesos
weights <- rep(0.0, dim(trainSet)[1])

# Associando o peso dos positivos (w_positive) aos respectivos exemplos
weights[trainSet$class == 1] = w_positive 

# Associando o peso dos negatives (w_negative) aos respectivos exemplos
weights[trainSet$class == 0] = w_negative 

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$class

logRegModel_weighting <- glmnet(x_train, y_train,  family="binomial",
                                weights = weights,
                                standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_weighting, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal_weights <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_weights


####### Balanceamento por Oversampling #######
trainSet <- read.csv("cholesterol_training_set.csv", stringsAsFactors = TRUE)
valSet <- read.csv("cholesterol_validation_set.csv", stringsAsFactors = TRUE)

positiveData <- trainSet[trainSet$class == 1,]
negativeData <- trainSet[trainSet$class == 0,]

dim(positiveData)
dim(negativeData)

# increasing in 2x
selectedIndex <- sample(1:nrow(positiveData), 2*nrow(positiveData), replace=TRUE)
oversampledPosData <- positiveData[selectedIndex,]
dim(oversampledPosData)

newTrainSet <- rbind(oversampledPosData, negativeData)
dim(newTrainSet)
table(newTrainSet$class)


# Z-norm normalization
mean_features <- apply(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, mean)
mean_features

sd_features <- apply(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, sd)
sd_features

newTrainSet[,1:(ncol(newTrainSet)-1)] <- sweep(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, mean_features, "-")
newTrainSet[,1:(ncol(newTrainSet)-1)] <- sweep(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, sd_features, "/")
summary(newTrainSet)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

x_train <- model.matrix(hypothesis, newTrainSet)
y_train <- newTrainSet$class

logRegModel_oversampling <- glmnet(x_train, y_train,  family="binomial", 
                                   standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_oversampling, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_oversampling <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_oversampling


####### Balanceamento por Undersampling #######
trainSet <- read.csv("cholesterol_training_set.csv", stringsAsFactors = TRUE)
valSet <- read.csv("cholesterol_validation_set.csv", stringsAsFactors = TRUE)

positiveData <- trainSet[trainSet$class == 1,]
negativeData <- trainSet[trainSet$class == 0,]

dim(positiveData)
dim(negativeData)

selectedIndex <- sample(1:nrow(negativeData), size=1.2*nrow(positiveData), replace=FALSE)
undersampledNegData <- negativeData[selectedIndex,]
dim(undersampledNegData)
dim(positiveData)

newTrainSet <- rbind(positiveData, undersampledNegData)
dim(newTrainSet)
table(newTrainSet$class)

# Z-norm normalization
mean_features <- apply(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, mean)
mean_features

sd_features <- apply(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, sd)
sd_features

newTrainSet[,1:(ncol(newTrainSet)-1)] <- sweep(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, mean_features, "-")
newTrainSet[,1:(ncol(newTrainSet)-1)] <- sweep(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, sd_features, "/")
summary(newTrainSet)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

x_train <- model.matrix(hypothesis, newTrainSet)
y_train <- newTrainSet$class

logRegModel_undersampling <- glmnet(x_train, y_train,  family="binomial", 
                                    standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_undersampling, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_undersampling <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_undersampling


##################################
############ SMOTE ###############
##################################
trainSet <- read.csv("cholesterol_training_set.csv", stringsAsFactors = TRUE)
valSet <- read.csv("cholesterol_validation_set.csv", stringsAsFactors = TRUE)

#help(SMOTE)
# Be carefull!!!! SMOTE only works with factor as labels
trainSet$class <- as.factor(trainSet$class) # Categorization
newTrainSet <- SMOTE(hypothesis, trainSet, 
                     perc.over = 100,  
                     perc.under = 200, 
                     k=3)

# per.over/100 eh o numero de novos exemplos sinteticos gerados para cada
# exemplo da classe minoritaria. Ou seja, se perc.over = 100, logo gera-se
# 100/100 = 1 exemplo sintetico para cada exemplo da classe minoritaria. 
# Como ha 465 exemplos na minoritaria, e para cada um deles um novo sintetico
# eh gerado, entao a classe minoritaria passa a ter 
# 465 (exemplos originais) + 465 (exemplos sinteticos) = 930 exemplos 

# perc.under/100 eh o numero de elementos amostrados da classe majoritaria
# para cada exemplo sintetico gerado. Como foram gerados 465 exemplos sinteticos
# sao amostrados 200/100 = 2 exemplos da classe majoritaria para cada um deles, 
# logo a classe majoritaria passar a ter 
# 465 (numero de exemplos sinteticos) * 2 = 930 exemplos 

# k = 3 eh o numero de vizinhos utilizaddos para cada exemplo da classe
# minoritaria para gerar novos exemplos

# Repare que apos esse processo, ambas as classes ficam
# com a mesma quantidade de exemplos para cada classe. No entanto,
# um bom balanceamento nao necessariamente significa duas (ou mais)
# classes com exatamente as mesmas quantidades. A frequencia otima de
# balanceamento depende do problema.

dim(newTrainSet)
table(newTrainSet$class)

# Z-norm normalization
mean_features <- apply(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, mean)
mean_features

sd_features <- apply(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, sd)
sd_features

newTrainSet[,1:(ncol(newTrainSet)-1)] <- sweep(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, mean_features, "-")
newTrainSet[,1:(ncol(newTrainSet)-1)] <- sweep(newTrainSet[,1:(ncol(newTrainSet)-1)], 2, sd_features, "/")
summary(newTrainSet)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

x_train <- model.matrix(hypothesis, newTrainSet)
y_train <- newTrainSet$class

logRegModel_SMOTE <- glmnet(x_train, y_train,  family="binomial", 
                            standardize = FALSE, alpha=0, lambda=1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_SMOTE, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_smote <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_smote

##### Getting best balancing technique #####
acc_bal_baseline*100
acc_bal_weights*100
acc_bal_oversampling*100
acc_bal_undersampling*100
acc_bal_smote*100

# Ha um empate entre o balanceamento por pesos e o undersampling, no entanto,
# o undersampling realiza a amostragem descartando parte dos
# dados de treinamento. Dessa maneira, optaremos pelo balanceamento por pesos.

# Carregando e processando o conjunto de teste para predicao sobre 
# a regressao logistica treinada por podenderacao por pesos
trainSet <- read.csv("cholesterol_training_set.csv", stringsAsFactors = TRUE)
testSet <- read.csv("cholesterol_test_set.csv", stringsAsFactors = TRUE)

summary(trainSet)
summary(testSet)

dim(trainSet)
dim(testSet)

merge(trainSet, testSet)

trainSet$class <- as.factor(trainSet$class)
testSet$class <- as.factor(testSet$class)

### Verifica Frequencia de cada uma das classes ###
table(trainSet$class)
table(testSet$class)

# Z-norm normalization
mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)
mean_features

sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)
sd_features

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")
summary(trainSet)

testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")
summary(testSet)

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$class

## logRegModel here comes from undersampling! Since it gave the best
## performance on validation set after balancing
testPred <- predict(logRegModel_weighting, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_weighting_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_weighting_test


###########################################
# MDC017 - Aprendizado Supervisionado I   #
# Exercicio 09 - Ensembles                #
# Analise de Telemarketing de um Banco    #
########################################### 

library(rpart)
library(rpart.plot)
library(caret)
library(ramify)


set.seed(40)

# Calcula a matriz de confusao relativa 
calculaMatrizConfusaoRelativa <- function(cm){
  
  # Aplicamos a transposicao para garantir que a referencia
  # fique nas linhas e a predicao nas colunas
  cm_absolute = t(cm$table)
  
  # SEMPRE construam e reportem a matriz de confusao relativa!
  cm_relative = cm_absolute
  
  cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
  cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
  
  return(cm_relative)  
}

data <- read.csv("bank-full.csv", header=TRUE, sep=";", stringsAsFactors=TRUE)

dim(data)
any(is.na(data))
summary(data)

# Este atributo deve obrigatoriamente ser retirado segundo
# os autores da base para que haja um treinamento e validacao justos.
data[,"duration"] <- NULL

data <- unique(data)

# Treino-Validacao 80% / Teste 20%
randomTrainValIndexes <- sample(1:nrow(data), size=0.8*nrow(data))
dataTrainVal <- data[randomTrainValIndexes, ]
dataTest  <- data[-randomTrainValIndexes, ] 

randomTrainIndexes <- sample(1:nrow(dataTrainVal), size=0.8*nrow(dataTrainVal))
dataTrain <- dataTrainVal[randomTrainIndexes, ]
dataVal  <- dataTrainVal[-randomTrainIndexes, ] 

merge(dataTrain, dataVal)
merge(dataTrain, dataTest)
merge(dataVal, dataTest)

dim(dataTrain)
dim(dataVal)
dim(dataTest)

# Faz o balanceamento das classes
dataTrainNo <- dataTrain[dataTrain$y == "no",]
dataTrainYes <- dataTrain[dataTrain$y == "yes",] 

dim(dataTrainYes)
dim(dataTrainNo)

lowest_samples <- min(nrow(dataTrainNo), nrow(dataTrainYes))
lowest_samples

###############
#   BAGGING   #
###############
ntrees <- 50

# Matriz de tamano N x M inicializada com zeros. Em que N eh o numero
# de exemplos no conjunto de validacao e M eh o numero de arvores que
# teremos no Ensemble. Cada coluna tera os valores preditos por  
# cada arvore no Ensemble. 
valPredictedClasses <- matrix(0, nrow = nrow(dataVal), ncol = ntrees)

for(i in 1:ntrees){
  
  # Toma uma quantidade aleatoria que esta entre 85% e 100% 
  # do numero de exemplos da classe menos frequente
  nsamples <- round(runif(1, min=0.85, max=1.0)*lowest_samples)
  
  # Seleciona, com reposicao (ja que eh Bagging), 
  # os indices da classe negativa
  NoIdx <- sample(1:nrow(dataTrainNo), nsamples, replace = TRUE)
  
  # Seleciona, com reposicao, os indices da classe positiva
  YesIdx <- sample(1:nrow(dataTrainYes), nsamples, replace = TRUE)
  
  # Monta o conjunto de treinamento baseado nos indices tomados
  # anteriormente
  subsetDataTrain <- rbind(dataTrainNo[NoIdx,], dataTrainYes[YesIdx,])
  
  # Treina o i-th modelo do ensemble
  treeModel <- rpart(formula=y ~ ., 
                     data=subsetDataTrain, method="class",
                     control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                     parms= list(split="information"))
  
  # Armazena os resultados para cada arvore.
  valPreds <- predict(treeModel, dataVal)
  
  # Como vamos contar os votos, precisamos transformar as predicoes
  # em numeros. Assim o "valPreds" anterior eh uma matriz N x 2
  # em que N eh o numero de exemplos no conjunto de validacao
  # e 2 eh o numero de classes ("yes" ou "no"). Assim, se a predicao
  # for "no" vamos colocar valor 0, mas se for "yes" vamos colocar 
  # valor 1. A linha abaixo realiza esta operacao.
  valClasses <- argmax(valPreds) - 1
  valPredictedClasses[,i] <- valClasses 
  
}

# Contagem de votos. Por exemplo, se tivermos 5 avores, podemos ter 
# a seguinte predicao: 1 0 0 1 0. A soma resulta em 2. Assim, a proporcao
# eh 2/5 = 0.4. Ja que 0.4 < 0.5, entao a classe mais votada eh zero. 
votes <- rowSums(valPredictedClasses)/ntrees
votes

votes[votes >= 0.5] <- 'yes'
votes[votes < 0.5] <- 'no'

cm <- confusionMatrix(data = as.factor(votes), 
                      reference = as.factor(dataVal$y), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

#### Vamos variar o numero de classificadores no Bagging e ver  ####
####          como a acuracia de validacao eh impactada          ####
accValBagging <- c(ntrees-1) 

for(i in 2:ntrees){
  votes <- rowSums(valPredictedClasses[,1:i])/i
  
  votes[votes >= 0.5] <- 'yes'
  votes[votes < 0.5] <- 'no'
  
  cm <- confusionMatrix(data = as.factor(votes), 
                        reference = as.factor(dataVal$y), 
                        positive='yes')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2

  accValBagging[i-1] <- acc_bal
}

plot(2:ntrees, accValBagging, xlab = "Number of classifiers", 
     ylab = "Balanced Acc Val", col="blue", type="o")


###############
#   PASTING   #
###############
ntrees <- 50

# Matriz de tamano N x M inicializada com zeros. Em que N eh o numero
# de exemplos no conjunto de validacao e M eh o numero de arvores que
# teremos no Ensemble. Cada coluna tera os valores preditos por  
# cada arvore no Ensemble. 
valPredictedClasses <- matrix(0, nrow = nrow(dataVal), ncol = ntrees)

for(i in 1:ntrees){
  
  # Toma uma quantidade aleat?ria que est? entre 85% e 100% 
  # do n?mero de exemplos da classe menos frequente
  nsamples <- round(runif(1, min=0.85, max=1.0)*lowest_samples)
  
  # Seleciona, sem reposicao (ja que eh Pasting), 
  # os indices da classe negativa
  NoIdx <- sample(1:nrow(dataTrainNo), nsamples, replace = FALSE)
  
  # Seleciona, sem reposicao, os indices da classe positiva
  YesIdx <- sample(1:nrow(dataTrainYes), nsamples, replace = FALSE)
  
  # Monta o conjunto de treinamento baseado nos indices tomados
  # anteriormente
  subsetDataTrain <- rbind(dataTrainNo[NoIdx,], dataTrainYes[YesIdx,])
  
  # Treina o i-th modelo do ensemble
  treeModel <- rpart(formula=y ~ ., 
                     data=subsetDataTrain, method="class",
                     control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                     parms= list(split="information"))
  
  # Faz a predicao em probabilidade para as classes
  valPreds <- predict(treeModel, dataVal)
  
  # Como vamos contar os votos, precisamos transformar as predicoes
  # em numeros. Assim o "valPreds" anterior eh uma matriz N x 2
  # em que N eh o numero de exemplos no conjunto de validacao
  # e 2 eh o numero de classes ("yes" ou "no"). Assim, se a predicao
  # for "no" vamos colocar valor 0, mas se for "yes" vamos colocar 
  # valor 1. A linha abaixo realiza esta operacao.
  valClasses <- argmax(valPreds) - 1
  valPredictedClasses[,i] <- valClasses 
  
}

# Contagem de votos. Por exemplo, se tivermos 5 arvores, podemos ter 
# a seguinte predicao: 1 0 0 1 0. A soma resulta em 2. Assim, a proporcao
# eh 2/5 = 0.4. Ja que 0.4 < 0.5, entao a classe mais votada eh zero. 
votes <- rowSums(valPredictedClasses)/ntrees
votes

votes[votes >= 0.5] <- 'yes'
votes[votes < 0.5] <- 'no'

cm <- confusionMatrix(data = as.factor(votes), 
                      reference = as.factor(dataVal$y), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

#### Vamos variar o numero de classificadores no Pasting e ver  ####
####          como a acuracia de validacao eh impactada         ####
accValPasting <- c(ntrees-1)

for(i in 2:ntrees){
  votes <- rowSums(valPredictedClasses[,1:i])/i
  
  votes[votes >= 0.5] <- 'yes'
  votes[votes < 0.5] <- 'no'
  
  cm <- confusionMatrix(data = as.factor(votes), 
                        reference = as.factor(dataVal$y), 
                        positive='yes')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
 
  accValPasting[i-1] <- acc_bal
  
}

plot(2:ntrees, accValPasting, xlab = "Number of classifiers", 
     ylab = "Balanced Acc Val", col="blue", type="o")


##################
#   BOOSTING     #
##################
# Define-se o numero de classificadores no Boosting
ntrees <- 50

# Primeiramente vamos balancear a base de dados. Diferente do Bagging e
# Pasting, o Boosting treina seu conjunto de classificadores sempre em 
# cima da mesma base de dados. O Bagging e o Pasting baseiam-se em 
# amostragens aleatorias o que permite certa robustez ao desbalanceamento.
# O Boosting pode ser severamente impactado se nao houver um estagio
# de balanceamento inicial.
NoIdx <- sample(1:nrow(dataTrainNo), 2*lowest_samples, replace = FALSE)
YesIdx <- sample(1:nrow(dataTrainYes), lowest_samples, replace = FALSE)
balancedDataTrain <- rbind(dataTrainNo[NoIdx,], dataTrainYes[YesIdx,])

table(balancedDataTrain$y)


# Matriz de tamano N x M inicializada com zeros. Em que N eh o numero
# de exemplos no conjunto de validacao e M eh o numero de arvores que
# teremos no Ensemble. Cada coluna tera os valores preditos por  
# cada arvore no Ensemble. 
valPredictedClasses <- matrix(0, nrow = nrow(dataVal), ncol = ntrees)

# Replica o peso 1 igualmente para todos os exemplos de treinamento.
# Em cada iteracao do Boosting, esse valores serao atualizados de acordo
# com o acerto ou com o erro. 
weights <- rep(1/nrow(balancedDataTrain), nrow(balancedDataTrain))

for(i in 1:ntrees){
  
  # Treina o i-th modelo do ensemble
  treeModel <- rpart(formula=y ~ ., weights = weights, 
                     data=balancedDataTrain, method="class",
                     control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                     parms= list(split="information"))
  
  # Armazena os valores preditos no conjunto de validacao
  valPreds <- predict(treeModel, dataVal)
  
  # Como vamos contar os votos, precisamos transformar as predicoes
  # em numeros. Assim o "valPreds" anterior eh uma matriz N x 2
  # em que N eh o numero de exemplos no conjunto de validacao
  # e 2 eh o numero de classes ("yes" ou "no"). Assim, se a predicao
  # for "no" vamos colocar valor 0, mas se for "yes" vamos colocar 
  # valor 1. A linha abaixo realiza esta operacao.
  valClasses <- argmax(valPreds) - 1
  valPredictedClasses[,i] <- valClasses 
  
  trainPreds <- predict(treeModel, balancedDataTrain)
  trainClasses <- argmax(trainPreds) - 1
  
  # Toma-se os indices dos casos de acerto e de erro no conjunto
  # de treinamento. 
  missedCases <- (trainClasses != balancedDataTrain$y)
  correctCases <- (trainClasses == balancedDataTrain$y)
  
  # Atualiza-se os pesos dos exemplos. Casos corretos tem seus pesos
  # decrementados em 10%, casos de erro tem seus pesos aumentados em
  # 10%. 
  weights[correctCases] <- 0.9*weights[correctCases]
  weights[missedCases] <- 1.1*weights[missedCases]
}

# Contagem de votos. Por exemplo, se tivermos 5 avores, podemos ter 
# a seguinte predicao: 1 0 0 1 0. A soma resulta em 2. Assim, a proporcao
# eh 2/5 = 0.4. Ja que 0.4 < 0.5, entao a classe mais votada eh zero. 
votes <- rowSums(valPredictedClasses)/ntrees
votes

votes[votes >= 0.5] <- 'yes'
votes[votes < 0.5] <- 'no'

cm <- confusionMatrix(data = as.factor(votes), 
                      reference = as.factor(dataVal$y), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

#### Vamos variar o numero de classificadores no Pasting e ver  ####
####          como a acuracia de validacao eh impactada         ####
accValBoosting <- c(ntrees-1)

for(i in 2:ntrees){
  votes <- rowSums(valPredictedClasses[,1:i])/i
  
  votes[votes >= 0.5] <- 'yes'
  votes[votes < 0.5] <- 'no'
  
  cm <- confusionMatrix(data = as.factor(votes), 
                        reference = as.factor(dataVal$y), 
                        positive='yes')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2

  accValBoosting[i-1] <- acc_bal
}

plot(2:ntrees, accValBoosting, xlab = "Number of classifiers", 
     ylab = "Balanced Acc Val", col="blue", type="o")


#### Vamos plotar agora as acuracias de validacao das tres tecnicas ####
plot(2:ntrees, accValBagging, xlab = "Number of classifiers", 
     ylab = "Balanced Acc Val", col="blue", type="o", ylim=c(0.61, 0.75))

points(accValPasting, col="red", pch="+")
lines(accValPasting, col="red", lty=2)

points(accValBoosting, col="green", pch="*")
lines(accValBoosting, col="green", lty=2)

legend(33, 0.64, legend=c("Bagging", "Pasting", "Boosting"), 
       col=c("blue","red", "green"), pch=c("o", "+", "*"), 
       cex=0.8, pt.cex = 1)

######################################################################
#   Random Forest modificada considerando balanceamento por arvore   #
######################################################################

getRandomForestResults <- function(ntree, m, trainSet, valSet){
  
  # Seleciona os exemplos das classes positivas e negativas
  dataNeg <- trainSet[trainSet$y == "no",]
  dataPos <- trainSet[trainSet$y == "yes",] 
  
  lowest_samples <- min(dim(dataNeg)[1], dim(dataPos)[1])
  
  print("Numero de elementos na classe negativa:")
  print(dim(dataNeg))
  
  print("Numero de elementos na classe positiva:")
  print(dim(dataPos))
  
  print("Menor desse valores:")
  print(lowest_samples)
  
  # Matriz de tamano N x M inicializada com zeros. Em que N eh o numero
  # de exemplos no conjunto de validacao e M eh o numero de arvores que
  # teremos no Ensemble. Cada coluna tera os valores preditos por  
  # cada arvore no Ensemble. 
  valPredictedClasses <- matrix(0, nrow = nrow(valSet), ncol = ntree)
  
  
  for(i in 1:ntree){
    
    nsamples <- round(runif(1, min=0.85, max=1.0)*lowest_samples)
    
    # Seleciona, com reposicao (ja que o Bagging faz parte da Random Forest), 
    # os indices da classe negativa
    NoIdx <- sample(1:nrow(dataNeg), nsamples, replace = TRUE)
    
    # Seleciona, com reposicao, os indices da classe positiva
    YesIdx <- sample(1:nrow(dataPos), nsamples, replace = TRUE)
    
    # Selecionamos aleatoriamente um subconjunto das features
    # originais (desconsiderando o target). Ja que, cada arvore 
    # na random forest, eh treinada com um subconjunto dos dados
    # tomados com reposicao (duas linha de comando a cima) e um 
    # subconjunto das features.
    featuresIdx <- sample(1:(ncol(trainSet)-1), m, replace = FALSE)
    
    # Como desconsideramos o target anteriormente,
    # temos que adiciona-lo de volta para o modelo treinar
    featuresIdx <- c(featuresIdx, ncol(trainSet)) 
    
    # Cria-se o conjunto de treino baseado na selecao de exemplos
    # e features das linhas anteriores
    subsetDataTrain <- rbind(dataNeg[NoIdx,featuresIdx], 
                             dataPos[YesIdx,featuresIdx])
    
    treeModel <- rpart(formula=y ~ ., 
                       data=subsetDataTrain, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                       parms= list(split="information"))
    
    valPreds <- predict(treeModel, valSet)
    
    # Como vamos contar os votos, precisamos transformar as predicoes
    # em numeros. Assim o "valPreds" anterior eh uma matriz N x 2
    # em que N eh o numero de exemplos no conjunto de validacao
    # e 2 eh o numero de classes ("yes" ou "no"). Assim, se a predicao
    # for "no" vamos colocar valor 0, mas se for "yes" vamos colocar 
    # valor 1. A linha abaixo realiza esta operacao.
    valClasses <- argmax(valPreds) - 1
    valPredictedClasses[,i] <- valClasses 
    
  }
  
  # Contagem de votos. Por exemplo, se tivermos 5 arvores, podemos ter 
  # a seguinte predicao: 1 0 0 1 0. A soma resulta em 2. Assim, a proporcao
  # eh 2/5 = 0.4. Ja que 0.4 < 0.5, entao a classe mais votada eh zero. 
  votes <- rowSums(valPredictedClasses)/ntree
  votes
  
  votes[votes >= 0.5] <- 'yes'
  votes[votes < 0.5] <- 'no'
  
  cm <- confusionMatrix(data = as.factor(votes), 
                        reference = as.factor(valSet$y), 
                        positive='yes')
  
  #### Vamos variar o numero de classificadores na Random Forest   ####
  ####         e como a acuracia de validacao eh impactada         ####
  accValRandomForest <- c(ntree-1)
  
  for(i in 2:ntree){
    votes <- rowSums(valPredictedClasses[,1:i])/i
    
    votes[votes >= 0.5] <- 'yes'
    votes[votes < 0.5] <- 'no'
    
    cm <- confusionMatrix(data = as.factor(votes), 
                          reference = as.factor(valSet$y), 
                          positive='yes')
    
    accValRandomForest[i-1] <- cm$byClass["Balanced Accuracy"]
  }
  
  plot(2:ntree, accValRandomForest, xlab = "Number of classifiers", 
       ylab = "Balanced Acc Val", col="blue", type="o")
  
  return(accValRandomForest)
}

number_of_trees <- 50
# Raiz quadrada do numero de features
m <- sqrt((ncol(dataTrain) - 1))
m
accValRandomForest01 <- getRandomForestResults(number_of_trees, m, dataTrain, dataVal)

# 50% do numero de features
m <- (ncol(dataTrain) - 1)*0.5
m
accValRandomForest02 <- getRandomForestResults(number_of_trees, m, dataTrain, dataVal)

# 75% do numero de features
m <- (ncol(dataTrain) - 1)*0.75
m
accValRandomForest03 <- getRandomForestResults(number_of_trees, m, dataTrain, dataVal)

#### Vamos plotar agora as acuracias de validacao de todas as tecnicas ####
plot(2:ntrees, accValBagging, xlab = "Number of classifiers", 
     ylab = "Balanced Acc Val", col="blue", type="l", lty=1, 
     ylim=c(min(accValBagging, accValPasting, accValBoosting, 
                accValRandomForest01, accValRandomForest02, accValRandomForest03), 
            max(accValBagging, accValPasting, accValBoosting, 
                accValRandomForest01, accValRandomForest02, accValRandomForest03)))

points(accValPasting, col="red", pch=".")
lines(accValPasting, col="red", lty=1)

points(accValBoosting, col="green", pch=".")
lines(accValBoosting, col="green", lty=1)

points(accValRandomForest01, col="magenta", pch=".")
lines(accValRandomForest01, col="magenta", lty=1)

points(accValRandomForest02, col="yellow2", pch=".")
lines(accValRandomForest02, col="yellow2", lty=1)

points(accValRandomForest03, col="aquamarine4", pch=".")
lines(accValRandomForest03, col="aquamarine4", lty=1)

legend(20, 0.65, legend=c("Bagging", "Pasting", "Boosting", 
                          "Random Forest sqrt(features)",
                          "Random Forest 50%",
                          "Random Forest 75%"), 
       col=c("blue","red", "green", "magenta", 
             "yellow2", "aquamarine4"), pch=c("__"), cex=0.7, pt.cex = 1)


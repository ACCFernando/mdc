### ============ Trabalho 03 ============ ###
# Arvores de Descisao e Florestas Aleatorias para Diagnostico de COVID-19
################## Membros ##################
#
# - Leonardo Cesar Silva dos Santos
# - Fernando Augusto Cardoso Candalaft
#
#############################################

# Definindo o diretório de trabalho e setando o seed
setwd("~/workspace/mdc/03_aprendizado_supervisionado_I_inf_0615/test03")
set.seed(42)
options(max.print=150)

## Libs
library(rpart) # To train the Decision Tree model
library(randomForest) # To train the Random Forest model
library(caret) # To use the ConfusionMatrix function
library(ramify)
# -------- To plot the feature importance --------
library(magrittr) # To use the function %>%
library(dplyr)
library(tibble)
library(forcats)
# ------------------------------------------------

## Funcoes auxiliares

get_importances <- function(treeModel) {
  var_imp <- varImp(treeModel)
  var_imp_names <- rownames(var_imp)
  var_imp_vls <- var_imp$Overall 
  names(var_imp_vls) <- var_imp_names
  
  return(var_imp_vls)
}

plot_feature_importance <- function(treeModel) {
  
  fimportance <-  treeModel$variable.importance %>% 
                  data.frame() %>%
                  rownames_to_column(var="Feature") %>%
                  rename(Overall = '.') %>%
                  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
                  geom_pointrange(aes(ymin = 0, ymax = Overall), 
                                  color = "cadetblue", size = .3) +
                  theme_minimal() +
                  coord_flip() +
                  labs(x = "", 
                       y = "", 
                       title=paste("Variable Importance ", 
                                   "- #variables: ", 
                                   length(treeModel$variable.importance)))
  print(fimportance)
  
}

# Calcula a matriz de confusao relativa 
calculaMatrizConfusaoRelativa <- function(cm){
  
  # Aplicamos a transposicao para garantir que a referencia
  # fique nas linhas e a predicao nas colunas
  cm_absolute = t(cm$table)
  
  # SEMPRE construam e reportem a matriz de confusao relativa!
  cm_relative = cm_absolute
  
  # TNR = TN / (TN + FP)
  cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2) # TNR
  
  cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
  cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
  
  # TPR = TP / (TP + FN)
  cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2) # TPR
  
  return(cm_relative)  
}

report_metrics <- function(true_values, pred_values, report_all=TRUE) {
  
  cm <- confusionMatrix(data=as.factor(pred_values), 
                        reference=as.factor(true_values), 
                        positive='POSITIVO')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  
  TNR <- cm_relative[1,1]
  TPR <- cm_relative[2,2]
  acc_bal <- (TNR + TPR) / 2
  
  if (report_all) {
    
    print(paste("True Negative Rate (TNR):", TNR, "| True Positive Rate (TPR):", TPR))
    print(paste("Balanced Accuracy:", acc_bal))
    print("Relative Confusion Matrix:")
    print(cm_relative)
  }
  else {
    # print(paste("Balanced Accuracy:", acc_bal))
    
    return(acc_bal)
  }
  
}

## 00. Lendo a base de dados
covid_df <- read.csv("./covid_analysis_train_val_sets.csv", 
                     header=T,
                     sep=",", 
                     stringsAsFactors=T)
# Lendo os dados de teste
test_df <- read.csv("./covid_analysis_test_set.csv",
                    header=T,
                    sep=",", 
                    stringsAsFactors=T)
head(covid_df)
dim(covid_df) # [1] 4051  101
# Separando o nome das features e da variavel target
features_names <- names(covid_df)[1:100]
target_name <- "Resultado"

# Verificando dados faltantes
any(is.na(covid_df)) # FALSE

# Removendo elementos repetidos antes da divisao treino e validacao
covid_df <- unique(covid_df)
dim(covid_df) # [1] 3376  101

## 01. Separando os dados entre treino e validacao (80/20)
rand_idxs <- sample(1:nrow(covid_df), size=0.8*nrow(covid_df))

train_df <- covid_df[rand_idxs, ]
val_df <- covid_df[-rand_idxs, ]

dim(train_df) # [1] 2700  101 (~80%)
dim(val_df) # [1] 676 101 (~20%)

# Checando se ha interseccao entre os dados de treino e validacao 
merge(train_df, val_df) # <0 rows> (or 0-length row.names): Nao ha interseccao

## 02. Inspecionando os dados de treinamento

# Quantidade de exemplos em cada classe
table(train_df[, target_name])
# NEGATIVO POSITIVO 
# 2142      558
# 79.33%    20.67%

# Lidando com o desbalanceamento entre as classes 
# Como temos aproximadamente 80% dos dados pertencentes a class NEGATIVO e aproximadamente 
# 20% dos dados pertencentes a classe POSITIVO segue entao que os nossos dados estao
# desbalanceados. Para que nosso algoritmo nao de preferencia a uma classe em detrimento 
# de outra vamos realizar um UNDERSAMPLING sobre a classe com mais exemplos (classe NEGATIVO)

train_neg_df <- train_df[train_df[, target_name] == "NEGATIVO", ]
train_pos_df <- train_df[train_df[, target_name] == "POSITIVO", ]

dim(train_neg_df)
dim(train_pos_df)

rand_neg_idxs <- sample(1:nrow(train_neg_df), size=1.6*nrow(train_pos_df))
undersampling_neg_df <- train_neg_df[rand_neg_idxs, ]
dim(undersampling_neg_df) # [1] 892 101

# Novo conjunto de treinamento, com dados um pouco mais balanceados
train_aj_df <- rbind(train_pos_df, undersampling_neg_df)
table(train_aj_df[, target_name])
# NEGATIVO POSITIVO 
# 892      558 

## 03. Treinando uma Arvore de Decisao como modelo baseline

# Para o baseline vamos usar todas as features disponiveis
prepare_formula <- function(features, target) {
  formula_str <- paste(target, "~", paste(features, collapse=" + "))
  hypothesis <- as.formula(formula_str)
  
  return(hypothesis)
}

length(features_names)
hypothesis <- prepare_formula(features_names, target_name)

# Checando se a formula esta correta
unique(unlist(strsplit(gsub("~|\\+", "", hypothesis), " "))) # "\n", "Resultado", ""
length(unique(unlist(strsplit(gsub("~|\\+", "", hypothesis), " ")))) 

baseline <- rpart(
                    formula=hypothesis, 
                    data=train_aj_df, 
                    method="class",
                    control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                    parms= list(split="information")
                    )
help("rpart")
# Verificando a importancia de cada feature 
# (Podemos usar essa informacao para reduzir o numero de features em outros modelos a serem
# testados futuramente)
plot_feature_importance(baseline)

imp_test <- get_importances(baseline)
length(imp_test[imp_test != 0])  

importance_per_feature <- baseline$variable.importance
relative_importances <- importance_per_feature / sum(importance_per_feature)
length(relative_importances)

# Selecionando as Top Features
sorted_importances <- sort(relative_importances, decreasing=TRUE)

topN_lst <- list()
for (i in seq(1, length(relative_importances), by=1)) {
  topN <- head(sorted_importances, i)
  topN_lst[[paste0("top", i)]] <- names(topN)
}

# ----------------------------------------------------
relative_importances[topN_lst$top10] # Top10 features
sum(relative_importances[topN_lst$top10]) # 0.3057876

relative_importances[topN_lst$top20] # Top20 features
sum(relative_importances[topN_lst$top20]) # 0.486551

sum(relative_importances[topN_lst$top70]) # 0.9618063

sum(relative_importances[topN_lst$top80]) # 0.9944249

# ----------------------------------------------------

# Avaliando o modelo sobre os dados de treinamento, validacao e teste
train_pred <- predict(baseline, train_aj_df, type="class")
val_pred <- predict(baseline, val_df, type="class")
test_pred <- predict(baseline, test_df, type="class")

length(train_pred)
dim(train_aj_df)

report_metrics(train_aj_df[, target_name], train_pred)
# [1] "True Negative Rate (TNR): 0.99 | True Positive Rate (TPR): 0.99"
# [1] "Balanced Accuracy: 0.99"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.99     0.01
# POSITIVO     0.01     0.99
report_metrics(val_df[, target_name], val_pred)
# [1] "True Negative Rate (TNR): 0.71 | True Positive Rate (TPR): 0.53"
# [1] "Balanced Accuracy: 0.62"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.71     0.29
# POSITIVO     0.47     0.53
report_metrics(test_df[, target_name], test_pred)
# [1] "True Negative Rate (TNR): 0.72 | True Positive Rate (TPR): 0.49"
# [1] "Balanced Accuracy: 0.605"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.72     0.28
# POSITIVO     0.51     0.49

## 04. Avaliando o efeito da profundidade da arvore sobre a performance do modelo

i <- 1
number_of_depths <- 30
search_len <- length(number_of_depths)
balanced_acc_df <- data.frame(depth=numeric(search_len),
                              trainBalancedAcc=numeric(search_len), 
                              valBalancedAcc=numeric(search_len))
for (max_depth in 1:number_of_depths) {
  
  tree_model <- rpart(
                      formula=hypothesis, 
                      data=train_aj_df, 
                      method="class",
                      control=rpart.control(minsplit=2, 
                                            cp=0.0, 
                                            xval=10,
                                            maxdepth=max_depth),
                      parms= list(split="information")
                    )
  
  training_preds <- predict(tree_model, train_aj_df, type="class")
  validation_preds <- predict(tree_model, val_df, type="class")
  
  balanced_accuracy_train <- report_metrics(train_aj_df[, target_name], 
                                            training_preds, report_all=FALSE)
  
  balanced_accuracy_val <- report_metrics(val_df[, target_name], 
                                          validation_preds, report_all=FALSE)
  
  print(paste(max_depth, 
              balanced_accuracy_train, 
              balanced_accuracy_val)
        )
  balanced_acc_df[i, ] <- c(max_depth, balanced_accuracy_train, balanced_accuracy_val)
  
  i <- i + 1
}

grafico <- ggplot(balanced_acc_df, aes(x = depth)) +
  geom_line(aes(y = trainBalancedAcc, color = "Train Balanced Acc")) +
  geom_line(aes(y = valBalancedAcc, color = "Val Balanced Acc")) +
  labs(title = "Balanced Acc by Depth",
       x = "Max Depth",
       y = "Balanced Accuracy") +
  scale_color_manual(values = c("Train Balanced Acc" = "blue", 
                                "Val Balanced Acc" = "red")) +
  scale_x_continuous(breaks = 1:30) +
  theme_minimal()
print(grafico)
# Visualmente temos uma regiao de VALOR OTIMO para uma profundidade igual a 1 ou 2. 
# Para uma profundidade maior ou igual a 3 o nosso modelo comeca a entrar em estado de OVERFITTING

# Retornando a melhor acuracia balanceada sobre os dados de validacao
balanced_acc_df$diff <- balanced_acc_df$trainBalancedAcc - balanced_acc_df$valBalancedAcc
balanced_acc_df[balanced_acc_df$valBalancedAcc == max(balanced_acc_df[, "valBalancedAcc"]), ]

# Seguindo o principio da Navalha de Occam, vamos escolher o modelo com 
# MAXDEPTH = 1, dado que possui a mesma acuracia balanceada quando a profundidade eh
# igual a 2, porem eh o modelo mais simples

# Melhor modelo na visao profundidade: MAXDEPTH = 1
best_depth_based_model <- rpart(
                                formula=hypothesis, 
                                data=train_aj_df, 
                                method="class",
                                control=rpart.control(minsplit=2, 
                                                      cp=0.0, 
                                                      xval=10,
                                                      maxdepth=1),
                                parms= list(split="information")
                              )

test_pred <- predict(best_depth_based_model, test_df, type="class")
report_metrics(test_df[, target_name], test_pred)
# [1] "True Negative Rate (TNR): 0.8 | True Positive Rate (TPR): 0.61"
# [1] "Balanced Accuracy: 0.705"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.80     0.20
# POSITIVO     0.39     0.61

# Em termos de acuracia balanceada, este nosso modelo performa melhor que o modelo 
# baseline. Temos que o nosso valor de TPR tambem eh melhor

## 05. Teste de selecao de features (Features Selection) 

# Verificando a importancia de cada feature com base no modelo baseline
length(topN_lst)
topN_lst$top5
topN_lst$top85[80:85]

# Selecionando features baseado em feature importance

ft01 <- topN_lst$top5
ft02 <- c(ft01, topN_lst$top85[80:85])

ft01
ft02

# Testando os modelos baseados nas features importance

# Top 5
fimp <- prepare_formula(ft01, target_name); fimp
fimp_model <- rpart(
                    formula=fimp, 
                    data=train_aj_df, 
                    method="class",
                    control=rpart.control(minsplit=2, 
                                          cp=0.0, 
                                          xval=10,
                                          maxdepth=1),
                    parms= list(split="information")
                    )
val_pred <- predict(fimp_model, val_df, type="class")
report_metrics(val_df[, target_name], val_pred)
# [1] "True Negative Rate (TNR): 0.78 | True Positive Rate (TPR): 0.65"
# [1] "Balanced Accuracy: 0.715"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.78     0.22
# POSITIVO     0.35     0.65

# Top 5 + ultimo 5
fimp <- prepare_formula(ft02, target_name); fimp
fimp_model <- rpart(
  formula=fimp, 
  data=train_aj_df, 
  method="class",
  control=rpart.control(minsplit=2, 
                        cp=0.0, 
                        xval=10,
                        maxdepth=1),
  parms= list(split="information")
)
val_pred <- predict(fimp_model, val_df, type="class")
report_metrics(val_df[, target_name], val_pred)
# [1] "True Negative Rate (TNR): 0.78 | True Positive Rate (TPR): 0.65"
# [1] "Balanced Accuracy: 0.715"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.78     0.22
# POSITIVO     0.35     0.65

# Vamos selecionar o modelo com 5 features como sendo o melhor, seguindo o principio da 
# Navalha de Occam
fimp <- prepare_formula(ft01, target_name)
fimp_model <- rpart(
  formula=fimp, 
  data=train_aj_df, 
  method="class",
  control=rpart.control(minsplit=2, 
                        cp=0.0, 
                        xval=10,
                        maxdepth=1),
  parms= list(split="information")
)

test_pred <- predict(fimp_model, test_df, type="class")
report_metrics(test_df[, target_name], test_pred)
# [1] "True Negative Rate (TNR): 0.8 | True Positive Rate (TPR): 0.61"
# [1] "Balanced Accuracy: 0.705"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.80     0.20
# POSITIVO     0.39     0.61

## 06. Avaliando Florestas Aleatorias

# Teste inicial
rf_model <- randomForest(
                        formula=hypothesis, 
                        data=train_aj_df, 
                        ntree=100, 
                        mtry=6)

# https://en.wikipedia.org/wiki/Out-of-bag_error
# https://www.analyticsvidhya.com/blog/2020/12/out-of-bag-oob-score-in-the-random-forest-algorithm/
layout(matrix(c(1,2),nrow=1), width=c(5,1)) 
par(mar=c(5,4,4,0)) # Sem margem no lado direito
plot(rf_model, log="y", main="Random Forest OOB")
par(mar=c(5,0,4,2)) # Sem margem do lado esquerdo
legend("bottomleft", colnames(rf_model$err.rate),
       fill=1:ncol(rf_model$err.rate),
       xpd=TRUE,
       bty = "n")

# Treinando Florestas com diferentes numero de arvores

i <- 1
n_trees <- 1:100
search_len <- length(n_trees)
balanced_acc_df <- data.frame(n_trees=numeric(search_len),
                              trainBalancedAcc=numeric(search_len), 
                              valBalancedAcc=numeric(search_len))
for (nt in n_trees) {
  
  rf_model <- randomForest(
                            formula=hypothesis, 
                            data=train_aj_df, 
                            ntree=nt, 
                            mtry=6)
  
  training_preds <- predict(rf_model, train_aj_df, type="class")
  validation_preds <- predict(rf_model, val_df, type="class")
  
  balanced_accuracy_train <- report_metrics(train_aj_df[, target_name], 
                                            training_preds, report_all=FALSE)
  
  balanced_accuracy_val <- report_metrics(val_df[, target_name], 
                                          validation_preds, report_all=FALSE)
  
  print(paste(nt, 
              balanced_accuracy_train, 
              balanced_accuracy_val)
  )
  balanced_acc_df[i, ] <- c(nt, balanced_accuracy_train, balanced_accuracy_val)
  
  i <- i + 1
}
head(balanced_acc_df)

grafico <- ggplot(balanced_acc_df, aes(x = n_trees)) +
  geom_line(aes(y = trainBalancedAcc, color = "Train Balanced Acc")) +
  geom_line(aes(y = valBalancedAcc, color = "Val Balanced Acc")) +
  labs(title = "Balanced Acc by #Trees",
       x = "Number of Trees",
       y = "Balanced Accuracy") +
  scale_color_manual(values = c("Train Balanced Acc" = "blue", 
                                "Val Balanced Acc" = "red")) +
  scale_x_continuous(breaks = 1:100) +
  theme_minimal()
print(grafico)
# Para um numero de arvores na floresta entre 1 ou 2 temos um caso de UNDERFITTING, dado que nosso
# modelo performa razoavelmente mal tanto em treino quanto em teste. Nunca atingimos um valor otimo
# de fato, porem ha situacoes em que o modelo obtem um valor de razoavel de 
# acuracia balanceada sobre o conjunto de validacao

# Retornando a melhor acuracia balanceada sobre os dados de validacao
balanced_acc_df$diff <- balanced_acc_df$trainBalancedAcc - balanced_acc_df$valBalancedAcc
balanced_acc_df[balanced_acc_df$valBalancedAcc == max(balanced_acc_df[, "valBalancedAcc"]), ]
# n_trees trainBalancedAcc valBalancedAcc  diff
# 56      56             0.99          0.745 0.245

# Treinando a melhor Floresta obtida
best_rf_model <- randomForest(
                              formula=hypothesis, 
                              data=train_aj_df, 
                              ntree=56, 
                              mtry=6)
test_pred <- predict(best_rf_model, test_df, type="class")
report_metrics(test_df[, target_name], test_pred)
# [1] "True Negative Rate (TNR): 0.91 | True Positive Rate (TPR): 0.48"
# [1] "Balanced Accuracy: 0.695"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.91     0.09
# POSITIVO     0.52     0.48
# https://stats.stackexchange.com/questions/242833/is-random-forest-a-good-option-for-unbalanced-data-classification

######################## EXTRA: Random Forest com balanceamento ######################## 
# dev.off()
set.seed(42)
getRandomForestResults <- function(ntree, m, trainSet, valSet, target, POS_class, NEG_class){
  
  # Seleciona os exemplos das classes positivas e negativas
  dataNeg <- trainSet[trainSet[, target] == NEG_class, ]
  dataPos <- trainSet[trainSet[, target] == POS_class, ] 
  
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
    # os indices da classe negativa e da classe positiva
    NoIdx <- sample(1:nrow(dataNeg), nsamples, replace=TRUE)
    YesIdx <- sample(1:nrow(dataPos), nsamples, replace=TRUE)
    
    # Selecionamos aleatoriamente um subconjunto das features
    # originais (desconsiderando o target). Ja que, cada arvore 
    # na random forest, eh treinada com um subconjunto dos dados
    # tomados com reposicao (duas linha de comando a cima) e um 
    # subconjunto das features.
    # (Assum que o target esta na ultima coluna do dataframe)
    featuresIdx <- sample(1:(ncol(trainSet)-1), m, replace=FALSE)
    
    # Como desconsideramos o target anteriormente,
    # temos que adiciona-lo de volta para o modelo treinar
    features_used <- names(dataNeg[, featuresIdx])
    
    featuresIdx <- c(featuresIdx, ncol(trainSet)) 

    # Cria-se o conjunto de treino baseado na selecao de exemplos
    # e features das linhas anteriores
    subsetDataTrain <- rbind(dataNeg[NoIdx, featuresIdx], 
                             dataPos[YesIdx, featuresIdx])
    
    # print(paste0("Iter ", i))
    # print("Train set dimension:")
    # print(dim(subsetDataTrain))
    # print("# classes:")
    # print(table(subsetDataTrain[, target]))
    # print(" ")
    
    form <- prepare_formula(features_used, target)
    treeModel <- rpart(formula=form, 
                       data=subsetDataTrain, 
                       method="class",
                       control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                       parms= list(split="information"))
    
    valPreds <- predict(treeModel, valSet, type="class")
    
    # Como vamos contar os votos, precisamos transformar as predicoes
    # em numeros. Assim o "valPreds" anterior eh uma matriz N x 2
    # em que N eh o numero de exemplos no conjunto de validacao
    # e 2 eh o numero de classes ("yes" ou "no"). Assim, se a predicao
    # for "no" vamos colocar valor 0, mas se for "yes" vamos colocar 
    # valor 1. A linha abaixo realiza esta operacao.
    valClasses <- ifelse(valPreds == POS_class, 1, 0)
    valPredictedClasses[,i] <- valClasses 
    
  }
  
  # Contagem de votos. Por exemplo, se tivermos 5 arvores, podemos ter 
  # a seguinte predicao: 1 0 0 1 0. A soma resulta em 2. Assim, a proporcao
  # eh 2/5 = 0.4. Ja que 0.4 < 0.5, entao a classe mais votada eh zero. 
  votes <- rowSums(valPredictedClasses)/ntree
  votes
  
  votes[votes >= 0.5] <- POS_class
  votes[votes < 0.5] <- NEG_class
  
  print("-----------------------------")
  report_metrics(valSet[, target], votes)
  print("-----------------------------")
  
  #### Vamos variar o numero de classificadores na Random Forest   ####
  ####         e como a acuracia de validacao eh impactada         ####
  accValRandomForest <- c(ntree-1)
  
  for(i in 2:ntree){
    votes <- rowSums(valPredictedClasses[,1:i])/i
    
    votes[votes >= 0.5] <- POS_class
    votes[votes < 0.5] <- NEG_class
    
    cm <- confusionMatrix(data = as.factor(votes), 
                          reference = as.factor(valSet[, target]), 
                          positive=POS_class)
    
    accValRandomForest[i-1] <- cm$byClass["Balanced Accuracy"]
  }
  
  df <- data.frame(Classifiers=2:ntree, 
                   Balanced_Acc_Val=accValRandomForest)
  
  dev.off()
  figplot <- ggplot(df, aes(x=Classifiers, y=Balanced_Acc_Val)) +
                  geom_line(color="blue") +
                  labs(x = "Number of classifiers", 
                       y = "Balanced Acc Val",
                       title = "Balanced Acc by # Classifiers") +
                  theme_minimal()
  print(figplot)
  
  print("Best number of classifiers:")
  print(df[df$Balanced_Acc_Val == max(df[, "Balanced_Acc_Val"]), ])
  
  return(accValRandomForest)
}


number_of_trees <- 250
# Raiz quadrada do numero de features
m <- sqrt((ncol(train_df) - 1))
m # 10
accValRandomForest01 <- getRandomForestResults(number_of_trees, m, train_df, val_df, target_name, "POSITIVO", "NEGATIVO")
# [1] "Numero de elementos na classe negativa:"
# [1] 2142  101
# [1] "Numero de elementos na classe positiva:"
# [1] 558 101
# [1] "Menor desse valores:"
# [1] 558
# [1] "-----------------------------"
# [1] "True Negative Rate (TNR): 0.9 | True Positive Rate (TPR): 0.52"
# [1] "Balanced Accuracy: 0.71"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.90     0.10
# POSITIVO     0.48     0.52
# [1] "-----------------------------"
# [1] "Best number of classifiers:"
# Classifiers Balanced_Acc_Val
# 135         136        0.7222872

# 50% do numero de features
m <- (ncol(train_df) - 1)*0.5
m # 50
accValRandomForest02 <- getRandomForestResults(number_of_trees, m, train_df, val_df, target_name, "POSITIVO", "NEGATIVO")
# [1] "Numero de elementos na classe negativa:"
# [1] 2142  101
# [1] "Numero de elementos na classe positiva:"
# [1] 558 101
# [1] "Menor desse valores:"
# [1] 558
# [1] "-----------------------------"
# [1] "True Negative Rate (TNR): 0.85 | True Positive Rate (TPR): 0.65"
# [1] "Balanced Accuracy: 0.75" **
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.85     0.15
# POSITIVO     0.35     0.65
# [1] "-----------------------------"
# [1] "Best number of classifiers:"
# Classifiers Balanced_Acc_Val
# 27          28        0.7740363

# 75% do numero de features
m <- (ncol(train_df) - 1)*0.75
m # 75
accValRandomForest03 <- getRandomForestResults(number_of_trees, m, train_df, val_df, target_name, "POSITIVO", "NEGATIVO")
# [1] "Numero de elementos na classe negativa:"
# [1] 2142  101
# [1] "Numero de elementos na classe positiva:"
# [1] 558 101
# [1] "Menor desse valores:"
# [1] 558
# [1] "-----------------------------"
# [1] "True Negative Rate (TNR): 0.82 | True Positive Rate (TPR): 0.67"
# [1] "Balanced Accuracy: 0.745"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.82     0.18
# POSITIVO     0.33     0.67
# [1] "-----------------------------"
# [1] "Best number of classifiers:"
# Classifiers Balanced_Acc_Val
# 20          21        0.7674911

# Examinando os modelos
length(2:number_of_trees)
length(accValRandomForest01)
length(accValRandomForest02)
length(accValRandomForest03)
rm_perf_df <- data.frame(classifiers=2:number_of_trees,
                         accValRandomForest01=accValRandomForest01,
                         accValRandomForest02=accValRandomForest02,
                         accValRandomForest03=accValRandomForest03)

grafico <- ggplot(rm_perf_df, aes(x = classifiers)) +
  geom_line(aes(y = accValRandomForest01, color = "RandomForest01")) +
  geom_line(aes(y = accValRandomForest02, color = "RandomForest02")) +
  geom_line(aes(y = accValRandomForest03, color = "RandomForest03")) +
  labs(title = "Balanced Acc by # classifiers",
       x = "Number of classifiers",
       y = "Balanced Accuracy") +
  scale_color_manual(values = c("RandomForest01" = "blue", 
                                "RandomForest02" = "black",
                                "RandomForest03" = "red")) +
  theme_minimal()
print(grafico)

# O melhor foi o que usou 27 arvores e 50% das features
m <- (ncol(train_df) - 1)*0.5
m # 50
accValRandomForest02 <- getRandomForestResults(27, m, train_df, test_df, target_name, "POSITIVO", "NEGATIVO")
# [1] "True Negative Rate (TNR): 0.85 | True Positive Rate (TPR): 0.54"
# [1] "Balanced Accuracy: 0.695"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference  NEGATIVO POSITIVO
# NEGATIVO     0.85     0.15
# POSITIVO     0.46     0.54













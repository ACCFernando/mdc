### ============ Trabalho 02 ============ ###
# Predicao de producao de anticorpos para desenvolvimento de vacinas
################## Membros ##################
#
# - Leonardo Cesar Silva dos Santos
# - Fernando Augusto Cardoso Candalaft
#
#############################################

# Definindo o diretório de trabalho
setwd("C:\\Users\\ferna\\Documents\\rep\\mdc\\03.aprendizado_supervisionado\\03.arvores\\trabalho")


## Libs
library(glmnet)
library(caret)
library(ggplot2)
library(reshape2)

# Setando uma semente para garantir a reprodutibilidade do estudo
set.seed(13)

## 00. Carregando os dados de treinamento e validacao
train_df <- read.csv("./proteins_training_set.csv", stringsAsFactors=TRUE)
val_df <- read.csv("./proteins_validation_set.csv", stringsAsFactors=TRUE)

## 01. Inspecionando os dados
dim(train_df) # [1] 9204   11
dim(val_df) # [1] 2303   11

head(train_df) # Claramente temos features com diferentes escalas

# Checando se ha dados sem anotacoes ou dados duplicados
# Treino
summary(train_df) # Nao ha dados faltantes
dim( unique(train_df) ) # Ha dados duplicados (4, no caso)
train_df <- unique(train_df); dim(train_df) # [1] 9200   11

# Validacao
summary(val_df) # Nao ha dados faltantes
dim( unique(val_df) ) # Nao ha dados duplicados

# No caso de existencia de dados faltantes, o que fariamos? Existem alguns metodos que podem ser utilizados 
# para resolver o problema de dados faltantes. Uma alternativa seria descartar os dados faltantes se a
# quantidade dos mesmos nao fosse relevante com relacao ao numero total de amostras do conjunto de treinamento
# Outra possibilidade seria substituir pelos vizinho mais proximo. 
# (As metodologias que podem ser utilizadas sao diversas e dependem do contexto do problema e se fosse o 
# nosso caso, uma avaliacao mais criteriosa sobre qual metodo utilizar)

# Checando se ha intersecao entre os dados de treino e validacao
# (isso evita vazamento (leakage) de informacao do treino para a validacao)
merge(train_df, val_df) # Nao ha dados em comum

## 02. Inspecionando a frequencia das classes no conjunto de treinamento

names(train_df)
names(val_df)

# Definindo as features para que nao usemos a variavel target "sem querer"
features_names <- names(train_df)[1:10]
target_name <- "target"
head(train_df[, features_names])

train_df[, target_name] <- as.factor(train_df[, target_name])
val_df[, target_name] <- as.factor(val_df[, target_name])

# Contando a quantidade de classes dos dados de treino e teste
## Target = 0 para representar que a cadeia proteica não estimula a produção de anticorpos
## Target = 1 para representar que a cadeia proteica estimula a produção de anticorpos
table(train_df[, target_name]) 
# 0    1 
# 6708 2492 
table(val_df[, target_name])
# 0    1 
# 1678  625 

# Claramente a quantidade de classes 0 e 1 sao diferentes (HA DESBALANCEAMENTO), 
# No caso do conjunto de treinamento temos ~73% sendo da classe 0 e ~27% sendo da classe 1

# Como temos que nossos dados de treinamento (e consequentemente tambem de validacao) estao 
# DESBALANCEADOS podemos usar algumas metodologias para nos ajudar a obter uma boa perfomance para o nosso
# modelo. No caso, algumas vistas em aula sao: Oversampling da classe menos frequente, Undersampling da 
# classe menos frequente, SMOTE para geracao de dados sinteticos da classe menos frequente.
# Escolhi adicionar pesos à função de custo para cada classe de acordo com sua frequencia

## 03. Normalizando os dados para treinamento, com Normalização Gaussiana
normalize_features <- function(features, 
                               training_df=NULL, 
                               not_training_df=NULL, 
                               training_means=NULL, 
                               training_stds=NULL) {
  
  if (is.null(training_df) == FALSE) {
    # Get means and stds and normalize the training dataframe
    mean_features <- apply(training_df[, features], 2, mean)
    sd_features <- apply(training_df[, features], 2, sd)
    
    normalized_train_df <- training_df
    normalized_train_df[, features] <- sweep(normalized_train_df[, features], 2, mean_features, "-")
    normalized_train_df[, features] <- sweep(normalized_train_df[, features], 2, sd_features, "/")
    
    returned_vars <- list(train_df = normalized_train_df, 
                          means = mean_features, 
                          stds = sd_features)
    
    return(returned_vars)
    
  }
    
  else if ( !any(is.null(c(not_training_df, training_means, training_stds))) ) {
    # Applying the normalization to the data which is for training
    
    means_training <- unlist(training_means)
    names(means_training) <- features
    stds_training <- unlist(training_stds)
    names(stds_training) <- features
    
    normalized_not_train_df <- not_training_df
    normalized_not_train_df[, features] <- sweep(normalized_not_train_df[, features], 2, means_training, "-")
    normalized_not_train_df[, features] <- sweep(normalized_not_train_df[, features], 2, stds_training, "/")
    
    return(normalized_not_train_df)
  } 
  
  else {
    print("Something went wrong!!!")
  }
    
}

# Normalize the training dataframe
norm_vars <- normalize_features(features = features_names, training_df = train_df)

normalized_train_df <- as.data.frame(norm_vars["train_df"])
means_train <- norm_vars["means"]
stds_train <- norm_vars["stds"]

names(normalized_train_df) <- c(features_names, target_name)
head(normalized_train_df)

# Normalize the validation dataframe
normalized_val_df <- normalize_features(features = features_names,
                                        not_training_df = val_df, 
                                        training_means = means_train, 
                                        training_stds = stds_train)

head(normalized_val_df)

## 04. Treinando e avaliando o modelo baseline

# Aux functions
getHypothesis <- function(feature_names, degree){
  
  hypothesis_string <- "hypothesis <- formula(target ~ "
  for(d in 1:degree){
    for(i in 1:length(feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 "I(", feature_names[i], "^", d, ") + ",
                                 sep = "")
    }
  }
  hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
}

write_hypotesis_from_string <- function(features){
  
  hypothesis_string <- "hypothesis <- formula(target ~ "
  for(feat in features){
    hypothesis_string <- paste(hypothesis_string, feat, " + ", sep = "")
  }
  hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
  
}

# Iremos usar pesos para lidar com o desbalanceamento 
# das  classes e avaliar todos os modelos com relacao a isso

####### Balanceamento por ponderacao da funcao de erro #######
classes_frequency = table(normalized_train_df[target_name])
classes_frequency

relative_classes_frequency = classes_frequency / sum(classes_frequency)
relative_classes_frequency

#Peso inversamente proporcional à frequência da classe
w_positive = 1 - relative_classes_frequency[2]
w_negative = 1 - relative_classes_frequency[1]

w_positive
w_negative

weights <- rep(0.0, dim(normalized_train_df)[1])
weights[normalized_train_df[target_name] == 1] = w_positive 
weights[normalized_train_df[target_name] == 0] = w_negative 
length(weights)

hypothesis <- getHypothesis(features_names, degree=1); hypothesis

x_train <- model.matrix(hypothesis, normalized_train_df)
y_train <- normalized_train_df[, target_name]

any(is.na(x_train))
any(is.na(y_train))

summary(x_train)

head(x_train)
table(y_train)

# Training the baseline model
## Logistic regression without regularization
logreg_baseline_model <- glmnet(x_train, y_train,  family="binomial",
                                 weights = weights,
                                 standardize = FALSE, alpha=0, lambda = 1e-6)
logreg_baseline_model

# Evaluating the baseline model on the validation set
evaluate_model <- function(hyp, model, validation_set, target) {
  
  x_val <- model.matrix(hyp, validation_set)
  y_val <- validation_set[, target]
  valPred <- predict(model, newx = x_val, type="response")
  
  # converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  return(valClassPred)
}

train_pred <- evaluate_model(hypothesis, logreg_baseline_model,
                             normalized_train_df, 
                             target_name)

val_pred <- evaluate_model(hypothesis, logreg_baseline_model,
                           normalized_val_df, 
                           target_name)


## Model evaluation functions

# Calcula a loss de um modelo dado os valores preditos e os labels
# O conceito usadao para a função de custo foi entropia
getLoss <- function(y_true, y_pred){
  y_true <- as.numeric(y_true) - 1
  
  totalLoss <- 0
  eps <- 1e-9
  # Recall: length(y_true) == length(y_pred)
  # loss = (1-y)*log2(1 - p + eps)) + y*log(p + eps)
  # eps is used for numerical stability, it is very close to 0.
  # Supose we have y = 1 and p = 1 (perfect prediction), the loss (without eps)
  # would be 0*log2(0) + 1*log(1). It would result in NaN
  # because of 0*log2(0). With eps: 0*log2(1e-9) + 1*log(1 + 1e-9) 
  for(i in 1:length(y_true)){
    loss <- -1*((1 - y_true[i])*log2(1 - y_pred[i] + eps) + y_true[i]*log2(y_pred[i] + eps))
    totalLoss <- totalLoss + loss
  }
  totalLoss <- totalLoss/(length(y_true))
  return(totalLoss)
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
                        positive='1')
  
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

# Metrics report on
# Training set
report_metrics(normalized_train_df[, target_name], train_pred)
# [1] "True Negative Rate (TNR): 0.6 | True Positive Rate (TPR): 0.65"
# [1] "Balanced Accuracy: 0.625"
# [1] "Relative Confusion Matrix:"
#          Prediction
# Reference    0    1
# 0         0.60 0.40
# 1         0.35 0.65

# Validation set
report_metrics(normalized_val_df[, target_name], val_pred)
# [1] "True Negative Rate (TNR): 0.59 | True Positive Rate (TPR): 0.65"
# [1] "Balanced Accuracy: 0.62"
# [1] "Relative Confusion Matrix:"
#          Prediction
# Reference    0    1
# 0         0.59 0.41
# 1         0.35 0.65

#Observa-se pouca perda de performance para TNR e nenhuam perda de performance 
# para o TPR entre treino e validação 


## 05. Implementacao de solucoes alternativas

# Teste de modelos polinomiais
i <- 1
d_searchs <- 2:15
search_len <- length(d_searchs)
balanced_acc_df <- data.frame(polyDegree=numeric(search_len),
                              trainBalancedAcc=numeric(search_len), 
                              valBalancedAcc=numeric(search_len))
for (d in d_searchs) {

  h <- getHypothesis(features_names, degree=d)
  
  x_train <- model.matrix(h, normalized_train_df)
  y_train <- normalized_train_df[, target_name]
  
  # Training the polynomial model with weights and without regularization
  poly_logreg_model <- glmnet(x_train, y_train, family="binomial",
                              weights = weights,
                              standardize = FALSE, alpha=0, lambda = 1e-6)
  
  training_preds <- evaluate_model(h, poly_logreg_model,
                                   normalized_train_df, 
                                   target_name)
  
  validation_preds <- evaluate_model(h, poly_logreg_model,
                                     normalized_val_df, 
                                     target_name)
  
  balanced_accuracy_train <- report_metrics(normalized_train_df[, target_name], 
                                            training_preds, report_all = FALSE)
  
  balanced_accuracy_val <- report_metrics(normalized_val_df[, target_name], 
                                      validation_preds, report_all = FALSE)
  
  print(paste(d, balanced_accuracy_train, balanced_accuracy_val))
  balanced_acc_df[i, ] <- c(d, balanced_accuracy_train, balanced_accuracy_val)
  
  i <- i + 1
}

grafico <- ggplot(balanced_acc_df, aes(x = polyDegree)) +
  geom_line(aes(y = trainBalancedAcc, color = "Train Balanced Acc")) +
  geom_line(aes(y = valBalancedAcc, color = "Val Balanced Acc")) +
  labs(title = "Balanced Acc by Polynomial degree",
       x = "Polynomial degree",
       y = "Balanced Accuracy") +
  scale_color_manual(values = c("Train Balanced Acc" = "blue", "Val Balanced Acc" = "red")) +
  theme_minimal()
print(grafico)

# Retornando a melhor acuracia balanceada sobre os dados de validacao
balanced_acc_df[balanced_acc_df$valBalancedAcc == max(balanced_acc_df[, "valBalancedAcc"]), ]
# polyDegree trainBalancedAcc valBalancedAcc
# 8           9            0.690          0.695
# 12         13            0.690          0.695
# 13         14            0.690          0.695
# 14         15            0.685          0.695
# Como tivemos alguns valores com acc balanceada iguais, selecionamos o mais simples, ie,
# polinomio de grau 9

# Training the best polynomial model
h <- getHypothesis(features_names, degree=9)

x_train <- model.matrix(h, normalized_train_df)
y_train <- normalized_train_df[, target_name]

poly_logreg_model <- glmnet(x_train, y_train, family="binomial",
                            weights = weights,
                            standardize = FALSE, alpha=0, lambda = 1e-6)

training_preds <- evaluate_model(h, poly_logreg_model,
                                 normalized_train_df, 
                                 target_name)

validation_preds <- evaluate_model(h, poly_logreg_model,
                                   normalized_val_df, 
                                   target_name)

report_metrics(normalized_train_df[, target_name], training_preds)
# [1] "True Negative Rate (TNR): 0.67 | True Positive Rate (TPR): 0.71"
# [1] "Balanced Accuracy: 0.69"
# [1] "Relative Confusion Matrix:"
#           Prediction
# Reference    0    1
# 0           0.67 0.33
# 1           0.29 0.71

report_metrics(normalized_val_df[, target_name], validation_preds)
# [1] "True Negative Rate (TNR): 0.68 | True Positive Rate (TPR): 0.71"
# [1] "Balanced Accuracy: 0.695"
# [1] "Relative Confusion Matrix:"
#           Prediction
# Reference    0    1
# 0           0.68 0.32
# 1           0.29 0.71
# O modelo nao parece estar beneficiando nenhuma classe em detrimento da outra


# Teste de combinacao de features
## Teste 01
comb <- as.formula("target ~ 
                        start_position * end_position +
                        emini / stability +
                        chou_fasman - hydrophobicity +
                        start_position:end_position +
                        exp(chou_fasman) +
                        chou_fasman * hydrophobicity +
                        emini / parker +
                        start_position - end_position")

x_train <- model.matrix(comb, normalized_train_df)
y_train <- normalized_train_df[, target_name]

comb_logreg_model <- glmnet(x_train, y_train, family="binomial",
                            weights = weights,
                            standardize = FALSE, alpha=0, lambda = 1e-6)

training_preds <- evaluate_model(comb, comb_logreg_model,
                                 normalized_train_df, 
                                 target_name)

validation_preds <- evaluate_model(comb, comb_logreg_model,
                                   normalized_val_df, 
                                   target_name)

report_metrics(normalized_train_df[, target_name], training_preds)
# [1] "True Negative Rate (TNR): 0.57 | True Positive Rate (TPR): 0.6"
# [1] "Balanced Accuracy: 0.585"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference    0    1
# 0 0.57 0.43
# 1 0.40 0.60
report_metrics(normalized_val_df[, target_name], validation_preds)
# [1] "True Negative Rate (TNR): 0.57 | True Positive Rate (TPR): 0.58"
# [1] "Balanced Accuracy: 0.575"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference    0    1
# 0 0.57 0.43
# 1 0.42 0.58

## Teste 02
comb02 <- as.formula("target ~ 
                        start_position + end_position +
                        emini + chou_fasman +
                        hydrophobicity + stability")

x_train <- model.matrix(comb02, normalized_train_df)
y_train <- normalized_train_df[, target_name]

comb02_logreg_model <- glmnet(x_train, y_train, family="binomial",
                            weights = weights,
                            standardize = FALSE, alpha=0, lambda = 1e-6)

training_preds <- evaluate_model(comb02, comb02_logreg_model,
                                 normalized_train_df, 
                                 target_name)

validation_preds <- evaluate_model(comb02, comb02_logreg_model,
                                   normalized_val_df, 
                                   target_name)

report_metrics(normalized_train_df[, target_name], training_preds)
# [1] "True Negative Rate (TNR): 0.58 | True Positive Rate (TPR): 0.59"
# [1] "Balanced Accuracy: 0.585"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference    0    1
# 0 0.58 0.42
# 1 0.41 0.59

report_metrics(normalized_val_df[, target_name], validation_preds)
# [1] "True Negative Rate (TNR): 0.59 | True Positive Rate (TPR): 0.59"
# [1] "Balanced Accuracy: 0.59"
# [1] "Relative Confusion Matrix:"
# Prediction
# Reference    0    1
# 0 0.59 0.41
# 1 0.41 0.59

# O melhor modelo eh o polinomio de grau 9: poly_logreg_model
# [1] "True Negative Rate (TNR): 0.68 | True Positive Rate (TPR): 0.71"
# [1] "Balanced Accuracy: 0.695"
# [1] "Relative Confusion Matrix:"
#           Prediction
# Reference    0    1
# 0           0.68 0.32
# 1           0.29 0.71
# O modelo nao parece estar beneficiando nenhuma classe em detrimento da outra

## 06. Analise de Regularizacao

############ Regularization Analysis ############
get_mean_loss <- function(dataframe, target, predictions) {
  
  lossN = getLoss(dataframe[, target][dataframe[, target] == 0], 
                  predictions[predictions == 0])
  lossP = getLoss(dataframe[, target][dataframe[, target] == 1], 
                  predictions[predictions == 1])
  mean_loss <- (lossN+lossP)/2
  
  return(mean_loss)
}

lambda_values <- c(1.0, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6)

LossPerRegularization <- data.frame(regularization=numeric(length(lambda_values)), 
                                    TrainLoss=numeric(length(lambda_values)),
                                    ValLoss=numeric(length(lambda_values)))

AccPerRegularization <- data.frame(regularization=numeric(length(lambda_values)), 
                                   TrainACC=numeric(length(lambda_values)),
                                   ValACC=numeric(length(lambda_values)))

get_mean_loss(normalized_train_df, target_name, training_preds)

h <- getHypothesis(features_names, degree=9)

i <- 1
for(l in lambda_values){
  
  print(l)
  # Applying hypothesis and training the model
  x_train <- model.matrix(h, normalized_train_df)
  y_train <- normalized_train_df[, target_name]
  
  model <- glmnet(x_train, y_train,  family="binomial", 
                  standardize = FALSE, maxit = 1e+05, 
                  alpha=0, lambda = l)
  
  training_preds <- evaluate_model(h, model,
                                   normalized_train_df, 
                                   target_name)
  
  validation_preds <- evaluate_model(h, model,
                                     normalized_val_df, 
                                     target_name)
  
  balanced_accuracy_train <- report_metrics(normalized_train_df[, target_name], 
                                            training_preds, report_all = FALSE)
  
  balanced_accuracy_val <- report_metrics(normalized_val_df[, target_name], 
                                          validation_preds, report_all = FALSE)
  
  mean_loss_train <- get_mean_loss(normalized_train_df, target_name, training_preds)
  mean_loss_val <- get_mean_loss(normalized_val_df, target_name, validation_preds)
  
  LossPerRegularization[i,] <- c(i, mean_loss_train, mean_loss_val)
  AccPerRegularization[i,] <- c(i, balanced_accuracy_train, balanced_accuracy_val) 
  
  i <- i + 1
  
}

############# Plotting Loss ############
LossPerRegularizationMelt <- melt(LossPerRegularization, id="regularization")  # convert to long format
LossPerRegularizationMelt
p <- ggplot(data=LossPerRegularizationMelt, aes(x=regularization, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia - Loss") + ylab("Loss") + scale_x_discrete(name ="Parametro de regularizacao", 
                                                                                  limits=c("1.0", "0.1", "1e-2", "1e-3", "1e-4", "1e-5", "1e-6"))
p + theme(legend.position = c(0.4, 0.9), legend.title = element_blank())

############ Ploting Acc Balanced ############
AccPerRegularizationMelt <- melt(AccPerRegularization, id="regularization")  # convert to long format
AccPerRegularizationMelt
p <- ggplot(data=AccPerRegularizationMelt, aes(x=regularization, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia - Acuracia Balanceada") + ylab("ACC") + scale_x_discrete(name ="Parâmetro de regularizacao", 
                                                                                                limits=c("1.0", "0.1", "1e-2", "1e-3", "1e-4", "1e-5", "1e-6"))
p + theme(legend.position = c(0.8, 0.1), legend.title = element_blank())

# Training the best regularized model
h <- getHypothesis(features_names, degree=9)
x_train <- model.matrix(h, normalized_train_df)
y_train <- normalized_train_df[, target_name]
reg_model <- glmnet(x_train, y_train,  family="binomial", 
                standardize = FALSE, maxit = 1e+05, 
                alpha=0, lambda = 1e-4)


## 07. Avaliando os modelos sobre os dados de teste
test_df <- read.csv("./proteins_test_set.csv", stringsAsFactors=TRUE)
test_df[, target_name] <- as.factor(test_df[, target_name])

dim(test_df)
dim( unique(test_df) )
summary(test_df)

normalized_test_df <- normalize_features(features = features_names,
                                          not_training_df = test_df, 
                                          training_means = means_train, 
                                          training_stds = stds_train)
# Testing the models on the test set
## Baseline model
h <- getHypothesis(features_names, degree=1)
test_pred <- evaluate_model(h, logreg_baseline_model,
                            normalized_test_df, 
                           target_name)
report_metrics(normalized_test_df[, target_name], test_pred)
# [1] "True Negative Rate (TNR): 0.6 | True Positive Rate (TPR): 0.64"
# [1] "Balanced Accuracy: 0.62"
# [1] "Relative Confusion Matrix:"
#             Prediction
# Reference    0    1
# 0          0.60 0.40
# 1          0.36 0.64

## Modelo polinomial de grau 9
h <- getHypothesis(features_names, degree=9)
test_pred <- evaluate_model(h, poly_logreg_model,
                            normalized_test_df, 
                            target_name)
report_metrics(normalized_test_df[, target_name], test_pred)
# [1] "True Negative Rate (TNR): 0.66 | True Positive Rate (TPR): 0.69"
# [1] "Balanced Accuracy: 0.675"
# [1] "Relative Confusion Matrix:"
#             Prediction
# Reference    0    1
# 0           0.66 0.34
# 1           0.31 0.69

## Modelo polinomial de grau 9 com regularizacao
h <- getHypothesis(features_names, degree=9)
test_pred <- evaluate_model(h, reg_model,
                            normalized_test_df, 
                            target_name)
report_metrics(normalized_test_df[, target_name], test_pred)
# [1] "True Negative Rate (TNR): 0.94 | True Positive Rate (TPR): 0.21"
# [1] "Balanced Accuracy: 0.575"
# [1] "Relative Confusion Matrix:"
#            Prediction
# Reference    0    1
# 0           0.94 0.06
# 1           0.79 0.21

# Melhor modelo >>> Modelo polinomial de grau 9
## Report sobre os dados SARS
SARS_df <- read.csv("./SARS_test_set.csv", stringsAsFactors=TRUE)
SARS_df[, target_name] <- as.factor(SARS_df[, target_name])

dim(SARS_df)
dim( unique(SARS_df) )
SARS_df <- unique(SARS_df)
summary(SARS_df)

normalized_sars_df <- normalize_features(features = features_names,
                                         not_training_df = SARS_df, 
                                         training_means = means_train, 
                                         training_stds = stds_train)
## Modelo polinomial de grau 9
h <- getHypothesis(features_names, degree=9)
sars_pred <- evaluate_model(h, poly_logreg_model,
                            normalized_sars_df, 
                            target_name)
report_metrics(normalized_sars_df[, target_name], sars_pred)
# [1] "True Negative Rate (TNR): 0.26 | True Positive Rate (TPR): 0.83"
# [1] "Balanced Accuracy: 0.545"
# [1] "Relative Confusion Matrix:"
#             Prediction
# Reference    0    1
# 0            0.26 0.74
# 1            0.17 0.83


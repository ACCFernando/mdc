### ============ Trabalho 01 ============ ###

################## Membros ##################
#
# - Leonardo Cesar Silva dos Santos
# - Fernando Augusto Cardoso Candalaft
#
#############################################

# Este eh o codigo a partir do qual voces devem desevolver o Trabalho 01


# A funcao abaixo auxilia na escrita dos modelos polinomiais. 
# Parametros:
#  "real_feature_names": conjunto de features continuas que sera considerado na
#                        criacao do modelo polinomial.
#
#  "categorical_feature_names": conjunto de features categoricas que sera 
#                               considerado na  criacao do modelo polinomial. Se
#                                voces desejarem um modelo sem variaveis categoricas
#                               basta nao passar nenhum valor para este parametro
#                               na chamada da funcao
#                       
# "degree": numero inteiro que indica ate qual grau polinomial as features continuas
#           em "real_feature_names" serao elevadas. 
#
# A funcao retorna a hipotese ja definida para realizar o treinamento do modelo. 
# Uma ilustracao de uma funcao similar aparece no Ex02.R na linha 490

setwd("~/workspace/mdc/03_aprendizado_supervisionado_I_inf_0615/test01")

getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}


###======= Desenvolvam o trabalho a partir daqui =======###

# Notacao cientifica
options(scipen=999)

## Libs
library(ggplot2)
library(reshape2)

# Metricas de avaliacao do modelo
###################################
####   Define MAE function     ####
MAE <- function(preds, labels){
  mae_values <- sum(abs(preds-labels))/length(preds)
  return(mae_values)
}

####################################
####   Define MSE function     ####
MSE <- function(preds, labels){
  mse_values <- sum((preds-labels)**2)/length(preds)
  return(mse_values)
}

###################################
#### Define R-squared function ####
R2 <- function(pred, true){
  rss <- sum((pred - true) ^ 2)
  tss <- sum((true - mean(true)) ^ 2)
  r2 <- 1 - rss/tss
  return(r2)
}

## 01. Leitura e Inspecao dos dados

## Leitura dos dados
train_df <- read.csv("./T01_train_set.csv")
val_df <- read.csv("./T01_val_set.csv")
test_df <- read.csv("./T01_test_set.csv")

# Dropando valores duplicados
dim(train_df) # [1] 9336   19
train_df <- unique(train_df)
dim(train_df) # [1] 9336   19

## Inspecao dos dados
head(train_df)
hist(train_df$target)

summary(train_df) # Coluna categorica: weekday
# Checando os valores unicos da coluna weekday
unique(train_df$weekday)
# Aplicando o One-Hot Encoding
for (wd in unique(train_df$weekday)) {
  train_df[, wd] <- as.numeric(train_df$weekday == wd)
}
train_df$weekday <- NULL

summary(train_df)

sum(is.na(train_df)) # Nao ha valores NA no dataframe de treinamento
# Caso seja necessario, podemos dropar linhas com NA values usando na.omit(df)
# Se houvessem valores NA nos dados de treino poderiamos avaliar duas situaçoes:
# - Se fossem poucas linhas com relacao ao total poderiamos remove-las
# - Se fosse uma coluna com muito valores NA poderiamos nao usa-la como feature

## 02. Aplicando normalizacao aos dados continuos
# Z-Score
names(train_df) # Colunas continuas: 1 a 17

mean_features <- apply(train_df[,1:17], 2, mean)
sd_features <- apply(train_df[,1:17], 2, sd)
train_df[,1:17] <- sweep(train_df[,1:17], 2, mean_features, "-")
train_df[,1:17] <- sweep(train_df[,1:17], 2, sd_features, "/")

summary(train_df)

# Aplicando o mesmo processo de tratamento para os dados de validacao
for (wd in unique(val_df$weekday)) {
  val_df[, wd] <- as.numeric(val_df$weekday == wd)
}

val_df$weekday <- NULL
sum(is.na(val_df))

val_df[,1:17] <- sweep(val_df[,1:17], 2, mean_features, "-")
val_df[,1:17] <- sweep(val_df[,1:17], 2, sd_features, "/")

summary(val_df)

# Verificando se ha valores em comum nos dados de treino e validacao
merge(train_df, val_df) # Nao ha!

## 03. Treinando o baseline
set.seed(47)

real_feature_names <- names(train_df)[1:17]
cate_feature_names <- names(train_df)[19:25]

hypothesis <- getHypothesis(real_feature_names, cate_feature_names, degree=1)

# Baseline model
# lm([target] ~ [predictor / features], data = [data source])
baseline <- lm(formula=hypothesis, data=train_df)

# Predicao sobre o baseline
train_pred <- predict(baseline, train_df)
val_pred <- predict(baseline, val_df)

get_metrics_values <- function(base_dataframe, target_col, predictions) {
  
  mae <- MAE(predictions, base_dataframe[, target_col])
  mse <- MSE(predictions, base_dataframe[, target_col])
  r2 <- R2(predictions, base_dataframe[, target_col])
  
  vl_metrics <- c(mae, mse, r2)
  names(vl_metrics) <- c("MAE", "MSE", "R2")
  
  return(vl_metrics)
}

# Valores das metricas
# Treino
train_baseline_metrics <- get_metrics_values(train_df, "target", train_pred)
train_baseline_metrics
# MAE             MSE              R2 
# 716.38234428 873307.04394931      0.07190057

val_baseline_metrics <- get_metrics_values(val_df, "target", val_pred)
val_baseline_metrics
# MAE             MSE              R2 
# 718.93696624 890337.63977997      0.07016462
# R2 muito baixo para os dados de treino e validacao, ou seja, 
# pouca explicabilidade do modelo

# Avaliando os residuos
predictions_df <- data.frame(
  train_target=train_df$target, 
  val_target=val_df$target, 
  train_pred=train_pred, 
  val_pred=val_pred
)

predictions_df$train_res <- predictions_df$train_target - predictions_df$train_pred
predictions_df$val <- predictions_df$val_target - predictions_df$val_pred

ggplot(predictions_df, aes(x=train_target, y=train_pred)) +
  geom_point(color = "blue") +  # Adicionar pontos
  labs(x="target", y="predicted", title="Train Residues") 

ggplot(predictions_df, aes(x=val_target, y=val_pred)) +
  geom_point(color = "blue") +  # Adicionar pontos
  labs(x="target", y="predicted", title="Val Residues") 
# A expectativa era de que se os resultados fossem de fato bons entao 
# deveriamos ter uma reta crescente 
# Study: https://marcusnunes.me/posts/analise_de_res%C3%ADduos_em_modelos_de_regressao_usando_o_r/

## 04. Testando solucoes alternativas (com o auxilio do ChatGPT)

train_evaluate_model <- function(combination_formula, train_data, val_data) {
  
  lm_model <- lm(formula=combination_formula, data=train_data)
  
  train_predictions <- predict(lm_model, train_data)
  val_predictions <- predict(lm_model, val_data)
  
  print("Train metrics:")
  print( get_metrics_values(train_data, "target", train_predictions))
  print("Val metrics:")
  print( get_metrics_values(val_data, "target", val_predictions))
  
}
# Combination 01
combination01 <- formula(target ~ log_n_tokens_content * global_sentiment_polarity +
                           num_keywords * global_sentiment_polarity +
                           global_rate_positive_words / global_rate_negative_words +
                           n_tokens_title * log_self_reference_avg_sharess +
                           num_keywords * (avg_positive_polarity - avg_negative_polarity) +
                           global_sentiment_polarity * (avg_positive_polarity - avg_negative_polarity) +
                           log_n_tokens_content * global_rate_positive_words +
                           log_n_tokens_content * log_num_hrefs +
                           num_keywords * global_rate_positive_words +
                           n_tokens_title * (avg_positive_polarity - avg_negative_polarity))

train_evaluate_model(combination01, train_df, val_df)
# [1] "Train metrics:"
# MAE             MSE              R2 
# 728.65226503 894846.46450659      0.04900973 
# [1] "Val metrics:"
# MAE             MSE              R2 
# 724.63510851 900805.53228809      0.05923235 

# Add categorical features
combination02 <- formula(target ~ log_n_tokens_content * global_sentiment_polarity +
                           num_keywords * global_sentiment_polarity +
                           global_rate_positive_words / global_rate_negative_words +
                           n_tokens_title * log_self_reference_avg_sharess +
                           num_keywords * (avg_positive_polarity - avg_negative_polarity) +
                           global_sentiment_polarity * (avg_positive_polarity - avg_negative_polarity) +
                           log_n_tokens_content * global_rate_positive_words +
                           log_n_tokens_content * log_num_hrefs +
                           num_keywords * global_rate_positive_words +
                           n_tokens_title * (avg_positive_polarity - avg_negative_polarity) + Saturday + Friday + 
                           Tuesday + Monday + Wednesday + Thursday + Sunday
                           )

train_evaluate_model(combination02, train_df, val_df)
# [1] "Train metrics:"
# MAE             MSE              R2 
# 719.33360726 878068.59910961      0.06684026 
# [1] "Val metrics:"
# MAE             MSE              R2 
# 721.20960919 895403.30356372      0.06487423 

# Basic Features
combination03 <- formula("target ~ n_tokens_title + num_keywords + global_subjectivity + global_rate_positive_words + avg_positive_polarity + log_n_tokens_content")
train_evaluate_model(combination03, train_df, val_df)
# [1] "Train metrics:"
# MAE            MSE             R2 
# 745.9188674 922880.0400143      0.0192173 
# [1] "Val metrics:"
# MAE             MSE              R2 
# 749.08539036 938673.01435999      0.01968496 

# Content-Length-Related Features
combination04 <- formula("target ~ log_n_tokens_content + log_num_hrefs + root2_num_self_hrefs")
train_evaluate_model(combination04, train_df, val_df)
# [1] "Train metrics:"
# MAE              MSE               R2 
# 753.993235235 935159.519916944      0.006167387 
# [1] "Val metrics:"
# MAE              MSE               R2 
# 757.134040326 954075.098079899      0.003599601 

# Sentiment-Related Features
combination05 <- formula("target ~ global_subjectivity + global_sentiment_polarity + avg_positive_polarity + avg_negative_polarity")
train_evaluate_model(combination05, train_df, val_df)
# [1] "Train metrics:"
# MAE             MSE              R2 
# 750.39434724 930042.09750250      0.01160588 
# [1] "Val metrics:"
# MAE              MSE               R2 
# 755.139735475 953148.037947127      0.004567788 

combination06 <- formula(target ~ 
                           (global_sentiment_polarity / log_n_tokens_content) + 
                           (num_keywords * global_rate_positive_words) + 
                           (avg_positive_polarity / avg_negative_polarity) + 
                           log_num_hrefs + 
                           (log_n_tokens_content * log_self_reference_max_shares) + 
                           (global_subjectivity * global_rate_negative_words) + 
                           (avg_negative_polarity / global_subjectivity) + 
                           (rate_positive_words * rate_negative_words) + 
                           (log_self_reference_avg_sharess / n_tokens_title))
train_evaluate_model(combination06, train_df, val_df)
# [1] "Train metrics:"
# MAE             MSE              R2 
# 726.67883331 890220.50726465      0.05392593 
# [1] "Val metrics:"
# MAE             MSE              R2 
# 723.59520886 896623.85706009      0.06359953 

combination07 <- formula(target ~ 
                           (global_sentiment_polarity / log_n_tokens_content) + 
                           (num_keywords * global_rate_positive_words) + 
                           (avg_positive_polarity / avg_negative_polarity) + 
                           log_num_hrefs + 
                           (log_n_tokens_content * log_self_reference_max_shares) + 
                           (global_subjectivity * global_rate_negative_words) + 
                           (avg_negative_polarity / global_subjectivity) + 
                           (rate_positive_words * rate_negative_words) + 
                           (log_self_reference_avg_sharess / n_tokens_title) + Saturday + Friday + 
                           Tuesday + Monday + Wednesday + Thursday + Sunday)
train_evaluate_model(combination07, train_df, val_df)
# [1] "Train metrics:"
# MAE            MSE             R2 
# 717.2858607 873973.8733531      0.0711919 
# [1] "Val metrics:"
# MAE             MSE              R2 
# 720.64061648 891991.60017679      0.06843729 *Melhor solucao das testadas
combination07_model <- lm(formula=combination07, data=train_df)

## 05. Testando solucoes alternativas

# Testando algumas solucoes polinomiais
hypothesis_d2 <- getHypothesis(real_feature_names, cate_feature_names, degree=2)
hypothesis_d3 <- getHypothesis(real_feature_names, cate_feature_names, degree=3)
hypothesis_d4 <- getHypothesis(real_feature_names, cate_feature_names, degree=4)
hypothesis_d5 <- getHypothesis(real_feature_names, cate_feature_names, degree=5)
hypothesis_d6 <- getHypothesis(real_feature_names, cate_feature_names, degree=6)
hypothesis_d7 <- getHypothesis(real_feature_names, cate_feature_names, degree=7)
hypothesis_d8 <- getHypothesis(real_feature_names, cate_feature_names, degree=8)
hypothesis_d9 <- getHypothesis(real_feature_names, cate_feature_names, degree=9)
hypothesis_d10 <- getHypothesis(real_feature_names, cate_feature_names, degree=10)
hypothesis_d11 <- getHypothesis(real_feature_names, cate_feature_names, degree=11)

hypothesis_vec <- c(hypothesis_d2, 
                    hypothesis_d3, 
                    hypothesis_d4, 
                    hypothesis_d5, 
                    hypothesis_d6, 
                    hypothesis_d7,
                    hypothesis_d8, 
                    hypothesis_d9, 
                    hypothesis_d10, 
                    hypothesis_d11)

# Plot da curva Vies x Variancia para identificacao de under/overfitting 
MaePerCombinationCat <- data.frame(polyDegree=numeric(length(hypothesis_vec)), 
                                   TrainMAE=numeric(length(hypothesis_vec)),
                                   ValMAE=numeric(length(hypothesis_vec)))  
i <- 1
d <- 2
for(h in hypothesis_vec){
  print(paste("Degree", d))
  
  model <- lm(formula=h, data=train_df)
  
  val_pred <- predict(model, val_df)
  train_pred <- predict(model, train_df)
  
  mae_val <- MAE(val_pred, val_df$target)
  mae_train <- MAE(train_pred, train_df$target)
  
  MaePerCombinationCat[i, ] <- c(d, mae_train, mae_val)
  i <- i + 1
  d <- d + 1
  
}  
MaePerCombinationCat
#    polyDegree TrainMAE   ValMAE
# 1           2 712.9499 716.5688
# 2           3 712.2570 717.1419
# 3           4 710.8237 715.8935 *Melhor modelo polinomial
# 4           5 710.1103 716.0732
# 5           6 709.0994 717.6095
# 6           7 708.6519 717.8417
# 7           8 708.1572 718.6752
# 8           9 707.1297 722.0124
# 9          10 706.5625 722.3649
# 10         11 706.3792 722.4782

# Plot
MaePerCombinationMelt <- melt(MaePerCombinationCat, id="polyDegree")  # convert to long format
p <- ggplot(data=MaePerCombinationMelt, aes(x=polyDegree, y=value, colour=variable)) + 
  geom_line(linetype = "dashed") + geom_point()

p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + 
  scale_x_discrete(name ="polyDegree", limits=as.character(1:length(hypothesis_vec)))
p <- p + theme(legend.position = c(0.7, 0.85), legend.title = element_blank())
p <- p + geom_line(data = MaePerCombinationMelt) + 
  geom_point(data = MaePerCombinationMelt)
p + scale_color_manual(values = c("TrainMAE" = "red", "ValMAE" = "blue")) 
# Modelo comeca a entrar em overfitting a partir de degree = 5, 
# quando as curvas de MAE em treino e validacao comecam a se afastar

# Melhor modelo polinomial: degree = 4
poly_d4_model <- lm(formula=hypothesis_d4, data=train_df)


####################################################################
## Reportando metricas no conjunto de teste

# Tratando e normalizando os dados de teste
head(test_df)

for (wd in unique(test_df$weekday)) {
  test_df[, wd] <- as.numeric(test_df$weekday == wd)
}
test_df$weekday <- NULL
test_df[,1:17] <- sweep(test_df[,1:17], 2, mean_features, "-")
test_df[,1:17] <- sweep(test_df[,1:17], 2, sd_features, "/")

summary(test_df)

test_preds_baseline <- predict(baseline, test_df)
print( get_metrics_values(test_df, "target", test_preds_baseline) )
# MAE             MSE              R2 
# 727.63135736 903732.28411721      0.06543409 

test_preds_comb07 <- predict(combination07_model, test_df)
print( get_metrics_values(test_df, "target", test_preds_comb07) )
# MAE             MSE              R2 
# 729.61734432 908892.89381148      0.06009741

# Melhor modelo polinomial: degree = 4
test_preds_poly_d4 <- predict(poly_d4_model, test_df)
print( get_metrics_values(test_df, "target", test_preds_poly_d4) )
# MAE             MSE              R2 
# 726.87985906 901666.02558801      0.06757085

models_metrics_df <- data.frame(
                                MAE = c(727.63135736, 729.61734432, 726.87985906),
                                MSE = c(903732.28411721, 908892.89381148, 901666.02558801),
                                R2 = c(0.06543409, 0.06009741, 0.06757085),
                                Model = c("Baseline", "Combination 07", "Polynomial degree = 4")
                              )
models_metrics_df
#        MAE      MSE         R2                 Model
# 1 727.6314 903732.3 0.06543409              Baseline
# 2 729.6173 908892.9 0.06009741        Combination 07
# 3 726.8799 901666.0 0.06757085 Polynomial degree = 4


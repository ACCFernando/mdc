#############################################
#  MDC017 - Aprendizado Supervisionado I    #
#  Exercicio 01 - House Pricing             #
#############################################

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

merge(dataTrain, dataVal)

# Transforming to One-Hot-Encoding
dataTrain$less1Hocean <- as.numeric(dataTrain$ocean_proximity == "<1H OCEAN")
dataTrain$inland <- as.numeric(dataTrain$ocean_proximity == "INLAND")
dataTrain$island <- as.numeric(dataTrain$ocean_proximity == "ISLAND")
dataTrain$nearby <- as.numeric(dataTrain$ocean_proximity == "NEAR BAY")
dataTrain$nearocean <- as.numeric(dataTrain$ocean_proximity == "NEAR OCEAN")
dataTrain$ocean_proximity <- NULL

dataVal$less1Hocean <- as.numeric(dataVal$ocean_proximity == "<1H OCEAN")
dataVal$inland <- as.numeric(dataVal$ocean_proximity == "INLAND")
dataVal$island <- as.numeric(dataVal$ocean_proximity == "ISLAND")
dataVal$nearby <- as.numeric(dataVal$ocean_proximity == "NEAR BAY")
dataVal$nearocean <- as.numeric(dataVal$ocean_proximity == "NEAR OCEAN")
dataVal$ocean_proximity <- NULL

summary(dataTrain)


correlacao <- cor(dataTrain[,1:8])
correlacao

#install.packages("corrplot") # Descomente para instalar a biblioteca
                              # que plota a correlacao
library(corrplot)
corrplot(correlacao, method = "color", type = "upper")


# MinMax normalization
min_features <- apply(dataTrain[,1:8], 2, min)
min_features

max_features <- apply(dataTrain[,1:8], 2, max)
max_features

diff <- max_features - min_features
diff

dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, min_features, "-")
dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, diff, "/")
summary(dataTrain)

dataVal[,1:8] <- sweep(dataVal[,1:8], 2, min_features, "-")
dataVal[,1:8] <- sweep(dataVal[,1:8], 2, diff, "/")
summary(dataVal)

# Z-Norm normalization
#mean_features <- apply(dataTrain[,1:8], 2, mean)
#mean_features

#sd_features <- apply(dataTrain[,1:8], 2, sd)
#sd_features

#dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, mean_features, "-")
#dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, sd_features, "/")
#summary(dataTrain)

#dataVal[,1:8] <- sweep(dataVal[,1:8], 2, mean_features, "-")
#dataVal[,1:8] <- sweep(dataVal[,1:8], 2, sd_features, "/")
#summary(dataVal)

#plot(dataTrain[,"housing_median_age"], ylab = "value", ylim=c(0,1))
#points(dataTrain[,"total_rooms"], pch="*", col="blue")
#points(dataTrain[,"median_income"], pch="+", col="green")


## Baseline ##
## O comando abaixo encontrar? os melhores valores de theta 
## na seguinte express?o: 
## meadian_house_value = theta0 + theta1*longitude + theta2*latitude + 
##                              theta3*house_median_age + theta4*total_rooms + 
##                                theta5*total_bedrooms + theta6*population + 
##                            theta7*households + theta8*median_income
##
## O modelo, por padr?o, utiliza equa??es normais. Em modelos mais complexos e 
## com bases de dados muito grandes, ele automaticamente utiliza descida do gradiente.
baseline <- lm(formula=median_house_value ~ longitude + latitude + housing_median_age 
                                            + total_rooms + total_bedrooms + population 
                                            + households + median_income, data=dataTrain)

summary(baseline)

valPred <- predict(baseline, dataVal)
trainPred <- predict(baseline, dataTrain)

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

mae_train_baseline <- MAE(trainPred, dataTrain$median_house_value)
mae_train_baseline

mae_val_baseline <- MAE(valPred, dataVal$median_house_value)
mae_val_baseline

mse_train_baseline <- MSE(trainPred, dataTrain$median_house_value)
mse_train_baseline

mse_val_baseline <- MSE(valPred, dataVal$median_house_value)
mse_val_baseline

r2_train_baseline <- R2(trainPred, dataTrain$median_house_value)
r2_train_baseline

r2_val_baseline <- R2(valPred, dataVal$median_house_value)
r2_val_baseline



## Combining features ### 
f01 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income 
               + (longitude + latitude + housing_median_age + total_rooms
              + total_bedrooms + population + households + median_income)^2)

f02 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
              + total_bedrooms + population + households + median_income 
              + (longitude + latitude + housing_median_age + total_rooms
                 + total_bedrooms + population + households + median_income)^3)

f03 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
              + total_bedrooms + population + households + median_income 
              + (longitude + latitude + housing_median_age + total_rooms
                 + total_bedrooms + population + households + median_income)^4)

formulas <- c(f01, f02, f03)
MaePerCombination <- data.frame(combination=numeric(length(formulas)), 
                                TrainMAE=numeric(length(formulas)),
                                ValMAE=numeric(length(formulas)))

i <- 1
for(f in formulas){
  
  model <- lm(formula=f, data=dataTrain)
  
  valPred <- predict(model, dataVal)
  trainPred <- predict(model, dataTrain)
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  mae_val <- MAE(valPred, dataVal$median_house_value)
 
  MaePerCombination[i,] = c(i, mae_train, mae_val)
  i <- i + 1
  
}

summary(model)
MaePerCombination

MaePerCombinationMelt <- melt(MaePerCombination, id="combination")  # convert to long format
p <- ggplot(data=MaePerCombinationMelt, aes(x=combination, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:length(formulas)))
p + theme(legend.position = c(0.7, 0.85), legend.title = element_blank())


### ChatGPT ###

### Prompt: Considere as seguintes features em minha base de dados sobre imóveis, 
### com a qual devemos prever o valor do imóvel (median_house_value):
### longitude, latitude,  housing_median_age,  total_rooms,  total_bedrooms, 
### population,   households,   median_income,     median_house_value,   
### and ocean_proximity
### Gere novas features por meio da combinação não-linear das features a cima em R

# Voces podem pedir para o chatGPT gerar 5, 10, 20, 30, etc. features se desejarem

# Sigo a sugestao do ChatGPT mas mantenho a relação linear entre as features
f01 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income 
               + longitude*latitude)

f02 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income 
               + total_rooms/households + total_bedrooms/total_rooms)

f03 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income 
               + longitude*latitude + total_rooms/households + total_bedrooms/total_rooms)

formulas <- c(f01, f02, f03)
MaePerCombinationChatGPT <- data.frame(combination=numeric(length(formulas)), 
                                TrainMAE=numeric(length(formulas)),
                                ValMAE=numeric(length(formulas)))

i <- 1
for(f in formulas){
    
    model <- lm(formula=f, data=dataTrain)
    
    valPred <- predict(model, dataVal)
    trainPred <- predict(model, dataTrain)
    
    mae_train <- MAE(trainPred, dataTrain$median_house_value)
    mae_val <- MAE(valPred, dataVal$median_house_value)
    
    MaePerCombinationChatGPT[i,] = c(i, mae_train, mae_val)
    i <- i + 1
    
}

summary(model)
MaePerCombinationChatGPT

MaePerCombinationChatGPTMelt <- melt(MaePerCombinationChatGPT, id="combination")  # convert to long format
p <- ggplot(data=MaePerCombinationChatGPTMelt, aes(x=combination, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:length(formulas)))
p + theme(legend.position = c(0.75, 0.40), legend.title = element_blank())


#### Polynomials  ####
f01 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income, data=dataTrain)

f02 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
          + total_bedrooms + population + households + median_income + I(longitude^2) 
          + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
          + I(total_bedrooms^2) + I(population^2) + I(households^2) 
          + I(median_income^2), data=dataTrain)

f03 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3), data=dataTrain)

f04 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4), data=dataTrain)

f05 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5), data=dataTrain)

f06 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6), data=dataTrain)

f07 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7), data=dataTrain)

f08 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7) + I(longitude^8) 
               + I(latitude^8) + I(housing_median_age^8) + I(total_rooms^8)
               + I(total_bedrooms^8) + I(population^8) + I(households^8) 
               + I(median_income^8), data=dataTrain)

f09 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7) + I(longitude^8) 
               + I(latitude^8) + I(housing_median_age^8) + I(total_rooms^8)
               + I(total_bedrooms^8) + I(population^8) + I(households^8) 
               + I(median_income^8) + I(longitude^9) 
               + I(latitude^9) + I(housing_median_age^9) + I(total_rooms^9)
               + I(total_bedrooms^9) + I(population^9) + I(households^9) 
               + I(median_income^9), data=dataTrain)

f10 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7) + I(longitude^8) 
               + I(latitude^8) + I(housing_median_age^8) + I(total_rooms^8)
               + I(total_bedrooms^8) + I(population^8) + I(households^8) 
               + I(median_income^8) + I(longitude^9) 
               + I(latitude^9) + I(housing_median_age^9) + I(total_rooms^9)
               + I(total_bedrooms^9) + I(population^9) + I(households^9) 
               + I(median_income^9) + I(longitude^10) 
               + I(latitude^10) + I(housing_median_age^10) + I(total_rooms^10)
               + I(total_bedrooms^10) + I(population^10) + I(households^10) 
               + I(median_income^10), data=dataTrain)




formulas <- list(f01, f02, f03, f04, f05, f06, f07, f08, f09, f10)
MaePerDegree <- data.frame(degree=numeric(length(formulas)), 
                                TrainMAE=numeric(length(formulas)),
                                ValMAE=numeric(length(formulas)))

i <- 1
for(i in 1:10){
  model <- lm(formula=formulas[[i]], data=dataTrain)
  
  valPred <- predict(model, dataVal)
  trainPred <- predict(model, dataTrain)
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  mae_val <- MAE(valPred, dataVal$median_house_value)

  MaePerDegree[i,] <- c(i, mae_train, mae_val)
  i <- i + 1
  
}

MaePerDegreeMelt <- melt(MaePerDegree, id="degree")  # convert to long format
p <- ggplot(data=MaePerDegreeMelt, aes(x=degree, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Grau polinomial", 
                                                                            limits=as.character(1:length(formulas)))
p + theme(legend.position = c(0.4, 0.85), legend.title = element_blank())


#### Considering categorical ####
baseline <- lm(formula=median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + less1Hocean
               + inland + island + nearby + nearocean, data=dataTrain)

summary(baseline)

# Eh normal o seguinte warning aparecer ap?s exercutar os comandos abaixo:
# Warning message:
# In predict.lm(baseline, dataVal) :
#   uma predicao a partir de um ajuste rank-deficient pode ser enganoso
# 
# Esse warning ocorre por causa dos processo de One-Hot Encoding. Quando essas
# vari?veis sao adicionadas ao modelo, elas podem criar uma relacao de dependencia
# entre si. Por exemplo, no caso deste exerc?cio, ha cinco possiblidades de valor
# para as variaveis categoricas: less1Hocean, inland, island, nearby, nearocean.
# Suponha que um exemplo de treino seja "nearocean", logo ele necessariamente
# NAO EH "less1Hocean", "inland", "island" e nem "nearby". Logo, se todas essas
# categorias forem 0, necessariamente "nearocean" sera 1, portanto ha esta 
# redundacia, a qual gera o warning a cima. Ela nao prejudica de maneira nenhuma
# nem o treino e nem a validacao, entao podem seguir sem se preocupar com ela. 

valPred <- predict(baseline, dataVal)
trainPred <- predict(baseline, dataTrain)

mae_train_baseline_cat <- MAE(trainPred, dataTrain$median_house_value)
mae_val_baseline_cat <- MAE(valPred, dataVal$median_house_value)


f01_cat <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + households + median_income + population + 
               + (longitude + latitude + housing_median_age + total_rooms
                  + total_bedrooms + households + median_income + population)^2 + less1Hocean + inland
                + island + nearby + nearocean)

f02_cat <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + households + median_income + population + 
               + (longitude + latitude + housing_median_age + total_rooms
                  + total_bedrooms + households + median_income + population)^3 +less1Hocean + inland
               + island + nearby + nearocean)

f03_cat <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + households + median_income 
               + (longitude + latitude + housing_median_age + total_rooms
                  + total_bedrooms + households + median_income + population)^4 + less1Hocean + inland
               + island + nearby + nearocean)


formulas <- c(f01_cat, f02_cat, f03_cat)
MaePerCombinationCat <- data.frame(combination=numeric(length(formulas)), 
                           TrainMAE_cat=numeric(length(formulas)),
                           ValMAE_cat=numeric(length(formulas)))

i <- 1
for(f in formulas){
  
  model <- lm(formula=f, data=dataTrain)
  
  valPred <- predict(model, dataVal)
  trainPred <- predict(model, dataTrain)
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  mae_val <- MAE(valPred, dataVal$median_house_value)
  
  MaePerCombinationCat[i,] <- c(i,mae_train,mae_val)
  i <- i + 1
  
}

MaePerCombinationCatMelt <- melt(MaePerCombinationCat, id="combination")  # convert to long format
p <- ggplot(data=MaePerCombinationCatMelt, aes(x=combination, y=value, colour=variable)) + geom_line(linetype = "dashed") + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:length(formulas)))
p <- p + theme(legend.position = c(0.7, 0.85), legend.title = element_blank())
p <- p + geom_line(data = MaePerCombinationMelt) + geom_point(data = MaePerCombinationMelt)
p + scale_color_manual(values = c("TrainMAE" = "red","TrainMAE_cat" = "orange",
                                  "ValMAE" = "blue", "ValMAE_cat"="deepskyblue3")) 


#### Performance on TEST SET ####
#### Best Model - combination 4 by 4 with categorical ####

## Getting min value on validation set
min(MaePerCombination$ValMAE)
min(MaePerCombinationChatGPT$ValMAE)
min(MaePerDegree$ValMAE)
min(MaePerCombinationCat$ValMAE_cat)


MaePerCombinationCat$ValMAE_cat # third model of the categorical models

dataTest <- read.csv("housePricing_test_set.csv", header=TRUE, stringsAsFactors=TRUE)
any(is.na(dataTest))

dataTest$less1Hocean <- as.numeric(dataTest$ocean_proximity == "<1H OCEAN")
dataTest$inland <- as.numeric(dataTest$ocean_proximity == "INLAND")
dataTest$island <- as.numeric(dataTest$ocean_proximity == "ISLAND")
dataTest$nearby <- as.numeric(dataTest$ocean_proximity == "NEAR BAY")
dataTest$nearocean <- as.numeric(dataTest$ocean_proximity == "NEAR OCEAN")
dataTest$ocean_proximity <- NULL

dataTest[,1:8] <- sweep(dataTest[,1:8], 2, min_features, "-")
dataTest[,1:8] <- sweep(dataTest[,1:8], 2, diff, "/")

## Retrain the best model ##
best_model <- lm(formula=median_house_value ~ longitude + latitude + housing_median_age + total_rooms
                   + total_bedrooms + households + median_income 
                   + (longitude + latitude + housing_median_age + total_rooms
                      + total_bedrooms + households + median_income + population)^4 + less1Hocean + inland
                   + island + nearby + nearocean, data=dataTrain)



############################
testPred <- predict(best_model, dataTest)
mae_test <- MAE(testPred, dataTest$median_house_value)
mae_test



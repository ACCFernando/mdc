##########################################
# MDC017 - Aprendizado Supervisionado 01 #
# Exercicio 02 - Regressao Linear        #
##########################################

library(ggplot2)
library(reshape2)

# Limpeza de dados (Data Cleaning). Nao obrigatorio para nenhum
# trabalho, apenas colocado aqui para ilustrar uma possibilidade
# do processo de limpeza e divis?o da base de dados. 

help(lm)
set.seed(12)

data <- read.csv("Financial Distress.csv")
summary(data)
dim(data)

data$x80 <- NULL
data$Company <- NULL
data$Time <- NULL

summary(data)

# x1

# Distribuicao original
p1 <- hist(data$x1, breaks=30)

# Distribuicao apos aplicar uma funcao para tornar a distribuicao mais
# "gaussiana" e um pouco mais adequada para o treinamento
p1 <- hist(log(data$x1), breaks=30)
data$log_x1 <- log(data$x1)
data$x1 <- NULL

#x2
p1 <- hist(data$x2, breaks=30)

#x3
p1 <- hist(data$x3, breaks=30)

#x4
p1 <- hist(data$x4, breaks=30)
p1 <- hist(data$x4**0.3, breaks=30)
data$root03_x4 <- data$x4**0.3
data$x4 <- NULL

#x5
p1 <- hist(data$x5, breaks=30)

#x6
p1 <- hist(data$x6, breaks=30)

#x7
p1 <- hist(log(data$x7), breaks=30)
data$log_x7 <- log(data$x7)
data$x7 <- NULL

#x8 - remove
summary(data$x8)
p1 <- hist(data$x8, breaks=30)
data$x8 <- NULL

#x9
p1 <- hist(data$x9, breaks=30)

#x10
p1 <- hist(data$x10, breaks=30)

#x11
p1 <- hist(data$x11, breaks=30)

#x12
p1 <- hist(data$x12, breaks=30)
data$x12 <- NULL

#x13
p1 <- hist(data$x13, breaks=30)


#x14
p1 <- hist(log(data$x14), breaks=30)
data$log_x14 <- log(data$x14)
data$x14 <- NULL

#x15
p1 <- hist(data$x15, breaks=30)
data$x15 <- NULL

#x16
p1 <- hist(data$x16, breaks=30)
data$x16 <- NULL

#x17
p1 <- hist(data$x17, breaks=30)
data$x17 <- NULL

#x18
p1 <- hist(log(data$x18), breaks=30)

data$log_x18 <- log(data$x18)
data$x18 <- NULL

#x19
p1 <- hist(data$x19, breaks=30)
data$x19 <- NULL

#x20
p1 <- hist(log(data$x20), breaks=30)
data$log_x20 <- log(data$x20)
data$x20 <- NULL

#x21
p1 <- hist(data$x21**0.3, breaks=30)
data$root03_x21 <- data$x21**0.3
data$x21 <- NULL

#x22
p1 <- hist(data$x22, breaks=30)
data$x22 <- NULL

#x23
p1 <- hist(data$x23, breaks=30)

#x24
p1 <- hist(data$x24, breaks=30)

#x25
p1 <- hist((data$x25**2)**0.07, breaks=30)
data$pow2_root007_x25 <- (data$x25**2)**0.07
data$x25 <- NULL

#x26
p1 <- hist(data$x26, breaks=30)

#x27
p1 <- hist(data$x27, breaks=30)
data$x27 <- NULL

#x28
p1 <- hist((data$x28**2)**0.1, breaks=30)
data$pow2_root01_x28 <- (data$x28**2)**0.1
data$x28 <- NULL

#x29
p1 <- hist(log(data$x29), breaks=30)
data$log_x29 <- log(data$x29)
data <- data[data$log_x29 > -Inf,]
data$x29 <- NULL

#x30
p1 <- hist(data$x30, breaks=30)

#x31
p1 <- hist(data$x31, breaks=30)
data$x31 <- NULL

#x32
p1 <- hist(data$x32, breaks=30)
data$x32 <- NULL

#x33
p1 <- hist(log2(-log2(data$x33)), breaks=30)
data$log2_log2_x33 <- log2(-log2(data$x33))
data <- data[data$log2_log2_x33 > -Inf,]
data$x33 <- NULL

#x34
p1 <- hist(data$x34, breaks=30)
data$x34 <- NULL

#x35
p1 <- hist(data$x35, breaks=30)
data$x35 <- NULL

#x36
p1 <- hist((data$x36**2)**0.1, breaks=30)
data$pow2_root10_x36 <- (data$x36**2)**0.1
data$x36 <- NULL

#x37
p1 <- hist(log(data$x37), breaks=30)
data$log_x37 <- log(data$x37)
data <- data[data$log_x37> -Inf,]
data$x37 <- NULL

#x38
p1 <- hist(data$x38, breaks=30)
data$x38 <- NULL

#x39
p1 <- hist(data$x39, breaks=30)
data$x39 <- NULL

#x40
p1 <- hist(data$x40, breaks=30)

#x41
p1 <- hist(data$x41**0.25, breaks=30)
data$root4_x41 <- data$x41**0.25
data$x41 <- NULL

#x42
p1 <- hist(data$x42, breaks=30)
data$x42 <- NULL

#x43
p1 <- hist(data$x43, breaks=30)
data$x43 <- NULL

#x44
p1 <- hist(data$x44, breaks=30)
data$x44 <- NULL

#x45
p1 <- hist((data$x45**2)**0.1, breaks=30)
data$pow5_root01_x45 <- (data$x45**2)**0.1
data$x45 <- NULL

#x46
p1 <- hist(data$x46, breaks=30)
data$x46 <- NULL

#x47
p1 <- hist(data$x47, breaks=30)
data$x47 <- NULL

#x48
p1 <- hist(data$x48**0.01, breaks=30)
data$root001_x48 <- data$x48**0.01
data$x48 <- NULL

#x49
p1 <- hist(log(data$x49), breaks=30)
data$log_x49 <- log(data$x49)
data$x49 <- NULL

#x50
p1 <- hist(data$x50, breaks=30)

#x51
p1 <- hist(data$x51, breaks=30)

#x52
p1 <- hist(data$x52, breaks=30)
data$x52 <- NULL

#x53
p1 <- hist(data$x53, breaks=30)

#x54
p1 <- hist(data$x54, breaks=30)
data$x54 <- NULL

#x55
p1 <- hist(data$x55, breaks=30)
data <- data[(data$x55 >= -0.9 & data$x55 <= 0.9),]

#x56
p1 <- hist(data$x56**0.5, breaks=30)
data$root2_x56 <- data$x56**0.5
data$x56 <- NULL

#x57
p1 <- hist(data$x57, breaks=30)
data$x57 <- NULL

#x58
p1 <- hist(data$x58, breaks=30)
data$x58 <- NULL

#x59
p1 <- hist(data$x59, breaks=30)
data$x59 <- NULL

#x60
p1 <- hist(data$x60**0.85, breaks=30)
data$x60 <- NULL

#x61
p1 <- hist(data$x61, breaks=30)
data$x61 <- NULL

#x62
p1 <- hist(data$x62, breaks=30)
data$x62 <- NULL

#x63
p1 <- hist(data$x63, breaks=30)
data$x63 <- NULL

#x64
p1 <- hist(data$x64, breaks=30)
data$x64 <- NULL

#x65
p1 <- hist(data$x65, breaks=30)
data$x65 <- NULL

#x66
p1 <- hist(data$x66, breaks=30)
data$x66 <- NULL

#x67
p1 <- hist(data$x67, breaks=30)
data$x67 <- NULL

#x68
p1 <- hist(data$x68, breaks=30)
data$x68 <- NULL

#x69
p1 <- hist(data$x69, breaks=30)
data$x69 <- NULL

#x70
p1 <- hist(data$x70, breaks=30)
data$x70 <- NULL

#x71
p1 <- hist(data$x71, breaks=30)
data$x71 <- NULL

#x72
p1 <- hist(data$x72, breaks=30)
data$x72_c1 <- as.numeric((data$x72 <= 1))
data$x72_c2 <- as.numeric((data$x72 > 1 & data$x72 < 3))
data$x72_c1 <- as.numeric((data$x72 >= 3))
data$x72 <- NULL

#x73
p1 <- hist(data$x73, breaks=30)
data$x73 <- NULL

#x74
p1 <- hist(data$x74, breaks=30)
data$x74 <- NULL

#x75
p1 <- hist(data$x75, breaks=30)
data$x75 <- NULL

#x76
p1 <- hist(data$x76, breaks=30)

#x77
p1 <- hist(data$x77, breaks=30)

#x78
p1 <- hist(data$x78, breaks=30)

#x79
p1 <- hist(data$x79, breaks=30)
data$x79 <- NULL

#x81
p1 <- hist(data$x81, breaks=30)
data$x81 <- NULL

#x82
p1 <- hist(data$x82, breaks=30)

#x83
p1 <- hist(data$x83, breaks=30)

# financial.distress (target)
p1 <- hist(data$Financial.Distress, breaks=30)
data <- data[(data$Financial.Distress >= 0.01 & data$Financial.Distress <= 3.0),]

dim(data)
summary(data)

# Remocao de duplicatas
data <- unique(data)
dim(data)

any(is.na(data))
summary(data)

# A divis?o em treino, validacao e teste ser? feita respeitando a distribuicao
# do target de forma a evitar vieses.

trainSet <- data.frame(matrix(ncol = ncol(data), nrow = 0))
colnames(trainSet) <- colnames(data)

valSet <- data.frame(matrix(ncol = ncol(data), nrow = 0))
colnames(valSet) <- colnames(data)

testSet <- data.frame(matrix(ncol = ncol(data), nrow = 0))
colnames(testSet) <- colnames(data)

# Esse comando divide todo o intervalo de valor do target em 30
# intervalos menores, de forma que, para cada um desses intervalos,
# seja feita um divisao em treino/validacao/teste. Isso garante
# que cada intervalo tera representatividade nos tres conjuntos 
# o que alivia os vieses e melhora a generalizacao do modelo (ou seja
# o modelo consegue otimizar e atingit uma performance melhor nos conjuntos
# de validacao e teste)

breaks = seq(from=floor(min(data$Financial.Distress)), 
            to=ceiling(max(data$Financial.Distress)), length.out=30)

for(i in 1:(length(breaks)-1)){
    sp = breaks[i]
    ep = breaks[i+1]
    
    selected_portion <- data[(sp <= data$Financial.Distress & data$Financial.Distress < ep),]
 
    randomTrainValIndexes <- sample(1:nrow(selected_portion), size=round(0.8*nrow(selected_portion)))
    selectedTrainValSet <- selected_portion[randomTrainValIndexes,]
    selectedTestSet <- selected_portion[-randomTrainValIndexes,]
    
    randomTrainIndexes <- sample(1:nrow(selectedTrainValSet), size=round(0.8*nrow(selectedTrainValSet)))
    selectedTrainSet <- selectedTrainValSet[randomTrainIndexes,]
    selectedValSet <- selectedTrainValSet[-randomTrainIndexes,]
    
    trainSet <- rbind(trainSet, selectedTrainSet)
    valSet <- rbind(valSet, selectedValSet)
    testSet <- rbind(testSet, selectedTestSet)

}

### Assumindo que a divisao ja foi realizada, come?a-se o processo de inspecao
### normalizacao e treino dos modelos de regressao

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)

dim(trainSet)
dim(valSet)
dim(testSet)

summary(trainSet)
summary(valSet)
summary(testSet)

# Verifica-se a frequencia dos valores do target em cada intervalo 
# para os conjuntos de treino (em verde) e validcao (e azul). Nota-se
# que h? uma propor??o similar entre treino e valicao para cada um dos
# 30 intervalos.

p1 <- hist(trainSet$Financial.Distress, col=rgb(0,1,0,0.75), breaks=30)
p1$counts
p1 <- hist(valSet$Financial.Distress, col=rgb(0,0,1, 0.75), breaks=30, add=T)
p1$counts

## Normalizing
# MinMax normalization
#min_features <- apply(trainSet[,2:ncol(trainSet)], 2, min)
#min_features

#max_features <- apply(trainSet[,2:ncol(trainSet)], 2, max)
#max_features

#diff <- max_features - min_features
#diff

#trainSet[,2:ncol(trainSet)] <- sweep(trainSet[,2:ncol(trainSet)], 2, min_features, "-")
#trainSet[,2:ncol(trainSet)] <- sweep(trainSet[,2:ncol(trainSet)], 2, diff, "/")
#summary(trainSet)

#valSet[,2:ncol(valSet)] <- sweep(valSet[,2:ncol(valSet)], 2, min_features, "-")
#valSet[,2:ncol(valSet)] <- sweep(valSet[,2:ncol(valSet)], 2, diff, "/")
#summary(valSet)

#testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, min_features, "-")
#testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, diff, "/")
#summary(testSet)

#### Notmalizacao Z-Norma ####
mean_features <- apply(trainSet[,2:(ncol(trainSet)-2)], 2, mean)
mean_features

sd_features <- apply(trainSet[,2:(ncol(trainSet)-2)], 2, sd)
sd_features

trainSet[,2:(ncol(trainSet)-2)] <- sweep(trainSet[,2:(ncol(trainSet)-2)], 2, mean_features, "-")
trainSet[,2:(ncol(trainSet)-2)] <- sweep(trainSet[,2:(ncol(trainSet)-2)], 2, sd_features, "/")
summary(trainSet)

valSet[,2:(ncol(valSet)-2)] <- sweep(valSet[,2:(ncol(valSet)-2)], 2, mean_features, "-")
valSet[,2:(ncol(valSet)-2)] <- sweep(valSet[,2:(ncol(valSet)-2)], 2, sd_features, "/")
summary(valSet)

testSet[,2:(ncol(testSet)-2)] <- sweep(testSet[,2:(ncol(testSet)-2)], 2, mean_features, "-")
testSet[,2:(ncol(testSet)-2)] <- sweep(testSet[,2:(ncol(testSet)-2)], 2, sd_features, "/")
summary(testSet)

### Definicao da funcao que auxilia na escrita da sintaxe dos modelos
### polinomiais.
getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(Financial.Distress ~ "
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

feature_names <- colnames(trainSet)[2:(ncol(trainSet)-2)]
feature_names

categorical_names <- colnames(trainSet)[(ncol(trainSet)-1):ncol(trainSet)]
categorical_names

hypothesis <- getHypothesis(feature_names,categorical_names, 1)
hypothesis

## Baseline ##
baseline <- lm(formula=hypothesis, data=trainSet)

valPred <- predict(baseline, valSet)
trainPred <- predict(baseline, trainSet)
testPred <- predict(baseline, testSet)


MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

mae_train_baseline <- MAE(trainPred, trainSet$Financial.Distress)
mae_train_baseline

mae_val_baseline <- MAE(valPred, valSet$Financial.Distress)
mae_val_baseline

mae_test_baseline <- MAE(testPred, testSet$Financial.Distress)
mae_test_baseline

##### Combining features #####
cor(trainSet)
f01 <- formula(Financial.Distress ~ .)

f02 <- formula(Financial.Distress ~ . + (x2+x5+x9+x10+x23)^2 
                                      + (x11+x53+x50+x83+x30)^2 
                                      + (x10+x40+x50+x82+x24)^2)

f03 <- formula(Financial.Distress ~ . + (x2+x5+x9+x10+x23)^3 
                                       + (x11+x53+x50+x83+x30)^3 
                                       + (x10+x40+x50+x82+x24)^3)

f04 <- formula(Financial.Distress ~ . + (x2+x5+x9+x10+x23)^4 
                                       + (x11+x53+x50+x83+x30)^4 
                                       + (x10+x40+x50+x82+x24)^4)

formulas <- c(f01, f02, f03, f04)
MaePerCombination <- data.frame(combination=numeric(length(formulas)), 
                                TrainMAE=numeric(length(formulas)),
                                ValMAE=numeric(length(formulas)))

i <- 1
for(f in formulas){
    
    model <- lm(formula=f, data=trainSet)
    
    valPred <- predict(model, valSet)
    trainPred <- predict(model, trainSet)
    
    mae_train <- MAE(trainPred, trainSet$Financial.Distress)
    mae_val <- MAE(valPred, valSet$Financial.Distress)
    
    MaePerCombination[i,] <- c(i, mae_train, mae_val)
    
    i <- i + 1
    
}

MaePerCombinationMelt <- melt(MaePerCombination, id="combination")  # convert to long format
p <- ggplot(data=MaePerCombinationMelt, aes(x=combination, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:length(formulas)))
p + theme(legend.position = c(0.7, 0.7), legend.title = element_blank())

## Training again and testing
best_formula <- formulas[[which.min(MaePerCombination$ValMAE)]]
best_formula
model <- lm(formula=best_formula, data=trainSet)

testPred <- predict(model, testSet)
mae_test <- MAE(testPred, testSet$Financial.Distress)
mae_test

## Polynomial Analysis
MaePerDegree <- data.frame(degree=numeric(5), 
                           TrainMAE=numeric(5),
                           ValMAE=numeric(5))


for(i in 1:5){
   
    hypothesis <- getHypothesis(feature_names, categorical_names, i)
    
    model_poly <- lm(formula=hypothesis, data=trainSet)
    
    valPred <- predict(model_poly, valSet)
    trainPred <- predict(model_poly, trainSet)
    
    mae_train <- MAE(trainPred, trainSet$Financial.Distress)
    mae_val <- MAE(valPred, valSet$Financial.Distress)
    
    MaePerDegree[i,] <- c(i, mae_train, mae_val)
}

MaePerDegreeMelt <- melt(MaePerDegree, id="degree")  # convert to long format
p <- ggplot(data=MaePerDegreeMelt, aes(x=degree, y=value, colour=variable)) + geom_line() + geom_point()
p <- p + ggtitle("Curva vies/variancia") + ylab("MAE") + scale_x_discrete(name ="Combination", 
                                                                          limits=as.character(1:length(formulas)))
p + theme(legend.position = c(0.4, 0.9), legend.title = element_blank())

#### Testing #### 
i<- which.min(MaePerDegree$ValMAE)
i

hypothesis <- getHypothesis(feature_names, categorical_names, i)
best_model <- lm(formula=hypothesis, data=trainSet)


testPred <- predict(best_model, testSet)
mae_test <- MAE(testPred, testSet$Financial.Distress)
mae_test


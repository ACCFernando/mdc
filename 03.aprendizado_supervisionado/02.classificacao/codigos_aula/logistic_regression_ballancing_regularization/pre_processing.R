######################################################
# MDC010 - Aprendizado Supervisionado 01             #
# Codigo de limpeza e processamento da base de dados #
# utilizada nos exercicios 03, 04 e 05               #
######################################################

source("support_functions.R")

# Esta funcao assume que o �ltimo atributo � aquele que deve ser predito
applyRoot <- function(data){
    number_of_features <- length(names(data))
    features <- names(data)[1:(number_of_features-1)]
    print(features)
    for(feat in features){
        data[,feat] <- data[,feat]**0.1
    }
    return(data)
}


# Carregando dados originais
data <- read.csv("labs.csv")
summary(data)
dim(data)

# Selecionando manualmente algumas atributos, incluindo aquele a ser predito
# (LBDLDL - LDL-colesterol)
selected_features <- c("LBXTR", "LBDHDD", "LBDBANO", "LBDEONO", 
                       "LBDLYMNO", "LBDMONO", "LBXMC","LBDB12",
                       "LBDBCDSI", "LBDBMNSI", "LBXGLT", "LBXAPB",
                       "LBDLDL")

new_data <- data[,selected_features]
summary(new_data)
dim(new_data)


## Eliminando os exemplos que apresentam NA no valor a ser predito. 
## Cuidado! Nem sempre podemos eliminar todos os exemplos! Nesse caso,
## em particular, a elimina��o � ben�fica
new_data <- new_data[!is.na(new_data$LBDLDL),]
summary(new_data)
dim(new_data)

## Categorizando o valor alvo assuming o threshold de 130.0. O mesmo 
## considerado pelos m�dicos para identificar colesterol alto ou baixo
new_data$class <- 0
new_data[new_data$LBDLDL >= 130.0, "class"] <- 1
new_data$class <- as.factor(new_data$class)
new_data$LBDLDL <- NULL

summary(new_data)
dim(new_data)

### Analisando alguns atributos ###

### Tomando um atributo pouco discriminativo
feature <- "LBDB12"
p1 <- hist(new_data[,feature][new_data$class == 0], breaks=20)
p2 <- hist(new_data[,feature][new_data$class == 1], breaks=20)
plot(p1, col=rgb(1,0,0,1/4))
plot(p2, col=rgb(0,0,1,1/4), add=T)

### Aplicando a Raiz de 10 para deixar em um formato mais "gaussiano"
feature <- "LBDB12"
p1 <- hist((new_data[,feature][new_data$class == 0])**0.1, breaks=20)
p2 <- hist((new_data[,feature][new_data$class == 1])**0.1, breaks=20)
plot(p1, col=rgb(1,0,0,1/4))
plot(p2, col=rgb(0,0,1,1/4), add=T)


### Tomando um atributo mais discriminativo
feature <- "LBXAPB"
p1 <- hist(new_data[,feature][new_data$class == 0], breaks=20)
p2 <- hist(new_data[,feature][new_data$class == 1], breaks=20)
plot(p1, col=rgb(1,0,0,1/4))
plot(p2, col=rgb(0,0,1,1/4), add=T)

### Aplicando a Raiz 10 para deixar em um formato mais "gaussiano"
feature <- "LBXAPB"
p1 <- hist((new_data[,feature][new_data$class == 0])**0.1, breaks=20)
p2 <- hist((new_data[,feature][new_data$class == 1])**0.1, breaks=20)
plot(p1, col=rgb(1,0,0,1/4))
plot(p2, col=rgb(0,0,1,1/4), add=T)

### Aplicando a raiz 10 ###
data <- applyRoot(new_data)
summary(data)
dim(data)

### Realizando a Divis�o Treino/Validacao/Test consideando as classes ###
negatives <- data[data$class == 0,]
positives <- data[data$class == 1,]

dim(negatives)
dim(positives)

positive_split <- splitTrainValTest(positives)
negative_split <- splitTrainValTest(negatives)

trainSet <- rbind(positive_split[[1]], negative_split[[1]])
valSet <- rbind(positive_split[[2]], negative_split[[2]])
testSet <- rbind(positive_split[[3]], negative_split[[3]])

summary(trainSet)
summary(valSet)
summary(testSet)

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)

### Removendo NA's nos conjuntos tomando a m�dia dos valores no TREINO ###
for(feature in colnames(trainSet)[1:(ncol(trainSet)-1)]){
    print(feature)
    mean_value <- mean(trainSet[,feature], na.rm = TRUE)
    
    # Filling NA on Training Set
    selected_lines <- is.na(trainSet[,feature])
    trainSet[selected_lines, feature] <- mean_value
    
    
    # Filling NA on Validation Set
    selected_lines <- is.na(valSet[,feature])
    valSet[selected_lines, feature] <- mean_value
    
    
    # Filling NA on Test Set
    selected_lines <- is.na(testSet[,feature])
    testSet[selected_lines, feature] <- mean_value
    
}

# Inspecoes finais
summary(trainSet)
summary(valSet)
summary(testSet)

dim(trainSet)
dim(valSet)
dim(testSet)

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)

# As tr�s bases ja foram salvas nos arquivos disponibilizados juntos
# com este exercicio.
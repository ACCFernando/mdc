###########################################################
# MDC017 - Aprendizado Supervisionado I                   #
# Exercicio 08 - arvores de Decisao e Floresta Aleatoria  #
# Analise de Telemarketing de um Banco                    #
###########################################################  
library(caret)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)


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

# Remove os elementos repetidos antes da divisao Treino/Validacao/Test
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

randomNoIdx <- sample(1:nrow(dataTrainNo), size=1.4*nrow(dataTrainYes))
subsamplingNo <- dataTrainNo[randomNoIdx,]
dataTrain <- rbind(dataTrainYes, subsamplingNo)


## Arvore de Decisao
help(rpart)

# minsplit = numero minimo de exemplos em um noh para que ele gere nohs 
# filhos.
# cp = fator que determina o quanto o erro no conjunto de treinamento deve 
# ser diminuido para que a geracao de filhos (split) seja realizada. 
# xval = numero de validacoes cruzadas que serao realizadas. Ou seja, 
# xval = 10 significa que a divisao treinamento/validacao sera realizada 10
# vezes. 

# Se quisermos usar como criterio a Entropia + Ganho de Informacao - parametro
# "information"
treeModel <- rpart(formula=y ~ age + job + marital + education + default + 
                       balance + housing + loan+ contact + day + 
                       month + campaign + pdays + 
                       previous + poutcome, 
                   data=dataTrain, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                   parms= list(split="information"))


summary(treeModel)
####    Explicacao do significado de cada parte de um noh da arvore  #####

#Node number 608662871: 10 observations,    complexity param=0.0002988643
#predicted class=yes  expected loss=0.2  P(node) =0.00124533
#class counts:     2     8
#probabilities: 0.200 0.800 
#left son=1217325742 (1 obs) right son=1217325743 (9 obs)
#Primary splits:
#    balance  < 825.5   to the right, improve=1.8645350, (0 missing)
#    age      < 34      to the left,  improve=1.6389660, (0 missing)
#    day      < 5.5     to the right, improve=1.6389660, (0 missing)
#    campaign < 1.5     to the right, improve=1.1849390, (0 missing)
#    housing  splits as  RL,          improve=0.8161371, (0 missing)


# Respectivo identificador do noh na arvore e  
# numero de observacoes que alcancaram esse noh. 
# Node number 608662871: 10 observations 

# Quantidade, em pontos pencentuais, que o erro foi reduzido ao adicionar
# este noh. Lembre-se do calculo feito para o CP. Este valor eh obtido por split,
# e novamente adicionado nas informacoes de cada no particularmente.
# complexity param=0.0002988643


# A classe majoritaria dos exemplos que alcancaram este noh.
# predicted class=yes 

# Fracao dos exemplos da menor classe que alcancaram este noh.
# expected loss=0.2  

# Probabilidade de alcancar este noh desde a raiz
# P(node) = 0.00124533 

# Valor absoluto das quantidades de exemplos de cada classe 
# que alcancaram esse noh
# class counts:    2   8 

# Fracao das quantidades de exemplos de cada classe que 
# alcancaram este noh
#probabilities: 0.2 0.8  

# indices de cada um dos filhos desse noh e quantos exemplos
# foram designados para cada um deles. 
# left son=1217325742 (1 obs) right son=1217325743 (9 obs)


# Ordem dos atributos de acordo com o ganho que cada um fornece 
# para a arvore de decisao. O primeiro deles eh utilizado pelo 
# modelo para decidir para qual filho direcionar o exemplo 
# que tera seu target predito. Nesse caso, "balance", para este noh,
# eh o atributo mais importante a ser observado, seguido respectivamente
# por "age", "day", "campaign" e "housing". O "improve" eh calculado tomando
# a formula do "Gain(S,A)" visto em aula e multiplicando pelo numero de 
# elementos que alcancaram o noh, utilizando o Numero de Euler como 
# base do logaritmo. Dessa maneira, calcula-se  a entropia para os exemplos
# que apresentam valor "balance" abaixo de 825.5 e para aqueles que apresentam
# o valor de "balance" a cima (ou igual) de 825.5. Com os valores de cada uma destas 
# duas entropias, com a entropia total do noh, e com as respectivas
# quantidades, conseguimos calcular o Ganho. Depois, nos o multiplicamos
# pelo numero de elementos no noh, e assim obtemos o "improve". Passo 
# a passo desse processo eh mostrado ao final do Exercicio 07.

#Primary splits:
#    balance  < 825.5   to the right, improve=1.8645350, (0 missing)
#    age      < 34      to the left,  improve=1.6389660, (0 missing)
#    day      < 5.5     to the right, improve=1.6389660, (0 missing)
#    campaign < 1.5     to the right, improve=1.1849390, (0 missing)
#    housing  splits as  RL,          improve=0.8161371, (0 missing)

# Repare que alguns atributos tem um limiar com o qual o modelo compara
# para decidir para qual noh enviar. Ou seja, para o atributo "balace", 
# ha um limiar de 825.5 . Valores abaixo desse limiar sao enviados para o filho
# da direita e, valores a cima ou igual a este limiar, para os filhos da
# esquerda. No entanto, "housing" apresenta as letras "L" e "R", indicando
# que se o valor de "housing" for "no", entao o exemplo vai para 
# o filho "L" (Left), mas se "housing" for "yes", o exemplo vai 
# para "R" (Right). Sabe-se que "L" e "R" 
# referem-se a "no" e "yes" respectivamente pois o modelo os coloca em ordem
# alfabetica as categorias presentes na feature. 
# Mais detalhes veja o exemplo ao final do codigo do Exercicio 07.

###############################################################

### Verificando a importacia de cada feature ###
importance_per_feature <- treeModel$variable.importance
relative_importance <- importance_per_feature/sum(importance_per_feature)
relative_importance

######### Poda apos treinamento (POST PRUNE) ########

# Mostra a tabela com os valores de CP e erros
printcp(treeModel)

# Realiza a poda baseado no valor de CP com menor erro no conjunto
# de validacao. 
minCP <- treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"]
minCP

ptree <- prune(treeModel, cp=minCP)
summary(ptree)

######### Avalia??o ##########

# Vamos ver a performance no conjunto de validacao sem a poda
val_pred <- predict(treeModel, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$y), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

# Agora vamos checar a performance do modelo apos a poda
val_pred <- predict(ptree, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$y), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal


########## ACC Vs Depth 
# Vamos ver como as acuracias no conjunto de treinamento e de validacao
# variam conforme variamos o tamanho limite das arvores
number_of_depths = 20
accPerDepth <- data.frame(depth=numeric(number_of_depths), 
                          accTrain=numeric(number_of_depths), 
                          accVal=numeric(number_of_depths))
summary(accPerDepth)
for (maxDepth in 1:number_of_depths){
    treeModel <- rpart(formula=y ~ age + job + marital + education + default + 
                               balance + housing + loan + contact + day + month + 
                               campaign + pdays + previous + poutcome, 
                       data=dataTrain, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, 
                                             maxdepth=maxDepth, xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treino
    train_pred <- predict(treeModel, dataTrain, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                          reference = as.factor(dataTrain$y), 
                          positive='yes')
    
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_relative_train[1,1] + cm_relative_train[2,2])/2
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, dataVal, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                                reference = as.factor(dataVal$y), 
                                positive='yes')
    
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_relative_val[1,1] + cm_relative_val[2,2])/2
    
    accPerDepth[maxDepth,] = c(maxDepth, acc_bal_train, 
                               acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

############# RANDOM FOREST (Floresta Aleat?ria) ###############
help(randomForest)


# Treina uma Floresta Aleatoria
rfModel <- randomForest(formula=y ~ age + job + marital + education + 
                                default + balance + housing + loan + 
                                contact + day + month + campaign + pdays +
                                previous + poutcome, 
                        data= dataTrain, ntree=100, mtry=7)


# Plotando o erro para cada classe a para OOB. Para saber
# o significado e explicacao do OOB veja o exercicio 07 
# na secao de Floresta Aleatoria.

layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) # Sem margem no lado direito
plot(rfModel, log="y")
par(mar=c(5,0,4,2)) # Sem margem do lado esquerdo
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfModel$err.rate),col=1:4,cex=0.8,fill=1:4)


# Matriz de Confusao
val_pred <- predict(rfModel, dataVal, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(dataVal$y), 
                      positive='yes')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal


# Vamos verificar agora como as acuracias de treinamento e de validcao
# variam com o numero de arvores na floresta aleatoria
nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500) #, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))



for (i in 1:length(nTreeList)){
    rfModel <- randomForest(formula=y ~ age + job + marital + education + 
                                default + balance + housing + loan + 
                                contact + day + month + campaign + pdays +
                                previous + poutcome, 
                            data= dataTrain, ntree=nTreeList[i], mtry=7)
    
    # Avaliando no conjunto de treino
    train_pred <- predict(rfModel, dataTrain, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(dataTrain$y), 
                                positive='yes')
    
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_relative_train[1,1] + cm_relative_train[2,2])/2
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, dataVal, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$y), 
                              positive='yes')
    
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_relative_val[1,1] + cm_relative_val[2,2])/2
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()

### Avaliando melhores modelos no conjunto de teste ###

### Arvore de decisao
maxDepth <- 8 # Depth da melhor arvore observando a curva vies/variancia
bestTreeModel <- rpart(formula=y ~ age + job + marital + education + default + 
                       balance + housing + loan + contact + day + month + 
                       campaign + pdays + previous + poutcome, 
                   data=dataTrain, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, 
                                         maxdepth=maxDepth, xval = 0),
                   parms= list(split="information"))

test_pred <- predict(bestTreeModel, dataTest, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                      reference = as.factor(dataTest$y), 
                      positive='yes')

cm_relative_test <- calculaMatrizConfusaoRelativa(cm_test)
cm_relative_test

acc_bal_test <- (cm_relative_test[1,1] + cm_relative_test[2,2])/2
acc_bal_test

### Floresta Aleatoria
ntree <- 100 # Melhor numero de arvores baseado na curva vies/variancia
best_rfModel <- randomForest(formula=y ~ age + job + marital + education + 
                            default + balance + housing + loan + 
                            contact + day + month + campaign + pdays +
                            previous + poutcome, 
                        data= dataTrain, ntree=ntree, mtry=7)

test_pred <- predict(best_rfModel, dataTest, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                           reference = as.factor(dataTest$y), 
                           positive='yes')

cm_relative_test <- calculaMatrizConfusaoRelativa(cm_test)
cm_relative_test

acc_bal_test <- (cm_relative_test[1,1] + cm_relative_test[2,2])/2
acc_bal_test


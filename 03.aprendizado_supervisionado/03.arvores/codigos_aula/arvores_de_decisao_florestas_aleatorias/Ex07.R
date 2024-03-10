#############################################
#  MDC017 - Aprendizado Supervisionado I    #
#  Exercise 07 - Banknote Authentication    #
# Arvores de Decisao e Random Forest        #  
############################################# 

# Descomente as seguintes linhas e execute-as para instalar
# as bibliotceas. Se ja estiverem instaladas, voce nao precisa
# executar estes comandos novamente.

#install.packages("caret", dependencies = TRUE)
#install.packages("reshape2", dependencies = TRUE)
#install.packages("ggplot2")
#install.packages("rpart", dependencies = TRUE)
#install.packages("rpart.plot")
#install.packages("randomForest", dependencies = TRUE)
#install.packages("ramify")

library(caret)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)


set.seed(12)

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

trainSet <- read.csv("banknote_authentication_train.csv", stringsAsFactors = T)
valSet <- read.csv("banknote_authentication_validation.csv", stringsAsFactors = T)
testSet <- read.csv("banknote_authentication_test.csv", stringsAsFactors = T)

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)


dim(trainSet)
dim(valSet)
dim(testSet)

summary(trainSet)
summary(valSet)
summary(testSet)

table(trainSet$class)
table(valSet$class)
table(testSet$class)

# Documentacao para arvore de decisao.
help(rpart)

# minsplit = numero  minimo de exemplos em um noh para que ele gere nohs filhos.
# cp = fator que determina o quanto o erro no conjunto de treinamento deve ser
# diminuido para que a geracao de filhos (split) seja realizada. 
# xval = numero de validacoes cruzadas que serao realizadas. Ou seja, 
# xval = 10 significa que a divisao treinamento/validacao sera realizada 10
# vezes, e a media e desvio padrao dos resultados sao reportados.

# Se quisermos usar como criterio a Entropia + Ganho de Informacao coloque 
# como parametro o nome "information".
treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy, 
                   data=trainSet, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                   parms= list(split="information"))

# Se quisermos usar o Gini como criterio de geracao de filhos.
#treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy,
#                    data=trainSet, method="class",
#                    control=rpart.control(minsplit=2, cp=0.0),
#                    parms= list(split="gini"))

# Mostra a tabela com o ganho de performance (CP).
# CP na linha i, eh calculado como:
# CP[i] = (rel_error[i] - rel_error[i+1])/(n_split[i+1] - n_split[i])
#
# Por exemplo,vamos calcular para i = 5. Temos que n_split[5] = 5, 
# n_split[6] = 7, rel_error[5] = 0.0848329 e rel_error[6] = 0.0257069. 
# Logo CP[5] = (0.0848329 - 0.0257069)/(7 - 5) = 0.029563
printcp(treeModel)


summary(treeModel)
####    Explicacao de cada parte das especificacoes de um noh #####

# Respectivo identificador do noh na arvore e 
# numero de observacoes que alcancaram o noh
# Node number 2: 469 observations 

# Reducao do erro relativo em pontos percentuais
# complexity param = 0.1336761

# Classe majoritaria dos exemplos que alcancou este no.
# predicted class = forgery

# Fracao da classe minoritaria dos exemplos que alcancaram este noh.
# expected loss = 0.2260128

# Probabilidade de alcancar este noh da raiz. A conta eh baseada
# no numero de exemplos enviados para cada filho. Assim se um noh 
# tem N exemplos e "Nd" exemplos sao enviados para o filho da direita
# e "Ne" exemplos sao enviados para o filho da esquerda, entao as 
# probabilidades de um exemplo descer pelo filho direito ou pelo esquerdo
# sao respectivamente: Nd/N e Ne/N. A probabilidade abaixo eh resultado
# da multiplicacao de cada probabilidade de cada filho desde o noh raiz
# ate o noh atual.

# P(node) = 0.5440835

# Quantidades absolutas de exemplos para cada classe que alcancaram 
# este noh. Baseado na informacao "predicted class", sabemos que 363
# sao da classe "forgery" e 106 da classe "genuine". 
# class counts:    363    106 

#  Mesma informacao que "class counts", mas em frequencias. 
# probabilities: 0.774 0.226 

# Numero de exemplos enviados para cada filho. Assim, a probabilidade
# de um exemplo ir para a direita eh 100/469 e de ir para esquerda eh 
# 369/469.
# "4" e "5" sao os indices dos filhos da esquerda 
# e da direita respectivamente.
# left son=4 (369 obs) right son=5 (100 obs) 

# Ordem dos atributos de acordo com o ganho que cada um fornece 
# para a arvore de decisao. O primeiro deles eh utilizado pelo 
# modelo para decidir para qual filho direcionar o exemplo 
# que tera seu target predito. Nesse caso, "skewness", para este noh,
# eh o atributo mais importante a ser observado, seguido respectivamente
# por "variance", "entropy" e "curtosis". O "improve" eh calculado tomando
# a formula do "Gain(S,A)" visto em aula e multiplicando pelo numero de 
# elementos que alcancaram o noh. Tambem utiliza-se o Numero de Euler como 
# base do logaritmo. Dessa maneira, calcula-se  a entropia para os exemplos
# que apresentam valor de"skewness" abaixo de 5.0956 e para aqueles que apresentam
# o valor de "skewness" a cima de 5.0956. Com os valores de cada uma destas 
# duas entropias, com a entropia total do noh, e com as respectivas
# quantidades, conseguimos calcular o Ganho. Depois podemos multiplica-lo 
# pelo numero de elementos no noh, e assim obter o "improve". 
# O Passo a passo desse calculo eh mostrado ao final do exercicio 
# utilizando o exemplo visto em aula.

#
#Primary splits:
#    skewness < 5.0956     to the left,  improve=91.49793, (0 missing)
#    variance < -2.80905   to the left,  improve=32.79927, (0 missing)
#    entropy  < -3.26915   to the right, improve=19.79886, (0 missing)
#    curtosis < 8.83885    to the right, improve=19.65944, (0 missing)

# Surrogate variables sao variaveis que sao utilizadas em um calculo ou
# em criterio de decisao caso uma outra variavel necessaria nao esteja
# presente. Neste noh em particular deste exercicio, 
# o metodo assume que o atributo "skweness" (o primeiro mais importante) 
# pode nao estar ausente, assim ele testa outros
# atributos disponiveis para simular o mesmo comportamento que esse noh
# teria caso a variavel original estivesse presente. Neste caso, o
# atributo "entropy" foi escolhido como segunda opcao com threshold
# de decisao de -4.4918

#Surrogate splits:
#    entropy  < -4.4918    to the right, agree=0.874, adj=0.41, (0 split)
#    variance < -5.14385   to the right, agree=0.825, adj=0.18, (0 split)
###############################################################

#Plot using prp
prp(treeModel)

#Plot using rpart.plot
rpart.plot(treeModel,
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)


### Verificando importancia das variaveis ###
importance_per_features <- treeModel$variable.importance
importance_per_features

relative_importance <- importance_per_features/sum(importance_per_features)
relative_importance

######### Poda pos treinamento (POST PRUNE) ########

# Mostra a tabela com CP's novamente
printcp(treeModel)

# Poda a ?rvore baseado no CP do menor erro no conjunto de validacao.
minCP <- treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"]
minCP

ptree <- prune(treeModel, cp=minCP)
summary(ptree)


# Plota a arvore de decisao podada
rpart.plot(ptree, 
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)


######### Avaliacao ##########
# Vamos ver a performance da arvore de Decisao sem a poda
val_pred <- predict(treeModel, valSet, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(valSet$class), 
                      positive='forgery')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

# Vamos ver agora a performance da arvore de decisao apos a poda
# com a primeira escolha.
val_pred <- predict(ptree, valSet, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(valSet$class), 
                      positive='forgery')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

########## ACC Vs Depth 
# Vamos ver como as acuracias de treinamento e de validacao 
# se comportam conforme variamos o tamanho da arvore de decisao. 
accPerDepth <- data.frame(depth=numeric(15), accTrain=numeric(15), accVal=numeric(15))
summary(accPerDepth)
for (maxDepth in 1:15){
    treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy, 
                       data=trainSet, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, 
                                             maxdepth=maxDepth, xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(treeModel, trainSet, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                          reference = as.factor(trainSet$class), 
                          positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, valSet, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                                reference = as.factor(valSet$class), 
                                positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
    
    accPerDepth[maxDepth,] = c(maxDepth, 
                               acc_bal_train, 
                               acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

###### Taking the best tree to run on test
treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy, 
                   data=trainSet, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, 
                                         maxdepth=5, xval = 0),
                   parms= list(split="information"))

test_pred <- predict(treeModel, testSet, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                          reference = as.factor(testSet$class), 
                          positive='forgery')

cm_test_relative <- calculaMatrizConfusaoRelativa(cm_test)
cm_test_relative

acc_bal_test <- (cm_test_relative[1,1] + cm_test_relative[2,2])/2
acc_bal_test

############# Floresta Aleatoria ################
help(randomForest)

# Treina a Floresta Aleatoria 
# mtry eh o numero de features que cada arvore da floresta 
# apresentara. Ou seja, para cada arvore da floresta, mtry
# features sao aleatoriamente amostradas para treinar esta arvore. 
rfModel <- randomForest(formula=class ~ variance + skewness 
                        + curtosis + entropy, 
                        data= trainSet, ntree=12, mtry=3)


# Plota o erro para cada classe e para o Out-Of-Bag (OOB)
# O OOB eh calculado com a seguinte logica: uma arvore qualquer
# da floresta eh treinada com amostragem aleatoria e com repeticao dos
# exemplos de treinamento, assim os exemplos de treinamento que
# nao estao nesta amostragem nao foram considerados para treinar esta arvore em particular. 
# Esses exemplos nao usados no treino sao usados para validar esta
# arvore e calcular sua performance. Repare que, para arvores diferentes,
# ha exemplos de treinamento diferentes e portanto os exemplos utilizados
# nesta validaco tambem sao diferentes. Como nao podemos comparar a 
# performance de cada arvore indivudualmente justamente pelo fato de 
# apresentarem conjuntos de validacao diferentes, toma-se a media da 
# performance para cada arvore sobre o respectivo conjunto de validacao, e
# essa performance eh chamada de Out-Of-Bag Error (OOB Error). 
# IMPORTANTE: OOB nao eh sinonimo de erro no conjunto de validacao! Ele
# eh um tipo de validacao interna da Floresta Aleatoria. Por esse motivo
# deixamos SEMPRE um conjunto de validacao externo (como temos neste 
# exercicio) para comparar os modelos!

layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) # Sem margem no lado direito 
plot(rfModel, log="y")
par(mar=c(5,0,4,2)) # Sem margem no lado esquerdo
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfModel$err.rate),col=1:4,cex=0.8,fill=1:4)


sizes = treesize(rfModel)
minSize = min(sizes)
maxSize = max(sizes)
# Verifica o tamanho das arvores a partir de um histograma
# de distribuicao dos tamanhos.
hist(sizes,
     main="Histogram of Tree Depths",
     xlab="Depth",
     ylab="Frequency",
     xlim=c(minSize,maxSize),
     ylim=c(0,maxSize),
     las=1, 
     breaks=maxSize - minSize)
axis(1, at=seq(minSize, maxSize, by=1), labels=seq(minSize, maxSize, by=1))


# Matriz de Confus?o
val_pred <- predict(rfModel, valSet, type="class")
cm_val <- confusionMatrix(data = as.factor(val_pred), 
                           reference = as.factor(valSet$class), 
                           positive='forgery')

cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
cm_val_relative

acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
acc_bal_val


# Vamos ver agora como as acuracias de treinamento e de validacao
# variam conforme aumentamos o numero de arvores na floresta. 
set.seed(42)
nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))


for (i in 1:length(nTreeList)){
    rfModel <- randomForest(formula=class ~ variance + skewness 
                            + curtosis + entropy, 
                            data= trainSet, ntree=nTreeList[i], mtry=3)
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(rfModel, trainSet, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(trainSet$class), 
                                positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, valSet, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(valSet$class), 
                              positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()

#### Avalia??o da melhor floresta no teste ####
# Treina a Floresta Aleat?ria
rfModel <- randomForest(formula=class ~ variance + skewness 
                        + curtosis + entropy, 
                        data= trainSet, ntree=25, mtry=3)

test_pred <- predict(rfModel, testSet, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                           reference = as.factor(testSet$class), 
                           positive='forgery')

cm_test_relative <- calculaMatrizConfusaoRelativa(cm_test)
cm_test_relative

acc_bal_test <- (cm_test_relative[1,1] + cm_test_relative[2,2])/2
acc_bal_test


######## Execu??o do Exemplo da Aula #############
classExample <- data.frame(tempo=c("ensolarado", "ensolarado", "nublado","chover",
                                   "chover", "chover", "nublado", "ensolarado",
                                   "ensolarado", "chover", "ensolarado", "nublado",
                                   "nublado", "chover"), 
                           temperatura=c("calor", "calor", "calor", "moderada", "frio",
                                         "frio", "frio", "moderada", "frio", "moderada",
                                         "moderada", "moderada", "calor", "moderada"),
                           umidade=c("Alta", "Alta", "Alta", "Alta", "Normal", "Normal",
                                     "Normal", "Alta", "Normal", "Normal", "Normal",
                                     "Alta", "Normal", "Alta"),
                           vento=c("Fraco", "Forte", "Fraco", "Fraco", "Fraco", "Forte",
                                   "Forte", "Fraco", "Fraco", "Fraco", "Forte", "Forte",
                                   "Fraco", "Forte"),
                           target=c("nao", "nao", "sim", "sim", "sim", "nao", "sim",
                                    "nao", "sim", "sim", "sim", "sim", "sim", "nao"),
                           stringsAsFactors = TRUE)

dim(classExample)
summary(classExample)

treeModel <- rpart(formula=target ~ tempo + temperatura + umidade + vento, 
                   data=classExample, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                   parms= list(split="information"))



printcp(treeModel)

summary(treeModel)
# Plota a arvore de decisao podada
rpart.plot(treeModel, 
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)

# Node number 1: 14 observations,    complexity param=0.3
# predicted class=sim  expected loss=0.3571429  P(node) =1
# class counts:      5     9
# probabilities: 0.357 0.643 
# left son=2 (10 obs) right son=3 (4 obs)
# Primary splits:
#    tempo       splits as  LLR, improve=2.1931200, (0 missing)
# umidade     splits as  LR,  improve=1.4734210, (0 missing)
# vento       splits as  LR,  improve=0.4670276, (0 missing)
# temperatura splits as  LRR, improve=0.2433601, (0 missing)


# Para o calculo do "improve" do atributo "tempo" ,
# segue-se o seguinte algoritmo:
# Toma-se "Chover" e "Ensolarado" em um ?nico conjunto. Se o exemplo 
# apresentar no atributo "tempo" os valores "Chover" ou "Ensolarado", 
# a ?rvore desce para a esquerda. Por isso as duas primeiras letras ap?s 
# "split as" sao "LL". Se apresentar o valor "Nublado" vai para a direita, 
# repare o "R" como ?ltima letra ("splits as  LLR"). 
# Sabe-se que os dois primeiros L's e o
# ?ltimo R referenciam os atributos "Chover", "Ensolarado" e "Nublado" 
# porque estes s?o apresentados em ordem alfabetica na descri??o do n?.

# Temos N = 14 exemplos no total, sendo 5 com target "nao" e 9 com target
# "sim". Assim, calculando a entropia com f?rmula vista em aula, mas
# trocando a base do logaritmo para o n?mero de Euler, temos:

Entropy_S = -((5/14)*log(5/14) + (9/14)*log(9/14))
Entropy_S 

# Assim sabemos que Entropy_S = 0.6518 
# Vamos utilizar a formula do ganho visto em aula, no entanto considerando
# "Chover" e "Ensolarado" como um inico nivel, e "Nublado" como outro nivel.
# Assim temos "nao" com 5 elementos e "sim" com 5 elementos para "Chover"
# + "Ensolarado", e 0 elementos com "nao"e 4 elementos com "sim" para nivel
# Nublado. Agora vamos calcular a entropia por nivel do atributo:


Entropy_chover_ensolarado = -((5/10)*log(5/10) + (5/10)*log(5/10))
Entropy_chover_ensolarado

# Zero substituido por 1e-12 para n?o resultar em NaN
Entropy_nublado = -((0/4)*log(1e-12/4) + (4/4)*log(4/4))
Entropy_nublado

# Assim temos que a Entropia para os niveis "Chover" + "Ensolarado" 
# eh igual a 0.6932, e para "Nublado" eh zero. Agora tomando as quantidades
# de exemplos de cada nivel temos: 10 para "Chover" + "Ensolarado" e 
# 4 para "Nublado". Colocando os valores na formula do Ganho temos:

gain = Entropy_S - (10/14)*Entropy_chover_ensolarado - (4/14)*Entropy_nublado
gain

# Agora basta multiplicar o ganho pelo numero total de exemplos que temos:

improve = 14*gain
improve

# Que eh exatamente o valor em "improve" mostrado no "Primary Splits". 
# Ha casos em que o valor nao eh exatamente o mesmo, com erros na terceira 
# ou quarta casa decimal. Isso ocorre devido a maneira como a biblioteca 
# "rpart" faz o calculos e questoes de arrendodamento e truncamento. 
# De qualquer maneira, esse eh o passo a passo que eh executado 
# para cada atributo e em cada no da arvore gerada. Como visto em aula, 
# toma-se aquele  atributo que gera o maior "improve". 


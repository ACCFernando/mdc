##################################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Codigos da Aula 1 - Conceitos Basicos
##################################################################

##################################################################
# Preparando ambiente - workspace
# # Linux
# setwd("~/usr/diretorio/INF-0611/aula1")
# # Windows
# setwd("C:\\user\\diretorio\\INF-0611\\aula1\\")

# # Instalando bibliotecas
# install.packages("proxy")
# install.packages("janeaustenr")
# install.packages("ggplot2") 
# install.packages("reshape2")


##################################################################
# Exemplo - Dados Estruturados
# importando a base de dados (data set)
data(cars)
# visualizando parte da base cars
head(cars)

# importando a base de dados (data set)
data(women)
# visualizando parte da base women
head(women)

##################################################################
# Exemplo - Dados Nao Estruturados
# importando a base de dados
library(janeaustenr)

# amostra de um dos livros de Jane Austen
austen_books()[c(10:14),]$text

##################################################################
# Exemplo - Distancia Euclidiana
# criando os pontos
p <- c(0, 2); q <- c(2, 0)
r <- c(3, 1); s <- c(5, 1)

# organizando os pontos em uma matriz
pontos <- do.call("rbind", list(p, q, r, s))

# nomeando linhas e colunas para referencia
rownames(pontos) <- c("p", "q", "r", "s"); 
colnames(pontos) <- c("x1", "x2"); pontos

# calculando a distancia euclidiana entre os pontos p e q 
# (dist: pacote padrao stat)
dist(pontos[c("p","q"),], method = "euclidean")

# calculando a distancia euclidiana entre todos pares de pontos
# (diag = TRUE indica que a diagonal da  matriz deve ser impressa)
dist(pontos, method = "euclidean", diag = TRUE)


##################################################################
# Exemplo - Distancia Manhattan
# calculando a distancia manhattan entre os pontos p e q 
dist(pontos[c("p","q"),], method = "manhattan")

# calculando a distancia manhattan entre todos pares de pontos 
dist(pontos, method = "manhattan", diag = TRUE)

##################################################################
# Exemplo - Distancia Minkowski
# carregando a base de dados iris
data(iris)
# informações sobre a base
?iris
# visualizando 6 primeiras amostras da base iris
head(iris)
# verificando as especies
as.vector(unique(iris$Species))

# calculando a matriz de distancias Minkowski com  p=2 equivalente
# a distancia euclidiana 
matriz_distancia <- dist(iris[,1:4], p = 2, 
                         method = "minkowski")
# convertendo o objeto em uma matriz para facilitar manipulacoes
matriz_distancia <- as.matrix(matriz_distancia)

# distancia entre amostras 1 e 2
matriz_distancia[1,2]

# comparando flores de classes distintas
# setosa x versicolor
matriz_distancia[1,51]

# versicolor x virginica
matriz_distancia[51,101]

# setosa x virginica
matriz_distancia[1,101]

##################################################################
# Exemplo - Simple Matching Coefficient
# criando os objetos binarios 
a <- c(1, 0, 0, 0, 1, 0, 0, 1, 1)
b <- c(0, 0, 0, 1, 1, 1, 0, 0, 1)

# calculando o SMC
SMC <- function (a, b) {
  return (sum(a == b) / length(a))
}

SMC(a,b)

##################################################################
# Exemplo - Coeficiente de Jaccard
# criando os objetos binarios
a <- c(1, 0, 0, 0, 1, 0, 0, 1, 1)
b <- c(0, 0, 0, 1, 1, 1, 0, 0, 1)

# calculando o coeficiente de Jaccard
J <- function(a, b) {
  num <- sum(a == b & a == 1)
  den <- length(a[a != b | a == 1])
  return (num / den)
}

J(a, b)

##################################################################
# Exemplo - Similaridade Cosseno
# criando os pontos
a <- c(2, 4); b <- c(3, 1); d <- c(2, 2)

# carregando a biblioteca com a implementacao da funcao simil para 
# computar a similaridade cosseno
library(proxy)

# calculando a similaridade entre os pontos a e b
simil(list(a, b), "cosine")

# calculando a similaridade entre os pontos a e d
simil(list(a, d), "cosine")

# carregando a base de dados
data(iris)
# amostras 1 e 2
iris[c(1, 2), c(1:4)]

# calculando a similaridade entre todos os pontos
iris_cosine_simil <- simil(iris[, 1:4], "cosine", diag = TRUE)
iris_cosine_simil <- as.matrix(iris_cosine_simil)

# similaridade do cosseno entre amostras 1 e 2
iris_cosine_simil[1, 2]

##################################################################
# Exemplo - Revocacao e Precisao
# incluindo o arquivo com as funcoes implementadas este arquivo 
# deve estar no mesmo workspace
source("ranking_metrics.R")

# criando conjunto de objetos
D <- LETTERS[1:10]

# vetor que indica se um elemento e relevante 
ground_truth <- c(1, 0, 1, 1, 1, 0, 1, 1, 1, 0)
# nomeando os itens do vetor para facilitar o acesso
names(ground_truth) <- D; ground_truth

# objetos recuperados (ranking parcial)
ranking <- c("A", "C", "B", "F", "J"); ranking

# calculando a precisao
precision(ground_truth, ranking, 5)

# calculando a revocacao
recall(ground_truth, ranking, 5)

##################################################################
# Exemplo - Taxa F1 
# considerando o ground_truth e ranking anteriores
ground_truth

ranking

# calculando a taxa F1
f1_score(ground_truth, ranking, 5)

##################################################################
# Exemplo - Precisao Media e 
# Media das Precisoes Medias
# considerando o ground_truth anterior
ground_truth

# rankings a e b completos
r_a <- c("A", "C", "B", "D", "F", "E", "G", "J", "I", "H")
r_b <- c("A", "B", "F", "C", "D", "E", "G", "J", "I", "H")

# calculando a precisao media
ap(ground_truth, r_a, 5)

# calculando a media das precisoes medias
map(list(list(ground_truth, r_a), list(ground_truth, r_b)), 5)

##################################################################
# Exemplo - Media de Ranking Reciproco
# preparando os dados
ground_truth

# rankings a e b
r_a <- c("B", "A", "F", "C", "J", "D", "I", "G", "H", "E"); r_a
r_b <- c("B", "F", "J", "A", "C", "D", "I", "G", "H", "E"); r_b

# calculando a media de ranking reciproco
mrr(ground_truth, list(r_a, r_b))

##################################################################
# Exemplo - Ganho Cumulativo, 
# Ganho Cumulativo Descontado 
# e Ganho Cumulativo Descontado Normalizado
# definindo os graus de relevancia 
relevancias <- c(3, 2, 1, 0, 2, 3, 1, 0, 3, 2); 
# nomeando os valores para facilitar o acesso
names(relevancias) <- D; relevancias

# criando o ranking
r_a <- c("A", "D", "B", "C", "H", "E", "G", "I", "F", "J")

# calculando o ganho cumulativo
cg(relevancias, r_a, 5)

# calculando o ganho cumulativo descontado
dcg(relevancias, r_a, 5)

# calculando o ganho cumulativo normalizado
ndcg(relevancias, r_a, 5)

##################################################################
# Exemplo - indice de Jaccard
# preparando os dados: 
# uma predicao e um vetor de posicoes no ranking
r_a <- c(4, 3, 2, 1, 7, 5, 6, 10, 8, 9)
r_b <- c(2, 8, 9, 4, 3, 1, 5, 10, 6, 7)

# calculando o indice de Jaccard
jaccard_index(r_a, r_b, 5)

##################################################################
# Exemplo - Distancia Kendall Tau
# preparando os dados
r_a <- c(4, 3, 2, 1, 5)
r_b <- c(2, 1, 5, 4, 3)
 
# calculando a distancia de Kendall Tau
kendall_tau(r_a, r_b)


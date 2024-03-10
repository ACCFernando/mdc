#####################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Codigos da Aula 1 - Conceitos Basicos
# Atividade Pratica
#####################################################


#####################################################
# OBJETIVO: 
# Criar um sistema de recuperacao de informacao que 
# retorne amostras de flores iris. Utilizaremos a 
# base de dados iris. Uma amostra dessa base deve ser 
# usada como consulta e o ranking deve ser formado 
# com base na distancia euclidiana entre os objetos. 
#####################################################


#####################################################
# Conhecendo a base de dados 
#####################################################

# carregando a base de dados (colecao de objetos)
data(iris)

# visualiza a base de dados (colecao de objetos)
head(iris)

# Numero de amostras do conjunto de dados
n <- nrow(iris); n

# Distribuicao das amostras 
# considerando apenas as sepalas
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, Sepal.Width, 
                 colour = Species)) + 
  geom_point() + theme_light()

# Distribuicao das amostras 
# considerando apenas as petalas
ggplot(iris, aes(x = Petal.Length, Petal.Width, 
                 colour = Species)) + 
  geom_point() + theme_light()


#####################################################
# Montando o sistema e preparando para a avaliacao
#####################################################

# Calcula a matriz de distancias euclidiana, que 
# servira de base para criar os rankings. Excluindo 
# a ultima coluna com as classes das flores 
distances <- dist(iris[, 1:4], method = "euclidean")
distances <- as.matrix(distances)

# Gerando a lista com os vetores ground truths
ground_truths <- list()
species <- c("setosa", "versicolor", "virginica")
for (i in 1:length(species)){
  ground_truths[[i]] <- as.numeric(iris[,5] == species[i])
}

# Verificando os ground_truth
ground_truths

# Posicoes da amostras usadas como consultas
queries <- c(50, 100, 150)

# Numero de consultas
nc <- length(queries)

#####################################################
# Executando consultas
#####################################################

# Ordena as amostras com base nos valores das 
# distancias das consultas para todas as amostras
rankings <- list()
for (i in 1:nc) {
  # cada ranking e um vetor das posicoes dos 
  # elementos mais relevantes
  rankings[[i]] <- order(distances[queries[i],])
}

#####################################################
# Avaliacao do sistema
#####################################################

# Incluindo implementacao das metricas
source("ranking_metrics.R")

# Definindo o tamanho do topo
top <- 75

for (i in 1:nc) {
  cat("Ranking", i, "\n")
  cat("P:", precision(ground_truths[[i]], rankings[[i]], 
                      top), "\n")
  cat("R:", recall(ground_truths[[i]], rankings[[i]], 
                   top), "\n")
  cat("AP:", average_precision(ground_truths[[i]], 
                               rankings[[i]], 
                               top), "\n\n")
}

plot_precision_recall_x_topk <- function(ranking, 
                                         gtruth, k) { 
  # Calculando a precisao com a funcao precision para 
  # cada valor de 1 ate k e armazenando no vetor p
  p <- mapply(precision, 1:k, MoreArgs = 
                list(gtruth = gtruth, 
                     ranking = ranking))
  
  # Calculando a revocacao com a funcao recall para 
  # cada valor de 1 ate k e armazenando no vetor r
  r <- mapply(recall, 1:k, MoreArgs = 
                list(gtruth = gtruth, 
                     ranking = ranking))
  
  # Criando um data.frame com k linhas e duas colunas 
  # que armazenam os valores de precisao (prec) e 
  # revocacao (rev)
  pr <- data.frame(prec = p, rec = r)
  plot_measure_x_topk(pr, k, 
                      list("Precisao", "Revocacao"), 
                      "Precisao e Revocacao X Top k")
}

# Precisao + Revocacao para a consulta 1
plot_precision_recall_x_topk(ranking = rankings[[1]], 
                             gtruth = ground_truths[[1]], 
                             k = 150)

# Precisao + Revocacao para a consulta 2
plot_precision_recall_x_topk(ranking = rankings[[2]], 
                             gtruth = ground_truths[[2]], 
                             k = 150)

# Precisao + Revocacao para a consulta 3
plot_precision_recall_x_topk(ranking = rankings[[3]], 
                             gtruth = ground_truths[[3]], 
                             k = 150)

#######################################
#    GRAFICO PRECISAO X REVOCACAO     #
#######################################

# Funcao que cria um grafico com precisao no 
# eixo y e revocacao no eixo x.
# Para executar essa funcao e necessario que as 
# funcoes do arquivo ranking_metrics.R 
# estejam carregadas
plot_prec_x_rec <- function(ranking, 
                            gtruth, k) {
  p <- mapply(precision, 1:k, MoreArgs = 
                list(gtruth = gtruth, 
                     ranking = ranking))
  r <- mapply(recall, 1:k, MoreArgs = 
                list(gtruth = gtruth, 
                     ranking = ranking))
  pr <- data.frame(prec = p, rec = r)
  plot_precision_x_recall(pr, "Precisao x Revocacao")
}

# Precisao x Revocacao para a consulta 1
plot_prec_x_rec(ranking = rankings[[1]], 
                gtruth = ground_truths[[1]], 
                k = 150)

# Precisao x Revocacao para a consulta 2
plot_prec_x_rec(ranking = rankings[[2]], 
                gtruth = ground_truths[[2]], 
                k = 150)

# Precisao x Revocacao para a consulta 3
plot_prec_x_rec(ranking = rankings[[3]], 
                gtruth = ground_truths[[3]], 
                k = 150)


# Funcao que cria um grafico com a media da
# precisao interpolada em 11 pontos no 
# eixo y e revocacao no eixo x.
# Para executar essa funcao e necessario que as 
# funcoes do arquivo ranking_metrics.R 
# estejam carregadas

plot_prec_x_rec_mean_11_points <- function(rankings, gtruth, k){
  
  l_pr <- NULL
  
  for(idx in 1:3){
    p <- mapply(precision, 1:k, MoreArgs = 
                  list(gtruth = gtruth[[idx]], 
                       ranking = rankings[[idx]]))
    r <- mapply(recall, 1:k, MoreArgs = 
                  list(gtruth = gtruth[[idx]], 
                       ranking = rankings[[idx]]))
    df <- data.frame(prec = p, rec = r)
    
    for (i in seq(from = 0.0, to = 1.0, by = 0.1)){
      l_pr <- rbind(l_pr, data.frame(prec = max(df[df$rec >= i, ]$prec), rec = i))
    }
  }
  
  for (i in seq(from = 0.0, to = 1.0, by = 0.1)){
    l_pr <- rbind(l_pr, data.frame(prec = mean(l_pr[l_pr$rec == i, ]$prec), rec = i))
  }
  
  plot_precision_x_recall_11_points(l_pr, "Precisao x Revocacao")
}

plot_prec_x_rec_mean_11_points(ranking = rankings, 
                               gtruth = ground_truths, 
                               k = 150)

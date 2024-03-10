#####################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Implementacoes dos metodos de avaliacao de ranking
#####################################################


#####################################################
# Notacao comum:
# 
# gtruth - ground truth: vetor 1xn binario, 
#     gtruth[i] = 1 indica que o i-esimo elemento 
#     e relevante e gtruth[i] = 0 indica que o 
#     i-esimo elemento NaO e relevante 
# relev: vetor com os graus relevancias dos elementos 
# ranking: vetor com indices para os elementos 
#     ordenados, deve ser possivel acessar o gtruth 
#     e o relev com esses indices
# k: tamanho do topo a ser considerado nos calculos
# r: total de elementos relevantes no topo_k
# t: total de elementos relevantes na base
#####################################################


#####################################################
# * Avaliacao para relevancia binaria:
#   - Precisao 
#   - Revocacao
#   - Taxa F1
#   - Precisao Media
#   - Media das Precisoes Medias
# 
#####################################################

######### 
# Precisao
# (P_k = r / k)
precision <- function(gtruth, ranking, k) {
  r <- sum(gtruth[ranking[1:k]])
  return(r / k)
}

######### 
# Revocacao
# (R_k = r / t)
recall <- function(gtruth, ranking, k) {
  r <- sum(gtruth[ranking[1:k]])
  t <- sum(gtruth)
  return(r / t)
}

########### 
# Taxa F1 
# (f1 = 2 * P_k * R_k / (P_k + R_k))
f1_score <- function(gtruth, ranking, k) {
  P_k <- precision(gtruth, ranking, k)
  R_k <- recall(gtruth, ranking, k)
  return((2 * P_k * R_k) / (P_k + R_k))
}

########## 
# Precisao Media 
# (AP_k = 1/r * sum((r_i - r_(i-1))*P_i))
ap <- function(gtruth, ranking, k) {
  r <- min(k, sum(gtruth[ranking]))
  if (r == 0) return(0)
  
  # indices do ranking onde um elemento relevante e
  # retornado
  k_relevant <- which(gtruth[ranking[1:k]] == 1)
  
  # lista de precisoes 
  Ps <- mapply(precision, k = k_relevant, 
               MoreArgs = list(gtruth = gtruth, 
                               ranking = ranking))
  if (length(Ps) != 0){
    return(sum(Ps) / r)
  }
  else{
    return(0)
  }
}
average_precision <- ap

########## 
# Medias das Precisoes Medias
# (MAP_k = 1/n * sum( AP_k ))
# gtruths_rankings e uma lista de pares 
# (ground truth, ranking)
map <- function(gtruths_rankings, k) {
  # aplicando a funcao de preciscao media em 
  # cada ranking da lista
  APs <- sapply(gtruths_rankings, 
                function(x) ap(x[[1]], x[[2]], k))
  
  return(mean(APs))
}
mean_average_precision <- map


#####################################################
# * Avaliacao para relevancia em niveis:
#   - Ganho Cumulativo 
#   - Ganho Cumulativo Descontado
#   - Ganho Cumulativo Descontado Normalizado
# 
#####################################################

########## 
# Ganho Cumulativo
# (CG_k = sum (rel_i))
cg <- function(relev, ranking, k) {
  return(sum(relev[ranking[1:k]]))
}
cumulative_gain <- cg 

########## 
# Ganho Cumulativo Descontado
# (DCG_k = sum (rel_i / lg (i+1)))
dcg <- function(relev, ranking, k) {
  # lista de descontos 
  desc <- log(2:(k+1), 2)
  
  return(sum(relev[ranking[1:k]] / desc))
}
discounted_cumulative_gain <- dcg

########## 
# Ganho Cumulativo Descontado Normalizado
# (NDCG_k = DCG_k / IDCG_k)
ndcg <- function(relev, ranking, k) {
  
  dcg <- dcg(relev, ranking, k)
  
  # usando a funcao order para obter as posicoes 
  # de um ranking ideal, ou seja, com os maiores 
  # graus de relevancia em ordem decrescente 
  ideal_ranking <- order(relev, decreasing=TRUE)
  idcg <- dcg(relev, ideal_ranking, k)
  
  return(dcg / idcg)
}
normalized_discounted_cumulative_gain <- ndcg

#####################################################
# * Avaliacao entre rankings:
#   - Media de Ranking Reciproco
#   - indice de Jaccard
#   - Distancia de Kendall Tau
# 
###########

########## 
# Media de Ranking Reciproco
# (MRR = 1 / n * sum ( 1 / pos_i))
mrr <- function(gtruth, rankings) { 
  # retorna a posicao da primeira ocorrencia do 
  # valor maximo da lista, no caso do ground truth
  # o valor maximo e 1
  pos_i <- function(ranking) {
    which.max(gtruth[ranking])
  }
  # aplicando a funcao pos_i em cada ranking
  rr <- mapply(pos_i, rankings)
  return(mean(1/rr))
}
mean_reciprocal_ranking <- mrr

########## 
# indice de Jaccard 
# (JAC_k = |a intersecao b |/|a uniao b|)
jaccard_index <- function(ranking_a, ranking_b, k) {
  uni <- union(ranking_a[1:k], ranking_b[1:k])
  inter <-  intersect(ranking_a[1:k], ranking_b[1:k])
  return(length(inter) / length(uni))
}

########## 
# Coeficiente de Kendall-Tau 
# 
kendall_tau <- function(ranking_a, ranking_b ) {
  D <- length(ranking_a)
  Cd <- (D * (D - 1)) / 2
  
  # obtendo as posicoes dos elementos nos rankings
  a <- order(ranking_a)
  b <- order(ranking_b)
  
  # funcao testa se ha uma discordancia entre as 
  # posicoes dos elementos i e j
  disagree <- function(i, j) {
    return((a[j] < a[i] && b[j] > b[i]) || 
             (a[j] > a[i] && b[j] < b[i]))
  }
  
  # funcao testa as discordancias entre o elemento 
  # j e os elementos i, com i < j
  count_disagree <- function(j) {
    mapply(disagree, i = 1:(j-1), 
           MoreArgs = list(j = j))
  }
  disagree <- unlist(mapply(count_disagree, D:2))
  
  return(sum(disagree / Cd))
}

#####################################################
# * Funcoes uteis para gerar plots:
#   - Tamanho do topo do ranking (eixo x) x
#     precisao e revocacao (eixo y)
#   - Precisao (eixo x) x  Revocacao (eixo y)
# 
###########
# Lista de cores para os plots
clrs <- c("#FC4E07", "#E7B800", "#141B41", "#00AFBB",
          "#74B45E", "#14591D", "#5B2A86")

plot_measure_x_topk <- function(df, k, legends, 
                                title) {
  library(ggplot2)
  library(reshape2)
  df$x <- c(1:k)
  df.long<-melt(df, id.vars="x")
  ggplot(df.long, aes(x, value, color=variable))+
    geom_line() + theme_light() +
    labs(colour = element_blank(), title = title) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_x_continuous(name = "Top k", 
                       limits = c(1, k), 
                       breaks = 5 * 1:(k/5), 
                       minor_breaks = NULL) + 
    scale_y_continuous(name = "", limits = c(0, 1), 
                       breaks = 0.1 * 0:10, 
                       minor_breaks = NULL)+
    scale_color_manual(labels = legends, 
                       values = clrs)
}

plot_precision_x_recall <- function(df,  title) {
  library(ggplot2)
  library(reshape2)
  
  ggplot(df, aes(x = rec)) + 
    geom_point(aes(y = prec)) + 
    geom_line(aes(y = prec)) +
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_x_continuous(name = "Revocacao", 
                       limits = c(0, 1), 
                       breaks = 0.1 * 0:10, 
                       minor_breaks = NULL) + 
    scale_y_continuous(name = "Precisao", limits = c(0, 1), 
                       breaks = 0.1 * 0:10, 
                       minor_breaks = NULL)
}

plot_precision_x_recall_11_points <- function(df,  title) {
  library(ggplot2)
  library(reshape2)
  
  ggplot(NULL, aes(x = rec)) + 
    geom_point(data=df[1:11,], aes(y = prec, color='Amostra 50')) + 
    geom_line(data=df[1:11,], aes(y = prec, color='Amostra 50')) +
    geom_point(data=df[12:22,], aes(y = prec, color='Amostra 100')) + 
    geom_line(data=df[12:22,], aes(y = prec, color='Amostra 100')) +
    geom_point(data=df[23:33,], aes(y = prec, color='Amostra 150')) + 
    geom_line(data=df[23:33,], aes(y = prec, color='Amostra 150')) +
    geom_point(data=df[34:44,], aes(y = prec, color='Media')) + 
    geom_line(data=df[34:44,], aes(y = prec, color='Media')) +
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(colour = element_blank(), title = title) + 
    scale_x_continuous(name = "Revocacao", 
                       limits = c(0, 1), 
                       breaks = 0.1 * 0:10, 
                       minor_breaks = NULL) + 
    scale_y_continuous(name = "Precisao", limits = c(0, 1), 
                       breaks = 0.1 * 0:10, 
                       minor_breaks = NULL)
}

#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 3 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Leonardo Cesar Siva dos Santos                                      
# - Fernando Augusto Cardoso Candalaft                                       
#                                        
# 
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("/home/rstudio/workspace/mdc/02_recuperacao_informacao_inf_0611/test03") 
options(warn=-1)
source("./ranking_metrics.R")
source("./trabalho3_base.R")

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens 
#----------------------------------------------------------------#
imagens <- read_images(path_plantas); names(imagens)

plot(load.image("./plantas/biloba_01.jpg")) # Testando o carregamento da imagem

#----------------------------------------------------------------#
# Obtem classe de cada imagem 
#----------------------------------------------------------------#
nome_classes <- get_classes(path_plantas); nome_classes # Matriz 1 x 50

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth(path_plantas, nome_classes, "biloba")
ground_truth_europaea <- get_ground_truth(path_plantas, nome_classes, "europaea")
ground_truth_ilex <- get_ground_truth(path_plantas, nome_classes, "ilex")
ground_truth_monogyna <- get_ground_truth(path_plantas, nome_classes, "monogyna")
ground_truth_regia <- get_ground_truth(path_plantas, nome_classes, "regia")

ground_truth_regia # Testando ground truth da classe "regia"

#----------------------------------------------------------------#
# Questao 1 
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  
  return(c(r, g, b))
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  
  img <- grayscale(img)[,,1,1]
  
  min_v <- min(img)
  max_v <- max(img)
  img <- ((img-min_v)/(max_v-min_v)) * 255
  # calcula histograma
  h <- hist(img, plot = FALSE, breaks = 0:255)$counts
  return(h)
}


# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  
  img <- grayscale(img)[,,1,1]

  min_v <- min(img)
  max_v <- max(img)
  img <- ((img-min_v)/(max_v-min_v)) * 255
  
  features <- NULL
  for(i in 0:2){
    for(j in 0:2){
      features <- cbind(features, momento(img, i,j, central=TRUE))
    }
  }
  return(features)
}


#----------------------------------------------------------------#
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)

features_t <- t(sapply(imagens, lbp_desc))
rownames(features_t) <- names(imagens)

features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)

features_c["./plantas/biloba_01.jpg", ] # Testando a matriz de features de Cor
length(features_c["./plantas/biloba_01.jpg", ]) # 765

features_t["./plantas/biloba_01.jpg", ] # Testando a matriz de features de Textura
length(features_t["./plantas/biloba_01.jpg", ]) # 255

features_s["./plantas/biloba_01.jpg", ] # Testando a matriz de features de Forma
length(features_s["./plantas/biloba_01.jpg", ]) # 9

#----------------------------------------------------------------#
# Questao 2 
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

plot(load.image(consulta_biloba))
plot(load.image(consulta_europaea))
plot(load.image(consulta_ilex))
plot(load.image(consulta_monogyna))
plot(load.image(consulta_regia))


# analisando rankings
analyse_rankings <- function(ranking, ground_truth, topks=c(5,10,15,20)) {
  
  rankings <- data.frame()  
  
  for(k in topks){
    
    precisao <- precision(ground_truth, ranking, k)
    revocacao <- recall(ground_truth, ranking, k)
    taxa_f1 <- f1_score(ground_truth, ranking, k)
    precisao_media <- ap(ground_truth, ranking, k)
    
    linha <- c(k, precisao, revocacao, taxa_f1, precisao_media)
    rankings <- rbind(rankings, linha)
  }
  names(rankings) <- c("amostra", "precisao", "revocacao", "taxa_f1", "precisao_media")
  
  return(rankings)
}


# criando descritor concatenando 
desc_all <- cbind(features_c, features_t, features_s)
desc_all["./plantas/biloba_01.jpg", ]
length(desc_all["./plantas/biloba_01.jpg", ]) # 1029 = 765 + 255 + 9

# criando rankings com descritor concatenado
# Consultas escolhidas: consulta_ilex e consulta_regia
ranking_ilex <- get_ranking_by_distance(desc_all, consulta_ilex)
ranking_ilex

ranking_regia <- get_ranking_by_distance(desc_all, consulta_regia)
ranking_regia

# analisando os rankings 
metrics_ilex_df <- analyse_rankings(ranking_ilex, ground_truth_ilex)
ground_truth_ilex # Avaliando a posicao dos valores de ground truth para ilex
metrics_ilex_df

metrics_regia_df <- analyse_rankings(ranking_regia, ground_truth_regia)
ground_truth_regia # Avaliando a posicao dos valores de ground truth para regia
metrics_regia_df

#----------------------------------------------------------------#
# Questao 3 
#----------------------------------------------------------------#

# Consultas escolhidas: consulta_ilex e consulta_regia

# calculando as distancias, descritor:  histograma de cor 
features_c # Checando os valores
dist_hist_ilex <- get_distance_vector(features_c, consulta_ilex) 
dist_hist_regia <- get_distance_vector(features_c, consulta_regia) 

dist_hist_ilex # Checando as distancias com relacao as imagens do dataset

# calculando as distancias, descritor:  textura 
dist_text_ilex <- get_distance_vector(features_t, consulta_ilex) 
dist_text_regia <- get_distance_vector(features_t, consulta_regia)

# calculando as distancias, descritor:  forma 
dist_forma_ilex <- get_distance_vector(features_s, consulta_ilex) 
dist_forma_regia <- get_distance_vector(features_s, consulta_regia)

# calculando e analisando  rankings combmin
names(imagens)
combmin(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)

r_combmin_ilex <- names(imagens)[combmin(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combmin_regia <- names(imagens)[combmin(dist_hist_regia, dist_text_regia, dist_forma_regia)]
r_combmin_regia # Checando

analyse_rankings(r_combmin_ilex, ground_truth_ilex)
#   amostra precisao revocacao   taxa_f1 precisao_media
# 1       5      0.6       0.3 0.4000000      0.5500000
# 2      10      0.5       0.5 0.5000000      0.4130952
# 3      15      0.4       0.6 0.4800000      0.4676407
# 4      20      0.4       0.8 0.5333333      0.5534960
cmin_anl_regia <- analyse_rankings(r_combmin_regia, ground_truth_regia)
#   amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 0.4000000       0.2 0.2666667      0.4000000
# 2      10 0.2000000       0.2 0.2000000      0.2000000
# 3      15 0.2666667       0.4 0.3200000      0.2516484
# 4      20 0.3000000       0.6 0.4000000      0.3126391

# calculando e analisando  rankings combmax
r_combmax_ilex <- names(imagens)[combmax(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combmax_regia <- names(imagens)[combmax(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analyse_rankings(r_combmax_ilex, ground_truth_ilex)
#   amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 0.4000000       0.2 0.2666667      0.4000000
# 2      10 0.2000000       0.2 0.2000000      0.2000000
# 3      15 0.3333333       0.5 0.4000000      0.2963203
# 4      20 0.3000000       0.6 0.4000000      0.3296537
cmax_anl_regia <- analyse_rankings(r_combmax_regia, ground_truth_regia)
#   amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 1.0000000       0.5 0.6666667       1.000000
# 2      10 0.6000000       0.6 0.6000000       0.575000
# 3      15 0.5333333       0.8 0.6400000       0.685989
# 4      20 0.4500000       0.9 0.6000000       0.735989

# calculando e analisando  rankings combsum
r_combsum_ilex <- names(imagens)[combsum(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combsum_regia <- names(imagens)[combsum(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analyse_rankings(r_combsum_ilex, ground_truth_ilex)
#   amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 0.6000000       0.3 0.4000000       0.600000
# 2      10 0.3000000       0.3 0.3000000       0.300000
# 3      15 0.4666667       0.7 0.5600000       0.464349
# 4      20 0.3500000       0.7 0.4666667       0.464349

csum_anl_regia <- analyse_rankings(r_combsum_regia, ground_truth_regia)
#   amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 1.0000000       0.5 0.6666667      1.0000000
# 2      10 0.7000000       0.7 0.7000000      0.6700000
# 3      15 0.5333333       0.8 0.6400000      0.7271429
# 4      20 0.4000000       0.8 0.5333333      0.7271429

# calculando e analisando  rankings borda
r_borda_ilex <- names(imagens)[bordacount(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_borda_regia <- names(imagens)[bordacount(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analyse_rankings(r_borda_ilex, ground_truth_ilex)
#   amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 0.6000000       0.3 0.4000000       0.600000
# 2      10 0.3000000       0.3 0.3000000       0.300000
# 3      15 0.4666667       0.7 0.5600000       0.464349
# 4      20 0.3500000       0.7 0.4666667       0.464349

cborda_anl_regia <- analyse_rankings(r_borda_regia, ground_truth_regia)
# amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 1.0000000       0.5 0.6666667      1.0000000
# 2      10 0.6000000       0.6 0.6000000      0.6000000
# 3      15 0.4666667       0.7 0.5600000      0.6636364
# 4      20 0.4000000       0.8 0.5333333      0.7080808

#----------------------------------------------------------------#
# Consulta escolhida para analise: consulta_regia

# Checando as métricas de cada ranking novamente
cmin_anl_regia # $precisao_media
cmax_anl_regia
csum_anl_regia
cborda_anl_regia

# Questao 3 - RESPONDA:                   
# (i) 
# Analisando os quatro métodos de agrupamento de rankings (combmin, combmax, combsum e borda)
# para a consulta "regia" fica visivel por meio das métricas retornadas para 
# k em {5, 10, 15, 20} que os melhores rankings são dados pelo método CombMax e CombSum, uma vez 
# que possuem os maiores valores de Precisão, Revocação, Taxa F1 e Precisão Média para cada valor
# de k. 
# Mas, para selecionar o melhor ranking para a consulta em questão, podemos olhar para a 
# Precisão Média. 
# Para k=5 temos que ambos os rankings são equivalentes. Para k=10 o método CombSum é melhor em todas 
# as métricas. Para k=15 todas métricas são iguais, exceto a precisão média, na qual o método 
# CombSum é melhor. Por fim, para k=20 o método CombMax perfoma melhor em todas as métricas.
# Portanto, de modo geral o método **CombSum** é o que tem melhor perfomance quando olhamos para a
# consulta "regia".
# 
# 
# (j) 
maps_df <- data.frame()
for(k in c(5, 10, 15, 20)) {
  
  map_combmax <- map( list( 
    list(ground_truth_regia, r_combmax_regia), list(ground_truth_ilex, r_combmax_ilex)
    ), k ) 
  
  map_combmin <- map( list( 
    list(ground_truth_regia, r_combmin_regia), list(ground_truth_ilex, r_combmin_ilex)
  ), k ) 
  
  map_combsum <- map( list( 
    list(ground_truth_regia, r_combsum_regia), list(ground_truth_ilex, r_combsum_ilex)
  ), k ) 
  
  map_borda <- map( list( 
    list(ground_truth_regia, r_borda_regia), list(ground_truth_ilex, r_borda_ilex)
  ), k ) 
  
  linha <- c(k, map_combmin, map_combmax, map_combsum, map_borda)
  maps_df <- rbind(maps_df, linha)

}
names(maps_df) <- c("k", "map_combmin", "map_combmax", "map_combsum", "map_borda")
maps_df
#    k map_combmin map_combmax map_combsum map_borda
# 1  5   0.4750000   0.7000000   0.8000000 0.8000000
# 2 10   0.3065476   0.3875000   0.4850000 0.4500000
# 3 15   0.3596445   0.4911547   0.5957459 0.5639927
# 4 20   0.4330675   0.5328213   0.5957459 0.5862149


# O método **CombSum** possui uma média de precisões médias maior 
# com relação a média das precisões médias dos outros métodos de agregação.
# Este resultado se da por conta do fato de que a base para calcular o método CombSum envolve a soma
# das distâncias indexadas a cada imagem no seu respectivo ranking. Ou seja, rankings usados na agregação
# que individualmente tenham uma performance ruim não afetam tanto o método CombSum como afeta os métodos
# CombMax e CombMin, uma vez que estes avaliam os valores máximos e mínimos de cada imagem em seu respectivo
# ranking.

#----------------------------------------------------------------#


#----------------------------------------------------------------#
# Consulta usada na questao 3: consulta_regia

# Questao 4 - RESPONDA:                   
# (i) 
# Top 6 imagens retornadas pelo ranking concatenado via consulta_regia:
ranking_regia[1:6]
# [1] "./plantas/regia_07.jpg"    "./plantas/regia_03.jpg"    "./plantas/ilex_05.jpg"    
# [4] "./plantas/biloba_03.jpg"   "./plantas/biloba_06.jpg"   "./plantas/monogyna_10.jpg"
# Top 6 imagens retornadas pelo melhor ranking obtido via agregação (CombSum) via consulta_regia:
r_combsum_regia[1:6]
# [1] "./plantas/regia_07.jpg" "./plantas/regia_03.jpg" "./plantas/regia_01.jpg"
# [4] "./plantas/regia_04.jpg" "./plantas/regia_08.jpg" "./plantas/regia_06.jpg"
#
# Como o resultado buscado são imagens que sejam o mais próximo possível de régias, o esperado é 
# que tenhamos no Top k o maior número possível de imagens de régias, seguido por outras plantas
# que sejam semelhantes, de modo que o ranking obtido pelo método **CombSum** é mais interessante
# 
# 
# (ii) 
# > metrics_regia_df
#   amostra precisao revocacao   taxa_f1 precisao_media
# 1       5     0.40       0.2 0.2666667      0.4000000
# 2      10     0.20       0.2 0.2000000      0.2000000
# 3      15     0.20       0.3 0.2400000      0.2230769
# 4      20     0.25       0.5 0.3333333      0.2729221
# 
# > csum_anl_regia
#   amostra  precisao revocacao   taxa_f1 precisao_media
# 1       5 1.0000000       0.5 0.6666667      1.0000000
# 2      10 0.7000000       0.7 0.7000000      0.6700000
# 3      15 0.5333333       0.8 0.6400000      0.7271429
# 4      20 0.4000000       0.8 0.5333333      0.7271429
# 
# O ranking mais interessante para a consulta regia é de fato o que possui as melhores métricas, 
# CombSum, o que faz sentido, dado que é o ranking que possui as melhores métricas quando comparado 
# com os outros métodos de agregação e também quando comparamos as métricas desse ranking versus as 
# métricas retornadas pelo ranking do ranking concatenado (Cor + Textura + Forma).
# 
# 
# (iii)
topk <- 10
# Consultas disponíveis:
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"
#
# Descritores individuais:
## Cor
ranking_biloba <- get_ranking_by_distance(features_c, consulta_biloba)
ranking_europaea <- get_ranking_by_distance(features_c, consulta_europaea)
ranking_ilex <- get_ranking_by_distance(features_c, consulta_ilex)
ranking_monogyna <- get_ranking_by_distance(features_c, consulta_monogyna)
ranking_regia <- get_ranking_by_distance(features_c, consulta_regia)

map_desc_cor <- map( list( 
  list(ground_truth_biloba, ranking_biloba), 
  list(ground_truth_europaea, ranking_europaea),
  list(ground_truth_ilex, ranking_ilex),
  list(ground_truth_monogyna, ranking_monogyna),
  list(ground_truth_regia, ranking_regia)
), topk ); map_desc_cor # [1] 0.6690714
## Textura
ranking_biloba <- get_ranking_by_distance(features_t, consulta_biloba)
ranking_europaea <- get_ranking_by_distance(features_t, consulta_europaea)
ranking_ilex <- get_ranking_by_distance(features_t, consulta_ilex)
ranking_monogyna <- get_ranking_by_distance(features_t, consulta_monogyna)
ranking_regia <- get_ranking_by_distance(features_t, consulta_regia)

map_desc_textura <- map( list( 
  list(ground_truth_biloba, ranking_biloba), 
  list(ground_truth_europaea, ranking_europaea),
  list(ground_truth_ilex, ranking_ilex),
  list(ground_truth_monogyna, ranking_monogyna),
  list(ground_truth_regia, ranking_regia)
), topk ); map_desc_textura # [1] 0.6636429
## Forma
ranking_biloba <- get_ranking_by_distance(features_s, consulta_biloba)
ranking_europaea <- get_ranking_by_distance(features_s, consulta_europaea)
ranking_ilex <- get_ranking_by_distance(features_s, consulta_ilex)
ranking_monogyna <- get_ranking_by_distance(features_s, consulta_monogyna)
ranking_regia <- get_ranking_by_distance(features_s, consulta_regia)

map_desc_forma <- map( list( 
  list(ground_truth_biloba, ranking_biloba), 
  list(ground_truth_europaea, ranking_europaea),
  list(ground_truth_ilex, ranking_ilex),
  list(ground_truth_monogyna, ranking_monogyna),
  list(ground_truth_regia, ranking_regia)
), topk ); map_desc_forma # [1] 0.402619
#
# Descritor concatenado:
ranking_biloba <- get_ranking_by_distance(desc_all, consulta_biloba)
ranking_europaea <- get_ranking_by_distance(desc_all, consulta_europaea)
ranking_ilex <- get_ranking_by_distance(desc_all, consulta_ilex)
ranking_monogyna <- get_ranking_by_distance(desc_all, consulta_monogyna)
ranking_regia <- get_ranking_by_distance(desc_all, consulta_regia)

map_desc_all <- map( list( 
  list(ground_truth_biloba, ranking_biloba), 
  list(ground_truth_europaea, ranking_europaea),
  list(ground_truth_ilex, ranking_ilex),
  list(ground_truth_monogyna, ranking_monogyna),
  list(ground_truth_regia, ranking_regia)
), topk ); map_desc_all # [1] 0.402619
# Melhor ranking escolhido no item anterior: CombSum
## Cor
dist_hist_biloba <- get_distance_vector(features_c, consulta_biloba) 
dist_hist_europaea <- get_distance_vector(features_c, consulta_europaea) 
dist_hist_ilex <- get_distance_vector(features_c, consulta_ilex) 
dist_hist_monogyna <- get_distance_vector(features_c, consulta_monogyna) 
dist_hist_regia <- get_distance_vector(features_c, consulta_regia) 
## Textura
dist_text_biloba <- get_distance_vector(features_t, consulta_biloba) 
dist_text_europaea <- get_distance_vector(features_t, consulta_europaea) 
dist_text_ilex <- get_distance_vector(features_t, consulta_ilex) 
dist_text_monogyna <- get_distance_vector(features_t, consulta_monogyna) 
dist_text_regia <- get_distance_vector(features_t, consulta_regia) 
## Forma
dist_form_biloba <- get_distance_vector(features_s, consulta_biloba) 
dist_form_europaea <- get_distance_vector(features_s, consulta_europaea) 
dist_form_ilex <- get_distance_vector(features_s, consulta_ilex) 
dist_form_monogyna <- get_distance_vector(features_s, consulta_monogyna) 
dist_form_regia <- get_distance_vector(features_s, consulta_regia) 
# Calculando o CombSum
r_combsum_biloba <- names(imagens)[combsum(dist_hist_biloba, dist_text_biloba, dist_form_biloba)]
r_combsum_europaea <- names(imagens)[combsum(dist_hist_europaea, dist_text_europaea, dist_form_europaea)]
r_combsum_ilex <- names(imagens)[combsum(dist_hist_ilex, dist_text_ilex, dist_form_ilex)]
r_combsum_monogyna <- names(imagens)[combsum(dist_hist_monogyna, dist_text_monogyna, dist_form_monogyna)]
r_combsum_regia <- names(imagens)[combsum(dist_hist_regia, dist_text_regia, dist_form_regia)]

map_combsum <- map( list( 
  list(ground_truth_biloba, r_combsum_biloba), 
  list(ground_truth_europaea, r_combsum_europaea),
  list(ground_truth_ilex, r_combsum_ilex),
  list(ground_truth_monogyna, r_combsum_monogyna),
  list(ground_truth_regia, r_combsum_regia)
), topk ); map_combsum # [1] 0.754
# 
# Novamente, o método **CombSum** obteve o melhor resultado na métrica analisada 
# (média das precisões médias = 0.754) versus os outros métodos utilizados. 
# Este fato pode estar atrelado à forma como o CombSum é calculado, na qual o mesmo utiliza a soma
# da distância de cada imagem para a respectiva consulta no ranking gerado, de modo que trata cada ranking
# agregado de maneira igual, ou seja, rankings gerados com baixa performance, como o ranking gerado via Forma,
# não afetam de forma significativa a avaliação de perfomance do ranking agregado, diferente de quando
# avaliamos somente a performance do ranking gerado por uma feature isolada ou a concatenação de features.
#
# 
#----------------------------------------------------------------#
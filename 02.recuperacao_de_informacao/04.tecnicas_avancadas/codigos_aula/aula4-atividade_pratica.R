##################################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Atividade Pratica da Aula 4 - Tecnicas Avancadas
##################################################################


options(warn=-1)
library(imager)
library(ggplot2)

# setwd("/user/dir")
source("ranking_metrics.R")
source("atividade4_base.R")

############################################
#             CARREGA BASE                 #
############################################
# a variavel img contem as imagens 256x256
load(file="bacteria.rda")

# Agona
show_image(img[[1]], labels[1])

# Hadar
show_image(img[[6]], labels[6])

# Newport
show_image(img[[11]], labels[11])

# Typhimurium
show_image(img[[16]], labels[16])

#------------------------------------------------#
# Define classe relevante                        #
#------------------------------------------------#

classe_relevante <- 'Newport'

#------------------------------------------------#
# obtem ground_truth                             #
#------------------------------------------------#

ground_truth <- get_ground_truth(labels, classe_relevante)

#------------------------------------------------#
# define consulta classe relevante               #
#------------------------------------------------#

consulta <- 13

#------------------------------------------------#
# FUNCOES PARA EXTRAIR CARACTERISTICAS           #
#------------------------------------------------#

#------------------------------------------------#
# obtem caracteristicas de cor                   #
#------------------------------------------------#

features_color <- function(img) {
  # entrada: uma imagens carregada
  # saida: um vetor de caracteristicas de cor

  # tranforma intensidade de pixels em valores de 0 a 255  
  min_v <- min(img)
  max_v <- max(img)
  img <- ((img-min_v)/(max_v-min_v)) * 255
  # calcula histograma
  h <- hist(img, plot = FALSE, breaks = 0:255)$counts
  return(h)
}


#------------------------------------------------#
# obtem caracteristicas de textura               #
#------------------------------------------------#

features_texture <- function(img) {
  # entrada: uma imagens carregada
  # saida: um vetor de caracteristicas de textura 
  print("lbp")
  # tranforma intensidade de pixels em valores de 0 a 255  
  min_v <- min(img)
  max_v <- max(img)
  img <- ((img-min_v)/(max_v-min_v)) * 255
  
  # calcula histograma
  r1 <- lbp(img,1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=0:59)$counts
  return(lbp_uniforme)
}

#------------------------------------------------#
# obtem caracteristicas de forma                 #
#------------------------------------------------#

features_shape <- function(img) {
  # entrada: uma imagens carregada

  feature <- NULL
  for(i in 0:2) {
    for(j in 0:2) {
      # adiciona um novo momento como caracteristica 
      # no vetor de caracteristicas da imagem
      cat(i," ",j,"\n")
      feature <- cbind(feature, momento(img, i,j, central=TRUE))
    }
  }
  return(feature)
}



#------------------------------------------------#
# GERANDO DISTANCIAS E RANKINGS                  #
#------------------------------------------------#



# obtendo distancias
generate_distances <- function(features, query) {
  # entrada: conjunto de caracteristicas que serao 
  # utilizadas para calculo de distancia e indice 
  # da consulta no vetor de caracteristicas
  # saida: vetor não-ordenado de distancias das 
  # imagens para a consulta
  
  # calcular distancia euclidiana de todas as imagens 
  # (representada pelas caracteristicas) para a consulta
  distancia <- as.matrix(dist(features, "euclidean"))
  distancia <- distancia[ ,query]
  
  return(distancia)
}

#------------------------------------------------#
# EXTRACAO DE CARACTERISTICAS                    #
#------------------------------------------------#

features_s <- t(sapply(img, features_shape)) 
features_c <- t(sapply(img, features_color)) 
features_t <- t(sapply(img, features_texture)) 

#------------------------------------------------#
# DISTANCIAS DE CARACTERISTICAS                  #
#------------------------------------------------#

distances_c <- generate_distances(features_c, consulta)
distances_t <- generate_distances(features_t, consulta)
distances_s <- generate_distances(features_s, consulta)

#------------------------------------------------#
# AGREGANDO POR VALOR                            #
#------------------------------------------------#

ranking_combmin <- combmin(distances_c, distances_t, distances_s)
ranking_combmax <- combmax(distances_c, distances_t, distances_s)
ranking_combsum <- combsum(distances_c, distances_t, distances_s)

#------------------------------------------------#
# AGREGANDO POR POSICAO                          #
#------------------------------------------------#

ranking_borda <- bordacount(distances_c, distances_t, distances_s)

#------------------------------------------------#
# COMPARANDO RANKINGS                            #
#------------------------------------------------#

#######################################
#    GRAFICO PRECISAO X TOPK          #
#######################################

k <- 5

metric <- precision

# precisao no topk para combmin
p_combMin <- mapply(metric, 1:k, 
                    MoreArgs = list(gtruth = ground_truth, 
                                    ranking = ranking_combmin))
# precisao no topk para combmax
p_combMax <- mapply(metric, 1:k,
                    MoreArgs = list(gtruth = ground_truth, 
                                    ranking = ranking_combmax))
# precisao no topk para combsum
p_combSum <- mapply(metric, 1:k, 
                    MoreArgs = list(gtruth = ground_truth, 
                                    ranking = ranking_combsum))
# precisao no topk para borda count
p_borda <- mapply(metric, 1:k, 
                  MoreArgs = list(gtruth = ground_truth, 
                                  ranking = ranking_borda))

pr <- data.frame(p_combMin = p_combMin, p_combMax = p_combMax, 
                 p_combSum = p_combSum, p_borda = p_borda)

ggplot(pr, aes(x = 1:k)) + 
  geom_point(aes(y = p_combMin), color = clrs[1]) + 
  geom_line(aes(y = p_combMin), color = clrs[1]) +
  geom_text(aes(0.2, 1,label = "CombMin"), vjust= -0.3, color = clrs[1]) +
  geom_point(aes(y = p_combMax), color = clrs[2]) + 
  geom_line(aes(y = p_combMax),  color = clrs[2]) +
  geom_text(aes(0.2, 0.9,label = "CombMax"), vjust= -0.3, color = clrs[2]) +
  geom_point(aes(y = p_combSum),color = clrs[3]) + 
  geom_line(aes(y = p_combSum),color = clrs[3], linetype="dotted") +
  geom_text(aes(0.2, 0.8, label = "CombSum"), vjust= -0.3, color = clrs[3]) +
  geom_point(aes(y = p_borda),color = clrs[4]) + 
  geom_line(aes(y = p_borda),color = clrs[4], linetype="dashed") +
  geom_text(aes(0.2, 0.7, label = "Borda"), vjust= -0.3, color = clrs[4]) +
  theme_light() +
  labs(colour = element_blank(), title = "Precisão x TopK") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Precisão", limits = c(0, 1.1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(-0.25, k), 
                     breaks = 0:k,
                     minor_breaks = NULL)


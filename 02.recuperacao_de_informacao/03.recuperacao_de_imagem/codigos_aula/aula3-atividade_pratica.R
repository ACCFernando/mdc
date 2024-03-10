##################################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Atividade Pratica da Aula 3 - Recuperação de Imagem
##################################################################

setwd("C:\\Users\\ferna\\Documents\\aulas\\01. Mineração de Dados Complexos - Unicamp\\02. Recuperação de Informação- INF-0611\\03. Aula 3\\Códigos da Aula 3")

options(warn=-1)
library(imager)
library(ggplot2)

source("./ranking_metrics.R")
source("./atividade3_base.R")
source("./cbir_aula3.R")

############################################
#             CARREGA BASE                 #
############################################
# a variavel img contem as imagens 256x256
load(file="bacteria.rda")

show_image(img[[5]], labels[5])
labels
# Essas imagens possuem apenas um canal de cor!
dim(img[[1]])

#------------------------------------------------#
# Define classe relevante                        #
#------------------------------------------------#
classe_relevante <- 'Agona'

#------------------------------------------------#
# obtem ground_truth                             #
#------------------------------------------------#

ground_truth <- get_ground_truth(labels, 
                                 classe_relevante)

#------------------------------------------------#
# define consulta classe relevante               #
#------------------------------------------------#
consulta <- 1

#------------------------------------------------#
# FUNCOES PARA EXTRAIR CARACTERISTICAS           #
#------------------------------------------------#

# obtem caracteristicas de cor                   
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

# obtem caracteristicas de textura               
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

# obtem caracteristicas de forma
features_shape <- function(img) {
  # entrada: uma imagens carregada
  print("momentos")
  feature <- NULL
  for(i in 0:2) {
    for(j in 0:2) {
      # adiciona um novo momento como caracteristica 
      # no vetor de caracteristicas da imagem
      feature <- cbind(feature, momento(img, i,j, central=TRUE))
    }
  }
  return(feature)
}

#------------------------------------------------#
# EXTRACAO DE CARACTERISTICAS                    #
#------------------------------------------------#
features_s <- t(sapply(img, features_shape)) 
features_c <- t(sapply(img, features_color)) 
features_t <- t(sapply(img, features_texture)) 

head(features_s)
head(features_c)
head(features_t)

#------------------------------------------------#
# GERANDO RANKINGS                               #
#------------------------------------------------#


# construindo rankings                          

generate_ranking <- function(features, query){
  # entrada: conjunto de caracteristicas que serao 
  # utilizadas para calculo de distancia e indice 
  # da consulta no vetor de caracteristicas
  # saida: vetor ordenado de posicoes de maneira 
  # crescente de distancias das imagens
  # para a consulta
  
  ## calcular distancia euclidiana de todas as 
  # imagens (representada pelas caracteristicas) 
  # para a consulta
  
  # calculando a distancia dos pontos p e q
  distancia <- dist(features, method = "euclidean")
  distancia <- as.matrix(distancia)
  distancia_interesse <- distancia[,query]
  # print(distancia_interesse)
  
  ## ordenar por menor distancia e retornar as 
  # posicoes das imagens com menor distancia 
  # (mais proximas)
  ranking <- order(distancia_interesse)
  print(ranking)
  return(ranking)
}

#------------------------------------------------#
# RANKINGS DE CARACTERISTICAS                    #
#------------------------------------------------#

rankings_c <- generate_ranking(features_c, consulta)
rankings_t <- generate_ranking(features_t, consulta)
rankings_s <- generate_ranking(features_s, consulta)

#------------------------------------------------#
# COMPARANDO RANKINGS                            #
#------------------------------------------------#

#######################################
#    GRAFICO PRECISAO X TOPK          #
#######################################

k <- 10

# precisao no topk para descritor de cor
p_c <- mapply(precision, 1:k, MoreArgs =  list(gtruth = ground_truth, 
                                               ranking = rankings_c))
# precisao no topk para descritor de textura
p_t <- mapply(precision, 1:k, MoreArgs = list(gtruth = ground_truth, 
                                              ranking = rankings_t))
# precisao no topk para descritor de forma
p_s <- mapply(precision, 1:k, MoreArgs =  list(gtruth = ground_truth, 
                                               ranking = rankings_s))

ps <- data.frame(p_c=p_c, p_t=p_t, p_s=p_s)
ggplot(ps, aes(x = 1:k)) + 
  # Adicionado linha e pontos referentes a precisão 
  geom_point(aes(y = p_c, colour = "Cor")) + 
  geom_line(aes(y = p_c, colour = "Cor")) +
  geom_point(aes(y = p_t, colour = "Textura")) + 
  geom_line(aes(y = p_t, colour = "Textura"), linetype="dashed") +
  geom_point(aes(y = p_s, colour = "Forma")) + 
  geom_line(aes(y = p_s, colour = "Forma"), linetype="dotted") +
  theme_light() +
  # Definindo conteúdo do título do gráfico
  labs(colour = element_blank(), 
       title = "Precisão dos descritores") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Configurando a legenda
  theme(legend.justification = c(1, 1)) +
  theme(legend.box.background = 
          element_rect(colour = "black")) +
  # Configurando o eixo x com legenda e número 
  # de marcações
  scale_x_continuous(name = "Top k", 
                     limits = c(1, k), 
                     breaks = 5 * 1:(k/5), 
                     minor_breaks = NULL) +
  # Configurando o eixo y com legenda e número 
  # de marcações
  scale_y_continuous(name = "", limits = c(0, 1), 
                     breaks = 0.1 * 0:10, 
                     minor_breaks = NULL)

k <- 10

# recall no topk para descritor de cor
r_c <- mapply(recall, 1:k, MoreArgs = 
                list(gtruth = ground_truth, 
                     ranking = rankings_c))
# recall no topk para descritor de textura
r_t <- mapply(recall, 1:k, MoreArgs = 
                list(gtruth = ground_truth, 
                     ranking = rankings_t))
# recall no topk para descritor de forma
r_s <- mapply(recall, 1:k, MoreArgs = 
                list(gtruth = ground_truth, 
                     ranking = rankings_s))

rs <- data.frame(r_c=r_c, r_t=r_t, r_s=r_s)
ggplot(rs, aes(x = 1:k)) + 
  # Adicionado linha e pontos referentes a recall 
  geom_point(aes(y = r_c, colour = "Cor")) + 
  geom_line(aes(y = r_c, colour = "Cor")) +
  geom_point(aes(y = r_t, colour = "Textura")) + 
  geom_line(aes(y = r_t, colour = "Textura"), linetype="dashed") +
  geom_point(aes(y = r_s, colour = "Forma")) + 
  geom_line(aes(y = r_s, colour = "Forma"), linetype="dotted") +
  theme_light() +
  # Definindo conteúdo do título do gráfico
  labs(colour = element_blank(), 
       title = "Revocação dos descritores") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Configurando a legenda
  theme( legend.justification = c(1, 1)) +
  theme(legend.box.background = 
          element_rect(colour = "black")) +
  # Configurando o eixo x com legenda e número 
  # de marcações
  scale_x_continuous(name = "Top k", 
                     limits = c(1, k), 
                     breaks = 5 * 1:(k/5), 
                     minor_breaks = NULL) +
  # Configurando o eixo y com legenda e número 
  # de marcações
  scale_y_continuous(name = "", limits = c(0, 1), 
                     breaks = 0.1 * 0:10, 
                     minor_breaks = NULL)



# Aplicando a interpolação com os valores de precisão
aux_df <- cbind(rs, ps)
l_pr <- NULL
for (desc in 1:3){
  for (i in seq(from = 0.0, to = 1.0, by = 0.1)){
    l_pr <- rbind(l_pr, data.frame(prec = max(aux_df[aux_df[desc] >= i, ][desc+3]), rec = i))
  }
}


# Gerando a média das precisões interpoladas
for (i in seq(from = 0.0, to = 1.0, by = 0.1)){
  l_pr <- rbind(l_pr, data.frame(prec = mean(l_pr[l_pr$rec == i, ]$prec), rec = i))
}

ggplot(l_pr, aes(x = rec)) + 
  # Adicionado linha e pontos 
  geom_point(data=l_pr[1:11,], aes(y = prec, color='Cor')) + 
  geom_line(data=l_pr[1:11,], aes(y = prec, color='Cor')) +
  geom_point(data=l_pr[12:22,], aes(y = prec, color='Textura')) + 
  geom_line(data=l_pr[12:22,], aes(y = prec, color='Textura')) +
  geom_point(data=l_pr[23:33,], aes(y = prec, color='Forma')) + 
  geom_line(data=l_pr[23:33,], aes(y = prec, color='Forma')) +
  geom_point(data=l_pr[34:44,], aes(y = prec, color='Media')) + 
  geom_line(data=l_pr[34:44,], aes(y = prec, color='Media')) +
  theme_light() + 
  # Definindo conteúdo do título do gráfico
  labs(colour = element_blank(), 
       title = "Precisao media interpolada em 11 pontos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Configurando a legenda
  theme( legend.justification = c(1, 1)) +
  theme(legend.box.background = 
          element_rect(colour = "black")) +
  # Configurando o eixo x com legenda e número 
  # de marcações
  scale_x_continuous(name = "Revocacao", 
                     limits = c(0, 1), 
                     breaks = 0.1 * 0:10, 
                     minor_breaks = NULL) + 
  # Configurando o eixo y com legenda e número 
  # de marcações
  scale_y_continuous(name = "Precisao", limits = c(0, 1), 
                     breaks = 0.1 * 0:10, 
                     minor_breaks = NULL)
  
  
  
  
  
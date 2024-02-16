#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao                             #
#                                                                #
# Trabalho Avaliativo 2                                          #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
# - Fernando Augusto Cardoso Candalaft                           #
# - Leonardo Cesar Silva dos Santos                              #
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares   
#----------------------------------------------------------------#
# configure o caminho antes de executar
# configuração da pasta local
setwd("C:\\Users\\ferna\\Documents\\aulas\\01. Mineração de Dados Complexos - Unicamp\\repo\\recup_info\\trabalho 2")
options(warn=-1)
#libs e imports
source("./ranking_metrics.R")
source("./trabalho2_base.R")
library(imager)

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens                 
#----------------------------------------------------------------#

#traz nome das imagens com o path
path_imgs <- list.files(path_plantas, full.names = TRUE);path_imgs
#traz nome das imagens sem o path
name_imgs <- list.files(path_plantas, full.names = FALSE); name_imgs
#da nome para as imagens no path
names(path_imgs) <- name_imgs; name_imgs
#testa o carregamento das imagens
plot(load.image(path_imgs[name_imgs[1]]), axes = FALSE, main = name_imgs[1])
#faz a leitura das imagens com a função do arquvio trabalho2_base.R
imagens <- read_images(path_plantas)


#----------------------------------------------------------------#
# Obtem classe de cada imagem             
#----------------------------------------------------------------#
nome_classes <- get_classes(path_plantas);nome_classes

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
#classifica com 1 todos os arquivos que são da planta relevante
ground_truth_biloba <- get_ground_truth(path_plantas,nome_classes,"biloba");ground_truth_biloba
ground_truth_europaea <- get_ground_truth(path_plantas,nome_classes,"europaea");ground_truth_europaea
ground_truth_ilex <- get_ground_truth(path_plantas,nome_classes,"ilex");ground_truth_ilex
ground_truth_monogyna <- get_ground_truth(path_plantas,nome_classes,"monogyna");ground_truth_monogyna
ground_truth_regia <- get_ground_truth(path_plantas,nome_classes,"regia");ground_truth_regia

#----------------------------------------------------------------#
# Questao 1                               
#----------------------------------------------------------------#

# obtem caracteristicas de cor  

hist_cor_desc <- function(img){
  #separa em RGB
  canal_vermelho <- img[,,1]
  canal_verde <- img[,,2]
  canal_azul <- img[,,3]
  #normaliza 
  canal_vermelho_normalizado <- 255*(canal_vermelho - min(canal_vermelho))/(max(canal_vermelho) - min(canal_vermelho))
  canal_verde_normalizado <- 255*(canal_verde - min(canal_verde))/(max(canal_verde) - min(canal_verde))
  canal_azul_normalizado <- 255*(canal_azul - min(canal_azul))/(max(canal_azul) - min(canal_azul))
  #traz o histograma por RGB
  r <- hist(canal_vermelho_normalizado, plot=F, breaks=0:255)$counts
  g <- hist(canal_verde_normalizado, plot=F, breaks=0:255)$counts
  b <- hist(canal_azul_normalizado, plot=F, breaks=0:255)$counts
  
  return(c(r,g,b))
}

#testando a funcao
img = load.image(path_imgs[name_imgs[1]])
hist_cor_desc(img)


# obtem caracteristicas de textura   
lbp_desc <- function(img) {
  #escala de cinza
  img <- grayscale(img)[,,1,1]
  #normalização
  img_norm <- (img-min(img))/(max(img)-min(img)) * 255
  #aplicação da função do arquivo trabalho2_base.R
  r1 <- lbp(img,1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_uniforme))
}

#testando a  funcao
img = load.image(path_imgs[name_imgs[1]])
lbp_desc(img);lbp_desc

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
  #escalas de cinza
  img <- grayscale(img)[,,1,1]
  img_norm <- (img-min(img))/(max(img)-min(img)) * 255
  #d=1 Média
  momento_media <- momento(img_norm, 1,0, central = T)
  #d=2 Desvio Padrão
  momento_devp <- momento(img_norm, 2,2, central = T)
  #d=3 Obliquidade
  momento_skew <- momento(img_norm, 3,3, central = T)
  #d=4 Curtose
  momento_kurt <- momento(img_norm, 4,4, central = T)
  
  return(c(momento_media, momento_devp, momento_skew, momento_kurt))
}

#----------------------------------------------------------------#
# obtem caracteristicas de cor, textura e forma para todas as imagens e 
# armazena em matrizes onde uma linha representa uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)
features_t <- t(sapply(imagens, lbp_desc))
rownames(features_t) <- names(imagens)
features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)

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

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(consulta_biloba)
mostrarImagemColorida(consulta_europaea)
mostrarImagemColorida(consulta_ilex)
mostrarImagemColorida(consulta_monogyna)
mostrarImagemColorida(consulta_regia)

#-----------------------------#
# construindo rankings                          
# para cada uma das 5 consultas, construa um ranking com base na cor
hist_cor_biloba <- hist_cor_desc(load.image(consulta_biloba))
hist_cor_europaea <- hist_cor_desc(load.image(consulta_europaea)); hist_cor_europaea
hist_cor_ilex <- hist_cor_desc(load.image(consulta_ilex)); hist_cor_ilex
hist_cor_monogyna <- hist_cor_desc(load.image(consulta_monogyna)); hist_cor_monogyna
hist_cor_regia <- hist_cor_desc(load.image(consulta_regia)); hist_cor_regia
#parei aqui
#aqui esta dando Null, pq??
ranking_c_biloba <- get_ranking_by_distance(hist_cor_biloba,features_c); ranking_c_biloba
ranking_c_europaea <- <to-do>
ranking_c_ilex <- <to-do>
ranking_c_monogyna <- <to-do>
ranking_c_regia <- <to-do>

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- <to-do>
ranking_t_europaea <- <to-do>
ranking_t_ilex <- <to-do>
ranking_t_monogyna <- <to-do>
ranking_t_regia <- <to-do>
  
# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- <to-do>
ranking_s_europaea <- <to-do>
ranking_s_ilex <- <to-do>
ranking_s_monogyna <- <to-do>
ranking_s_regia <- <to-do>

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20

analyse_rankings <- function(ranking, ground_truth) {
  <to-do>
}

# analisando rankings gerados com caracteristicas de cor
analyse_rankings(<to-do>)

# analisando rankings gerados com caracteristicas de textura
analyse_rankings(<to-do>)

# analisando rankings gerados com caracteristicas de forma
analyse_rankings(<to-do>)


#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (a) Escolha uma consulta para analisar mais detalhadamente e
# responda: Para essa consulta qual descritor retornou o melhor
# ranking? Lembre-se de analisar visualmente as imagens da classe,
# contextualizando o que foi extraído em cada descritor. Também
# aponte pontos fortes e fracos dos descritores usados que podem
# justificar esse comportamento.
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
# (b) Considerando as 5 consultas definidas, calcule a média das
# precisões médias em top 10. Avaliando essa medida, qual descritor
# obteve melhores rankings? 
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat = <to-do>
  
# gerar novos rankings
ranking_concat_biloba <- <to-do>
ranking_concat_europaea <- <to-do>
ranking_concat_ilex <- <to-do>
ranking_concat_monogyna <- <to-do>
ranking_concat_regia <- <to-do>

# analisando rankings gerados com caracteristicas concatenadas
analyse_rankings(<to-do>)

#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (a) Qual o impacto dessas alterações nas medidas de avaliação
# calculadas?
# 
# 
# (b) Os descritores combinados apresentaram melhores resultados?
# Justifique sua resposta.
# 
# 
# 
# 
# (c) Você acredita que algum dos descritores apresentou maior
# influência na combinação? Justifique sua resposta.
# 
# 
# 
#----------------------------------------------------------------#

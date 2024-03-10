##################################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Codigos da Aula 4 - Tecnicas Avancadas
##################################################################


##################################################################
# Preparando ambiente - workspace
# # Linux
# setwd("~/user/diretorio/INF-0611/aula4/")
# # Windows
# setwd("C:\\user\\diretorio\\INF-0611\\aula4\\")

# Não precisaremos de pacotes novos para a execução deste arquivo


##################################################################
# Agregação de Rankings
##################################################################

# valores das distâncias
metodo1 <- c(1.0, 0.4, 2.3, 5.2, 6.7)
metodo2 <- c(5.0, 1.2, 2.0, 2.4, 8.1)
metodo3 <- c(3.0, 5.4, 2.1, 6.2, 2.7)
metodo4 <- c(2.5, 4.3, 2.6, 1.2, 3.5)

##################################################################
# CombMIN
combmin <- function(...) {
  order(mapply(min, ...))
}

##################################################################
# CombMAX
combmax <- function(...) {
  order(mapply(max, ...))
}

##################################################################
# CombSUM
combsum <- function(...) {
  order(mapply(sum, ...))
}

##################################################################
# Borda Count
bordacount <- function(...) {
  # obtem os rankings
  rankings <- lapply(list(...), rank)
  # calcula a ordem baseada na soma das posições dos rankings
  return(do.call(combsum, rankings))
}

bordacount(metodo1, metodo2, metodo3, metodo4)
combmin(metodo1, metodo2, metodo3, metodo4)
combmax(metodo1, metodo2, metodo3, metodo4)
combsum(metodo1, metodo2, metodo3, metodo4)

##################################################################
# Relevance Feedback 
##################################################################

# incluindo pacotes
library(udpipe)
library(tm)
library(dplyr)
library(tidytext)
library(tokenizers)
library(proxy)

# incluindo o arquivo auxiliar, lembre de configurar o workspace
# setwd("/user/dir/")
source("aula4_auxiliar.R")
data <- processamento_do_texto("oz.txt")
data

q <- "good witch"
query <- tokenize_words(q, strip_punct = TRUE, lowercase = TRUE)
query

##################################################################
# Rocchio method 

# criando o corpus
corpus <- tokenize_words(data$text, strip_punct = TRUE, lowercase = TRUE, 
                         stopwords = stopwords("english"))

# criando a matriz Termo-Documento
vocab <- sort(unique(unlist(corpus)))
tdm <- as.data.frame(sapply(vocab, 
                            function(x) sapply(corpus, 
                            function(y) length(which(x==y)))))
rownames(tdm) <- data$title
head(tdm[,1:10])

# transformando a consulta em um vetor de frequencias
q_txt <- table(query[[1]])
q_txt

# verificando a dimensao da  matriz termo-documento
dim(tdm)

# criando u vetor com a mesma dimensão da matriz termo-documento
# para armazenar a consulta
q_vec <- rep(0, dim(tdm)[2])
names(q_vec) <- colnames(tdm)

# transferindo as frequencias do termos da consulta para o vetor 
# que considera o vocabulario inteiro
q_vec[names(q_txt)] <- as.vector(q_txt)
q_vec["good"]
q_vec["witch"]
q_vec[1:10]

# adicionando a consulta como ultimo documento na 
# matriz termo-documento
tdm_q <- rbind(tdm, q_vec)
tail(tdm[,1:5])
rownames(tdm_q)[25] <- "query"
tail(tdm_q[,1:5])

# computando o ranking 
top <- 5
dists <- simil(tdm_q, "cosine")
dists <- as.matrix(dists)
dists[is.na(dists)] <- 0
ranking_0 <- data$title[order(dists[1:24, 25], 
                              decreasing = TRUE)]
ranking_0
ranking_0[1:top]

# usuário seleciona como relevante dois documentos 
# do topo (o segundo e o quinto)
Cr <- ranking_0[c(2, 5)]
Cr

# consideraremos os demais como não relevantes
Cnr <- data$title[!is.element(data$title, Cr)]
Cnr

# aplicando o metodo de Rocchio
alpha <- 1
beta <- 0.75
gamma <- 0.15

sum_Cr <- (1/length(Cr)) * colSums(tdm[Cr, ])
sum_Cnr <- (1/length(Cnr)) * colSums(tdm[Cnr, ])

q_vec_opt <- alpha * q_vec + beta * sum_Cr - gamma * sum_Cnr

q_vec["good"]
q_vec_opt["good"]

q_vec["witch"]
q_vec_opt["witch"]

q_vec[1:10]
q_vec_opt[1:10]

# computando o novo ranking 
top <- 5
tdm_q <- rbind(tdm, q_vec_opt)
dists <- simil(tdm_q, "cosine")
dists <- as.matrix(dists)
dists[is.na(dists)] <- 0
ranking_opt <- data$title[order(dists[1:24, 25], 
                                decreasing = T)]
# ranking inicial
ranking_0[1:top]
# ranking melhorado
ranking_opt[1:top]

##################################################################
# Clusters de associacao

# obtendo tokens por capitulo
tokens <- unnest_tokens(data, term, text) 
# removendo stopwords
tokens <- tokens %>% 
  filter(!term %in% stop_words$word)

# criando a matriz de termo-documento 
dtf <- document_term_frequencies(tokens, 
                                 term = "term")

# computando as estatisticas: tf, idf, tf-dif,
# tf_bm25 e bm25
chptr_stats <- 
  document_term_frequencies_statistics(dtf, k = 1.2, b=0.75)
chptr_stats <- as.data.frame(chptr_stats)

# recuperando os documentos com o método tf-idf
q_0 <- get_top_docs("tf_idf", chptr_stats, query, 
                    top = 5); q_0

# conjunto de documentos retornados pela consulta 
Dl <- data[is.element(data$title, q_0), ]; Dl

# vocabulário formado apenas pelos subconjunto
# de documentos retornados pela q_0
corpus_l <- tokenize_words(Dl$text, strip_punct = TRUE, lowercase = TRUE, 
                         stopwords = stopwords("english"))

# nova matriz Termo-Documento
vocab_l <- sort(unique(unlist(corpus_l)))
Ml <- as.data.frame(sapply(vocab_l, 
                            function(x) 
                              sapply(corpus_l, 
                                  function(y) length(which(x==y)))))

rownames(Ml) <- Dl$title
Ml <- t(Ml)

# matriz de correlações dos termos
Cl <- Ml %*% t(Ml)
n_termos <- dim(Ml)[1]

# matriz de associação dos termos
for (a in 1:n_termos) {
  for (b in a:n_termos) {
    Cl[a,b] <- Ml[a,] %*% Ml[b,]
    Cl[b,a] <- Cl[a,b]
  }
  Cl[a,a] <- 0
}

# melhorar a consulta com os n termos mais 
# próximos de cada termo da consulta original
n <- 3
# n termos mais próximos de "good"
asc_good <- order(Cl["good", ], decreasing = T)[1:n]
asc_good <- rownames(Cl)[asc_good]; asc_good 

# n termos mais próximos de "witch"
asc_witch <- order(Cl["witch",], decreasing = T)[1:n]
asc_witch <- rownames(Cl)[asc_witch]; asc_witch 

# nova consulta agora contém os termos mais
# próximos aos termos da consulta original
new_query <- c(asc_good, asc_witch)

# adciona os novos termos a consulta
query[[1]] <- unique(c(new_query, query[[1]]))
q_1 <- get_top_docs("tf_idf", chptr_stats, query, top = 5)

# resultado da busca inicial
q_0
# resultado da nova busca
q_1

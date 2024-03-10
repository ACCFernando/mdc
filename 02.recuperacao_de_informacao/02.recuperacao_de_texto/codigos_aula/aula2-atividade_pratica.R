##################################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Atividade Pratica da Aula 2 - Recuperacao de Texto
##################################################################


# Carregando pacotes necessarios 
library(tokenizers)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)

# Carregando arquivos auxiliares
source("ranking_metrics.R")
source("aula2-atividade_pratica-auxiliar.R")

##################################################################
# PROCESSANDO O TEXTO 

# Nesse processamento dos dados, transformamos cada 
# capítulo do livro em um documento
data <- processamento_do_texto("oz.txt")

# Visualizando o primeiro capítulo 
data[1,]

# Obtendo tokens por documento
tokens <- unnest_tokens(data, word, text) 

# Criando a matriz de termo-documento com frequencias
term_freq <- document_term_frequencies(tokens, term = "word")
# Visualizando a matriz de termo-documento
term_freq

# Computando as estatisticas baseadas em frequencias: 
#   tf, idf, tf-dif, tf_bm25 e bm25
docs_stats <- document_term_frequencies_statistics(term_freq, 
                                            k = 1.2, b = 0.75)

# Transformando em data.frame
docs_stats <- as.data.frame(docs_stats)

# Visualizando as estatisticas
head(docs_stats)


##################################################################
# DEFININDO CONSULTA E TOP 
query <- "good witch"
tokens_query <- tokenize_words(query, strip_punct = FALSE, lowercase = FALSE)
top <- 10

##################################################################
# APLICANDO TF-IDF 
ranking_tfidf <- get_ranking_by_stats(stat_name = "tf_idf", 
                                      docs_stats, tokens_query[[1]])

top_tf_idf <- ranking_tfidf[1:top,'doc_id']

##################################################################
# APLICANDO BM25 
ranking_bm25 <- get_ranking_by_stats(
                    stat_name = "bm25", docs_stats, 
                    tokens_query[[1]])
top_bm25 <- ranking_bm25[1:top, 'doc_id']

##################################################################
# COMPARANDO RANKINGS 
top_tf_idf
top_bm25

index <- jaccard_index(top_tf_idf, 
                       top_bm25, top); index
kt <- kendall_tau(ranking_tfidf$doc_id, 
                  ranking_bm25$doc_id); kt


######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Leonardo Cesar Silva dos Santos                                #
#   - Fernando Augusto Cardoso Candalaft                             #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(tokenizers)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)

# Configure aqui o diretório onde se encontram os arquivos do trabalho
wdir <- "/home/rstudio/workspace/mdc/02_recuperacao_informacao_inf_0611/test01/"
setwd(wdir)

# Carregando os arquivos auxiliares
source("ranking_metrics.R", encoding = "UTF-8")
source("trabalho1_base.R", encoding = "UTF-8")



######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", 
                     "XX-Text [[:alnum:]]", 
                     "Article_0", 
                     convertcase=TRUE, 
                     remove_stopwords=FALSE)
# Visualizando os documentos (apenas para debuging)
head(docs)
tail(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", 
                        "XX-Find [[:alnum:]]", 
                        "Query_0", 
                        convertcase=TRUE, 
                        remove_stopwords=FALSE)
# Visualizando as consultas (apenas para debuging)
head(queries)
tail(queries)

# Exemplo de acesso aos tokens de uma consulta
q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header=TRUE)

# Visualizando os ground_truths (apenas para debuging)
head(ground_truths)

# Exemplo de acesso vetor de ground_truth da consulta 1:
ground_truths[1,]

# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
names(ground_truths)[ground_truths[1,]==1]

# Computando a matriz de termo-documento
term_freq <- document_term_frequencies(docs, term ="word")
term_freq

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, 
                                                                 k=1.2, 
                                                                 b=0.75))
# Visualizando as estatísticas da coleção (apenas para debuging)
head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  # Dica: você pode acessar a segunda coluna da query a partir de $word ou [["word"]]
  ranking <- get_ranking_by_stats(stat_name=stat_name, 
                                  stats,
                                  query$word)
  # Visualizando o ranking (apenas para debuging)
  print(head(ranking, n=5))
  
  # Calculando a precisão
  # Dica: para calcular a precisão, revocação e utilizar a função plot_prec_e_rev,
  # utilize a coluna doc_id do ranking gerado (você pode acessar com $doc_id)
  p <- precision(ground_truth, ranking$doc_id, top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, top, text)
  
}

# Definindo a consulta 1 
# Dicas para as variáveis consulta1 e n_consulta1:
# Para a variável consulta1, você deve acessar os tokens de uma consulta, conforme
# o exemplo da linha 52 e 53.
# Para a variável n_consulta1, você deve informar o número da consulta. Por exemplo,
# se usar a Query_01 como consulta, n_consulta1 deve receber o valor 1.

# Checando o formato dos IDs das queries
unique(queries$doc_id)

consulta1 <- queries[queries$doc_id == "Query_023",]; consulta1
n_consulta1 <- 23



## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ], 
                   docs_stats, "tf_idf", 
                   top=20, "Ranking para o modelo tf-idf")
# Precisão:  0.15 	Revocação:  0.6

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1, ground_truths[n_consulta1, ], 
                   docs_stats, "bm25", 
                   top=20, "Ranking para o modelo bm25")
# Precisão:  0.2 	Revocação:  0.8

# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_055",]; consulta2
n_consulta2 <- 55

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2, ground_truths[n_consulta2, ], 
                   docs_stats, "tf_idf", 
                   top=20, "Ranking para o modelo tf-idf - consulta 2")
# Precisão:  0.05 	Revocação:  0.5 

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2, ground_truths[n_consulta2, ], 
                   docs_stats, "bm25", 
                   top=20, "Ranking para o modelo bm25 - consulta 2")
# Precisão:  0.05 	Revocação:  0.5

######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################

# Para as consultas analisadas (ids 23 e 55) o modelo BM25 teve uma perfomance
# melhor em termos de precisão e revocação. Portanto, para as consultas escolhidas
# o modelo BM25 teve uma perfomance melhor. 
# Porém, para termos uma avaliação mais crítica é necessário avaliarmos nossas métricas
# sobre todas as consultas disponíveis e utilizar outros métodos de avaliação entre 
# rankings, como por exemplo a interpolação média de 11 pontos de um método contra o outro.

######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
head(queries_proc)
tail(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc, term ="word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, 
                                                                      k=1.2, 
                                                                      b=0.75))


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_023",]; consulta1_proc
n_consulta1_proc <- 23
# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], 
                   docs_stats, "tf_idf", 
                   top=20, "Ranking para o modelo tf-idf - consulta 1")
# Precisão:  0.15 	Revocação:  0.6 

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], 
                   docs_stats, "bm25", 
                   top=20, "Ranking para o modelo bm25 - consulta 1")
# Precisão:  0.2 	Revocação:  0.8

# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_055",]; consulta2_proc
n_consulta2_proc <- 55
# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], 
                   docs_stats, "tf_idf", 
                   top=20, "Ranking para o modelo tf-idf - consulta 2")
# Precisão:  0.05 	Revocação:  0.5 

# Resultados para a consulta 1 e bm25
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], 
                   docs_stats, "bm25", 
                   top=20, "Ranking para o modelo bm25 - consulta 2")
# Precisão:  0.05 	Revocação:  0.5

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################

# Para as consultas utilizadas na questão (ids 23 e 55) não houve melhora de 
# perfomance tanto em precisão quanto em revocação. Portanto, a remoção de stopwords
# não trouxe melhora para estes casos. 
# Novamente, se quisermos avaliar com mais rigor o possível benefício da remoção de 
# stopwords para o contexto em questão é necessário avaliarmos os nossos modelos 
# sobre todo o conjunto de teste/ground truth.


######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
                              full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
                               full.names = TRUE)
file.copy(from=plots.png.paths, to="~/Desktop/")
######################################################################

































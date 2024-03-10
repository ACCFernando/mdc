##################################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Codigos da Aula 2 - Recuperacao de Texto
##################################################################


##################################################################
# Preparando ambiente - workspace
# # Linux
# setwd("~/user/diretorio/INF-0611/aula2/")
# # Windows
# setwd("C:\\user\\diretorio\\INF-0611\\aula1\\")

# # Instalando os pacotes
# install.packages("tm")
# install.packages("dplyr")
# install.packages("tokenizers")
# install.packages("janeaustenr")
# install.packages("tidytext")
# install.packages("tidyverse")

##################################################################
# Processamento de Texto
##################################################################


##################################################################
# Analise Lexica - Exemplo
# carregando o pacote 
library(tokenizers)

# criando um exemplo simples, cada palavra sera um documento
text <- "...Mas a saudade e isto mesmo; e o passar e repassar das 
memorias antigas..."

# criando os tokens 
tokens <- tokenize_words(text, strip_punct = FALSE, lowercase = FALSE); tokens

##################################################################
# Remocao de stopwords - Exemplo
# carregando o pacote
library(tm)

# visualizando a lista de stopwords do portugues
head(stopwords("portuguese"))

# removendo stopwords do portugues
tokens_sw <- tokenize_words(text, strip_punct = FALSE, lowercase = FALSE, 
                            stopwords = stopwords("portuguese")); tokens_sw

# Exemplo 2 -- Stopwords do ingles
# criando um corpus onde cada frase e um documento
text_en <- c("Because I could not stop for Death -", 
             "He kindly stopped for me -", 
             "The  Carriage held but just Ourselves -", 
             "and Immortality")

# removendo stopwords do ingles
tokens_en_sw <- tokenize_words(text_en, strip_punct = FALSE, lowercase = FALSE, 
                               stopwords = stopwords("english")); tokens_en_sw

##################################################################
# Stemming (Portugues) - Exemplo
# aplicando tokenizacao seguida de stemming
stemming_c <- tokenize_word_stems(text, language  = "portuguese")

# visualizando os tokens apos o stemming
stemming_c

##################################################################
# Stemming (Ingles) - Exemplo
# aplicando stemming
stems_en <- tokenize_word_stems(text_en, language  = "english") 

# visualizando documentos apos o stemming
stems_en

##################################################################
# Normalizacao - Exemplo
# criando um documento simples
text <- "...Mas a saudade          e isto    mesmo; e o passar e 
repassar das memorias  antigas..."

# removendo espacos em branco, pontuacao e convertendo para letras minusculas
tokens_norm <- tokenize_words(text, strip_punct = TRUE, lowercase = TRUE); tokens_norm


##################################################################
# Representacao e Armazenamento de Documentos
##################################################################


##################################################################
# Matriz Termo-Documento - Exemplo
# carregando a biblioteca
library(tm)

# preparando os documentos e corpus
chico_txt <- c("Nao sei", "so sei que foi assim so")
chico_docs <- tokenize_words(chico_txt, 
                             strip_punct = TRUE, lowercase = TRUE); chico_docs

# extraindo vocabulario
vocab <- sort(unique(unlist(chico_docs))); vocab

# visualizando a matriz termo-documento
chico_tdm <- as.data.frame(t(sapply(vocab, 
                                    function(x) 
                                      sapply(chico_docs, 
                                             function(y) as.numeric(is.element(x, y))))))

colnames(chico_tdm) <- 1:length(chico_txt); chico_tdm

##################################################################
# indice Invertido - Exemplo 
# criando os documentos
documents <- c("To be or not to be. I am what I am.", 
               "To do is to be. To be is to do.", 
               "I think therefore I am. Let it be.")

# criando o corpus
docs <- tokenize_words(documents, strip_punct = TRUE, lowercase = TRUE); docs

# extraindo vocabulario
vocab <- sort(unique(unlist(docs))); vocab

# criando lista de documentos que contem cada termo
index <- sapply(vocab, function(x) which(sapply(docs,
                                                function(y) is.element(x, y))))


# organizando em um data.frame
index = data.frame(docs = I(index))
index

# # Exemplo mais simples do indice invertido (extra)
# # criando um data frame vazio para guardar o indice
# index <- data.frame(term = character(), docs = numeric())
# # para cada termo do vocabulario 
# for (t in c(1:length(vocab))) {
#   line <- c()
#   # verificamos a quais documentos o termo pertence 
#   for (d in c(1:length(docs))) {
#     if (is.element(vocab[t], docs[[d]])) {
#       line<-append(line, d)
#     }
#   }
#   # adicionando uma nova linha ao data.frame
#   line <- data.frame(term = vocab[t], docs = I(list(line)))
#   index <- rbind(index, line)
# }

# # visualizando o indice invertido criado
# index

##################################################################
# Modelos de Recuperacao de Texto
##################################################################


##################################################################
# Modelo de Espaco Vetorial - Exemplo 
# carregando os pacotes
library(tm)
library(proxy)

# vetores dos termos Jealous e Gossip (consultas)
jealous <- c(0, 1)
gossip <- c(1, 0)  

# definindo vetores dos livros de acordo com os termos 
# Jealous e Gossip
SaS <- c(2, 10)
PaP <- c(0, 7)
WH <- c(6, 11)

# criando uma matriz com os vetores anteriores
matriz_vetores <- rbind(jealous, gossip, SaS, PaP, WH)
matriz_vetores

# calculando a similaridade entre todos os documentos e consultas
similaridades <- simil(matriz_vetores, "cosine")
similaridades

##################################################################
# tf - Exemplo
# carregando os pacotes
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)

# carregando livros Jane Austin
book_words <- austen_books() 

# obtendo tokens por livro
book_words <- unnest_tokens(book_words, word, text) 

# contando a frequencia dos tokens em cada livro
book_words <- count(book_words, book, word, sort = TRUE)

# agrupando a frequencia dos tokens em cada categoria 
# (livro correspondente)
total_words <- group_by(book_words, book) 
total_words

# obtem numero total de palavras de cada uma das categorias
total_words <- summarize(total_words, total = sum(n))
total_words

# junta o numero de palavras com a tabela de frequencia de termos
book_words <- left_join(book_words, total_words)
book_words

# visualizando histogramas com os valores de tf no eixo x e o
# numero de termos com esse valor de tf no eixo y  
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) + xlim(NA, 0.0009) +
  labs(x = "tf") + labs(y = "numero de termos") +
  facet_wrap(~book, ncol = 2, scales = "free_y")



##################################################################
# tf-idf - Exemplo
# calculando e vincula as colunas tf, idf e tf_idf  no dataframe
book_words <- bind_tf_idf(book_words, word, book, n)

# ordenando os termos por maior tf_idf
book_words <- book_words[order(book_words$tf_idf, 
                               decreasing = TRUE),]
book_words

# visualizando os 5 termos com maior tf_idf por livro, no eixo x 
# temos os valores de tf_idf e no eixo y temos os termos
book_words %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% top_n(5) %>% ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") + coord_flip()


##################################################################
# BM25 - Exemplo
# carregando os pacotes
library(dplyr)
library(udpipe)
library(tidytext)

# instanciando os  documentos
docs <- c("white audi 2.5 car", "black shoes from office",
          "new mobile iphone 7", "audi tyres audi a3",
          "nice audi bmw toyota corolla")

# vinculando titulo do documento com texto do documento
data <- tibble(doc_id = c(1:5), text = docs)

# obtendo tokens por documento
tokens <- unnest_tokens(data, word, text) 

# criando a matriz de termo-documento com frequencias
term_freq <- document_term_frequencies(tokens, term = "word")
# visualizando a matriz de termo-documento
head(term_freq)

# computando as estatisticas baseadas em frequencias: 
#   tf, idf, tf-dif, tf_bm25 e bm25
docs_stats <- document_term_frequencies_statistics(
  term_freq, k = 1.2, b = 0.75)

# transformando em data.frame
docs_stats <- as.data.frame(docs_stats)

# visualizando a estatisticas bm25
head(docs_stats[,c("doc_id", "term", "bm25")])

############################################
#             PROCESSANDO O TEXTO          #
############################################
# Referência: https://cran.r-project.org/web/packages/corpus/vignettes/corpus.html

# Esse processamento é específico para essa base de dados
processamento_do_texto <- function(arquivo) {
	# carregando o texto do livro
	lines <- readLines(arquivo, encoding = "UTF-8")

	# determinando o inicio efetivo do livro
	half_title <- grep("^THE WONDERFUL WIZARD OF OZ", lines)

	# determinando inicio dos capitulos que comecam depois de um numero
	chapter <- grep("^[[:space:]]*[[:digit:]]+\\.", lines)
	chapter <- chapter[chapter > half_title]

	# obtendo o nome dos capitulos
	start <- c(1, chapter + 1) # + 1 para pular o titulo
	end <- c(chapter - 1, length(lines))
	text <- mapply(function(s, e) paste(lines[s:e], collapse = "\n"), start, end)

	# removendo espacos brancos em sequencia
	text <- trimws(text)
	# descartando paginas iniciais
	text <- text[-1]

	# obtendo os titulos dos capitulos e removendo as numeracoes ("1.", "2.", etc.)
	title <- sub("^[[:space:]]*[[:digit:]]+[.][[:space:]]*", "", lines[chapter])
	# removendo espacos brancos em sequencia
	title <- trimws(title)

	# vinculando titulo do capitulo com texto do capitulo
	data <- tibble(title, text)
	
	data$title <- as.factor(data$title) 
	data$title <- ordered(data$title, levels=unique(data$title))
	
	return (data)
}

############################################
#              APLICANDO MODELOS           #
############################################

scores_termo_consulta <- function(docs_stats, 
                                  query_term, stat) {
  # seleciona as linhas que contém o termo da consulta
	term_stat <- docs_stats[docs_stats$term == query_term,]
	# seleciona as colunas com o id do documento e a estatística 
	term_stat <- term_stat[,c("doc_id", stat)]
	# renomeando a colunas
	stat_col_name <- paste(stat, query_term, sep="-")
	colnames(term_stat) <- c("doc_id", stat_col_name)
	return(term_stat)
}

# - stat_name deve ser um desses valores:
#     "tf", "idf", "tf_idf", "tf_bm25" OU "bm25"
# - docs_stats deve ser um data.frame obtido através da 
#     conversão dos resultados da função 
#     document_term_frequencies_statistics
# - tokens_query deve ser uma lista simples de tokens
get_ranking_by_stats <- function(stat_name, docs_stats, 
                                 tokens_query) {
  tokens_query <- unique(tokens_query)
  # para cada elemento x de tokens_query, executamos 
  # uma chamada da função scores_termo_consulta e 
  # obtemos uma lista com os data.frame's resultantes
  # da execução da função scores_termo_consulta
  stats <- lapply(tokens_query, 
                  function(x) scores_termo_consulta(
                        docs_stats, x, stat_name)
                  )
  # aplicamos a função reduce que faz um full_join das
  stats <- stats %>% reduce(full_join, by = "doc_id")
  # substituindo valores NA (faltantes) por 0
	stats[is.na(stats)] <- 0
	# somando os valores da estatística para cada documento 
	# multiplica os tf_idfs dos termos 'good' e 'witch'
	aux <- stats[,-1] %>% 
	  mutate(SUM = Reduce(`+`, .))
	stats$SUM <- aux$SUM
	# ordenando os documentos pelo valor da soma das 
	# estatísticas de cada termo da consulta
	positions <- order(stats$SUM, decreasing = TRUE)
	ranking <- stats[positions,]
	return(ranking)
}


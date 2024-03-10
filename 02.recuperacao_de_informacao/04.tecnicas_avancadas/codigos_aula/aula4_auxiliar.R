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

    return (data)
}

scores_termo_consulta <- function(estatistica_docs, termo_query, stat) {
  term_values <- estatistica_docs[estatistica_docs$term == termo_query,]
  term_values <- term_values[,c("doc_id", stat)]
  names(term_values)[names(term_values) == stat] <- paste(stat,termo_query, sep="-")
  return(term_values)
}

get_top_docs <- function(stat_name, chapter_words_stats, tokens_query, top) {
  # buscando os valores das estatisticas para o primeiro termo da consulta
  stats <- scores_termo_consulta(chapter_words_stats, tokens_query[[1]][1], stat_name)
  n_tokens <- length(tokens_query[[1]])
  # tratando os tokens seguintes
  for (i in 2:n_tokens) {
    t <- tokens_query[[1]][i]
    # consultando as estatisticas 
    aux <- scores_termo_consulta(chapter_words_stats, t, stat_name)
    # unindo as estatisticas em um unico data-frame
    stats <- full_join(stats, aux, by="doc_id")
  }
  # substituindo valores NA (faltantes) por 0
  stats[is.na(stats)] <- 0
  # somando estatisticas dos termos
  stats$SUM <- rowSums(stats[2:(n_tokens+1)])
  # ordena pela soma
  ranking <- stats[order(stats$SUM, decreasing = TRUE),1]
  
  return(ranking[1:top])
}
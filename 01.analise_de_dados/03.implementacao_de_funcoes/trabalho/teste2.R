########################################
# Teste 2         
# Nome(s): 
#   - Leonardo Cesar Silva dos Santos
#   - Fernando Augusto Cardoso Candalaft

########################################

## 1 - Agrupamento

groupsum <- function(df, group_col, col_sum) {
  
  summary_infos <- tapply(df[col_sum], df[group_col], sum)
  s_infos_df <- data.frame(
    cidade = names(summary_infos),
    chuva  = unname(summary_infos)
  )
  
  return(s_infos_df)
}

##### Exemplos no PDF:
dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
chuvas <- data.frame(cidade, dia, chuva); chuvas
groupsum(chuvas, "cidade", "chuva")

## 2 - Binario para Decimal

binToDec <- function(...) {
  
  dec_vec <- NULL
  
  vecs <- list(...)
  for (vec in vecs) {
    n_vec <- length(vec)
    
    i <- 1
    dec <- 0
    while (i <= n_vec) {
      dec <- dec + vec[i] * 2^(n_vec - i)
      i <- i + 1
    }
    
    dec_vec <- c(dec_vec, dec)
  }
  
  return(dec_vec)
}

##### Exemplos no PDF:
binToDec(c(1, 0, 1))
binToDec(c(1, 0))
binToDec(c(0, 0, 1), c(1, 1))
binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de Palavras

wordCount <- function(word, text) {
  
  lower_text <- tolower(text)
  lower_text <- gsub("[.,!?]", "", lower_text)[[1]]
  
  str_vec <- strsplit(lower_text, split=" ")
  
  return( sum( as.integer( sapply(str_vec, function(w) {w == word}) ) ) )
  
}

##### Exemplos no PDF:
text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
wordCount("rato", text)
wordCount("roma", text)
text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
wordCount("outra", text)
wordCount("vaca", text)
wordCount("malhada", text)
text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
wordCount("liga", text)
wordCount("ligasse", text)

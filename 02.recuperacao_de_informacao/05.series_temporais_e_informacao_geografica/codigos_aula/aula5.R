##################################################################
# Mineracao de Dados Complexos -- MDC 2024
# Recuperacao de Informacao
# Codigos da Aula 5 - Series Temporais
##################################################################

##################################################################
# Series Temporais 
##################################################################

library(dtw)

# carrega os dados do Cepagri
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.table(con, header = FALSE, 
                      fill = TRUE, sep = ";", 
                      col.names = names)

# remove as as medições incompletas
cepagri <- cepagri[!is.na(cepagri$sensa), ]

# transforma as temperaturas em numeric
cepagri$temp <- as.numeric(cepagri$temp)

# transforma o horário em POSIXct
cepagri$horario <- as.POSIXct(as.character(cepagri$horario),
                              format = '%d/%m/%Y-%H:%M', 
                              tz = "America/Sao_Paulo")

# gera uma coluna com datas
cepagri$data <- as.Date(cepagri$horario)

# gera lista com as (144) medições de 
# temperaturas de cada dia
base <- tapply(cepagri$temp, cepagri$data,
               function(x){x})

# dado um objeto dtw, obtem a distância
getDistance <- function(obj) {obj$distance}

# busca na base a query com id dado
search <- function(id, base) {
  query <- base[[id]]
  base <- base[names(base) != id]
  obj <- lapply(base, dtw, query, 
                distance.only = TRUE)
  dist <- sapply(obj, getDistance)
  dist[order(dist)]
}

# retorna os t objetos mais próximos da query
top <- function(query, base, t = 1) {
  head(search(query, base), t)
}

# retorna o gráfico comparando a query com o
# objeto mais próximo dele na base
best <- function(query, base) {
  b <- names(top(query, base))
  alignment <- dtw(base[[query]], base[[b]], keep = TRUE)
  title <- paste("Query (", query, ") x Best (", b, ")", 
                 sep = "")
  dtwPlotTwoWay(alignment, xlab = "Series Index",
                ylab = "Value", main = title)
  legend("topleft", c("Query", "Best"), 
         pch = 15, col = c("black", "red"))
}

top("2017-06-30", base, 15)
top("2015-09-19", base, 18)

best("2017-06-30", base)
best("2015-09-19", base)
best("2018-11-15", base)

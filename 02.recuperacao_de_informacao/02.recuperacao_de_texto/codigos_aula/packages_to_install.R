##################################################################
# Mineração de Dados Complexos -- MDC 2024
# Recuperação de Informação
# Pacotes extras para a Aula 2 - Recuperação de Texto
# 
# 
# 
# Abra este arquivo com o Rstudio e execute cada 
# linha separadamente. Caso encontre algum erro entre 
# em contato com os monitores. Lembre-se de indicar
# aos monitores o seu sistemaa operacional e a versão 
# do R instalada. 
##################################################################

# Instalando o pacote 
install.packages("tm")
# Carregando o pacote
library(tm)
# Mensagem esperada:
# Loading required package: NLP

# !!!! Dependência no Ubuntu libxml2-dev. 
# Instale via no terminal do sistema:
# sudo apt install libxml2-dev
# Em seguida repita o comando install.packages no Rstudio

# Instalando o pacote 
install.packages("dplyr")
# Carregando o pacote
library(dplyr)
# Mensagem esperada:
# 
# Attaching package: ‘dplyr’
# 
# The following objects are masked from ‘package:stats’:
# 
#   filter, lag
# 
# The following objects are masked from ‘package:base’:
# 
#   intersect, setdiff, setequal, union


# Instalando o pacote 
install.packages("udpipe")
# Carregando o pacote
library(udpipe)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
install.packages("tokenizers")
# Carregando o pacote
library(tokenizers)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
install.packages("tidytext")
# Carregando o pacote
library(tidytext)
# Mensagem esperada:
# (Sem mensagem)

# Instalando o pacote 
install.packages("tidyverse")
# Carregando o pacote
library(tidyverse)
# Mensagem esperada:
# ── Attaching packages ──────────────────────────────────────── tidyverse 1.3.0 ──
# ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
# ✓ tibble  3.0.5     ✓ stringr 1.4.0
# ✓ tidyr   1.1.2     ✓ forcats 0.5.1
# ✓ readr   1.4.0     
# ── Conflicts ─────────────────────────────────────────── tidyverse_conflicts() ──
# x dplyr::filter() masks stats::filter()
# x dplyr::lag()    masks stats::lag()

# !!!! Dependência no Ubuntu r-cran-curl. 
# Instale via no terminal do sistema:
# sudo apt install r-cran-curl
# Em seguida repita o comando install.packages no Rstudio

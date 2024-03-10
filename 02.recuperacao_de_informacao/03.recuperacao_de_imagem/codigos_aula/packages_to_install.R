##################################################################
# Mineração de Dados Complexos -- MDC 2024
# Recuperação de Informação
# Pacotes extras para a Aula 3 - Recuperação de Imagens
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
install.packages("imager")
# Carregando o pacote
library(imager)
# Mensagem esperada:
# Loading required package: magrittr

# Attaching package: ‘imager’

# The following object is masked from ‘package:magrittr’:

#     add

# The following objects are masked from ‘package:stats’:

#     convolve, spectrum

# The following object is masked from ‘package:graphics’:

#     frame

# The following object is masked from ‘package:base’:

#     save.image

# Dependências no Ubuntu
# sudo apt-get install libfftw3-dev libtiff5-dev
# sudo apt-get install libx11-dev  

# Dependência no MacOS
# Aplicativo xQuartz disponível em https://www.xquartz.org/


# Instalando o pacote 
install.packages("OpenImageR")
# Carregando o pacote
library(OpenImageR)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
install.packages("gridExtra")
# Carregando o pacote
library(gridExtra)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("EBImage")
# Carregando o pacote
library(EBImage)
# Mensagem esperada:
# Attaching package: ‘EBImage’
#
# The following objects are masked from ‘package:OpenImageR’:
#  
#   readImage, writeImage
#
# The following objects are masked from ‘package:imager’:
#  
#   channel, dilate, display, erode, resize, watershed


# Instalando o pacote 
install.packages("raster")
# Carregando o pacote
library(raster)
# Mensagem esperada:
# Loading required package: sp
#
# Attaching package: ‘sp’
#
# The following object is masked from ‘package:imager’:
#
#     bbox

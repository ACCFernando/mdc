########################################
#
# Trabalho final (Teste 3) - Análise de Dados    
# Nome(s): 
#   - Leonardo Cesar Silva dos Santos
#   - Fernando Augusto Cardoso Candalaft
#
########################################

setwd("/home/rstudio/workspace/mdc/01_analise_dados_inf_0612/test03_final")

### Libs
library(tidyverse) # Data manipulation
library(zoo) # zoo: S3 Infrastructure for Regular and Irregular Time Series (Z's Ordered Observations)
library(ggplot2) # Plot
library(patchwork)

### Leitura e Tratamento dos dados
cepagri_df <- read.csv("./cepagri.csv", header=FALSE, sep=";"); head(cepagri_df)
#           dt_dhora vl_temp   vl_vel_vento vl_umidade vl_stermica
# 1 02/03/2014-19:08    23.7         59.3       77.1        22.6
# 2 02/03/2014-19:18    23.4         59.1       77.9        22.3
# 3 02/03/2014-19:28    23.2         56.7       78.9        22.1
# 4 02/03/2014-19:38    23.0         55.4       79.2        21.9
# 5 02/03/2014-19:48    22.8         52.6       79.7        21.7
# 6 02/03/2014-19:58    22.6         62.6       80.7        21.5

# Descricao das colunas:
# - string informando o dia e hora em que o dado foi coletado; 
# - a temperatura (em °C); 
# - a velocidade do vento (em km/h); 
# - a umidade (em porcentagem); 
# - a sensação térmica (em °C)
names(cepagri_df) <- c("dt_dhora", 
                       "vl_temp", 
                       "vl_vel_vento", 
                       "vl_umidade", 
                       "vl_stermica"); head(cepagri_df)

# Dimensao dos dados
dim(cepagri_df)

### Tratamento dos dados

# Checando descricao macro dos dados
summary(cepagri_df)
# dt_dhora           vl_temp           vl_vel_vento      vl_umidade      vl_stermica   
# Length:518094      Length:518094      Min.   :  0.00   Min.   :  0.00   Min.   :-8.20  
# Class :character   Class :character   1st Qu.: 10.50   1st Qu.: 56.30   1st Qu.:16.60  
# Mode  :character   Mode  :character   Median : 19.10   Median : 72.50   Median :19.90  
#                                       Mean   : 22.77   Mean   : 68.96   Mean   :19.86  
#                                       3rd Qu.: 31.00   3rd Qu.: 83.40   3rd Qu.:23.90  
#                                       Max.   :143.60   Max.   :100.00   Max.   :99.90  
#                                       NA's   :40864    NA's   :40864    NA's   :74270 
#

# Obs: Os valores da coluna "vl_temp" estao representados com o tipo "character" 
# quando deveriam ser numericos
cepagri_df$vl_temp <- as.numeric(cepagri_df$vl_temp) # NAs introduced by coercion 

dim(cepagri_df[is.na(cepagri_df$vl_temp) 
               & is.na(cepagri_df$vl_temp)
               & is.na(cepagri_df$vl_vel_vento)
               & is.na(cepagri_df$vl_umidade)
               & is.na(cepagri_df$vl_stermica),]) # Quantidade de valores nulos

# Removendo todos as linhas com valores nulos nas colunas listadas abaixo
cepagri_df <- cepagri_df[
                           !is.na(cepagri_df$dt_dhora)
                         & !is.na(cepagri_df$vl_temp) 
                         & !is.na(cepagri_df$vl_temp)
                         & !is.na(cepagri_df$vl_vel_vento)
                         & !is.na(cepagri_df$vl_umidade) #& !is.na(cepagri_df$vl_stermica)
                         , ]

summary(cepagri_df) # Sensacao termica com valor maximo proximo de 100° e 33406 NA's
# dt_dhora            vl_temp       vl_vel_vento      vl_umidade      vl_stermica   
# Length:477230      Min.   : 4.60   Min.   :  0.00   Min.   :  0.00   Min.   :-8.20  
# Class :character   1st Qu.:18.60   1st Qu.: 10.50   1st Qu.: 56.30   1st Qu.:16.60  
# Mode  :character   Median :21.50   Median : 19.10   Median : 72.50   Median :19.90  
#                    Mean   :21.99   Mean   : 22.77   Mean   : 68.96   Mean   :19.86  
#                    3rd Qu.:25.50   3rd Qu.: 31.00   3rd Qu.: 83.40   3rd Qu.:23.90  
#                    Max.   :38.10   Max.   :143.60   Max.   :100.00   Max.   :99.90  
#                                                                      NA's   :33406

# Avaliando os valores de minimo e maximo de sensacao termica
# Minimos
head(cepagri_df[cepagri_df$vl_stermica < 0, ]) # Temperatura relativamente baixa, alta umidade e alta velocidade do vento ~ baixa senscao termica
tail(cepagri_df[cepagri_df$vl_stermica < 0, ])

# Maximo
head(cepagri_df[cepagri_df$vl_stermica == 99.9, ]) # Erro de medicao na coluna sensacao termica
#                dt_dhora vl_temp vl_vel_vento vl_umidade vl_stermica
# 114706 26/05/2016-07:10    13.8         16.9       94.5        99.9
# 114850 27/05/2016-07:10    14.3         19.7       98.1        99.9
# 114994 28/05/2016-07:10    16.1         47.1       91.5        99.9
# 115138 29/05/2016-07:10    14.8         31.3       94.8        99.9
# 115282 30/05/2016-07:10    14.1         16.2       96.6        99.9
# 115426 31/05/2016-07:10    15.7         15.5      100.0        99.9

cepagri_df[!is.na(cepagri_df$vl_stermica) & (cepagri_df$vl_stermica == 99.9), 5] <- NA

summary(cepagri_df)
# dt_dhora            vl_temp       vl_vel_vento      vl_umidade      vl_stermica   
# Length:477230      Min.   : 4.60   Min.   :  0.00   Min.   :  0.00   Min.   :-8.20  
# Class :character   1st Qu.:18.60   1st Qu.: 10.50   1st Qu.: 56.30   1st Qu.:16.60  
# Mode  :character   Median :21.50   Median : 19.10   Median : 72.50   Median :19.90  
#                    Mean   :21.99   Mean   : 22.77   Mean   : 68.96   Mean   :19.84  
#                    3rd Qu.:25.50   3rd Qu.: 31.00   3rd Qu.: 83.40   3rd Qu.:23.90  
#                    Max.   :38.10   Max.   :143.60   Max.   :100.00   Max.   :34.70  
#                                                                      NA's   :33548 

# Verificando valores repetidos
consecutive <- function(vector, k=2) {
  
  n <- length(vector)
  
  result <- logical(n)
  for (i in k:n) {
    if (all(vector[(i-k+1):i] == vector[i])) {
      result[(i-k+1):i] <- TRUE
    }
  }
  return(result)
}

# Verificando temperaturas consecutivas em um dia 
# (As chances de nao termos nenhuma variacao de temperatura em um dia sao muito baixas)
any(consecutive(cepagri_df$vl_temp, k=1440))
repeated_temps <- consecutive(cepagri_df$vl_temp, k=144) # Dia com erro na coleta
repeated_temps[1:10]
# Datas com repeticao consecutiva de temperaturas
cepagri_df[, 1] <- as.POSIXct(cepagri_df[, 1],
                               format='%d/%m/%Y-%H:%M', 
                               tz='America/Sao_Paulo')
cepagri_df$dt_dhora[1:10]
hour(cepagri_df$dt_dhora[1:10])
day(cepagri_df$dt_dhora[1:10])
month(cepagri_df$dt_dhora[1:10])
year(cepagri_df$dt_dhora[1:10])

cepagri_df$cd_hora <- hour(cepagri_df$dt_dhora)
cepagri_df$cd_dia <- day(cepagri_df$dt_dhora)
cepagri_df$cd_ano <- year(cepagri_df$dt_dhora)
cepagri_df$cd_mes <- month(cepagri_df$dt_dhora)

cepagri_df <- cepagri_df[, -which(names(cepagri_df) == "dt_dhora")]
dim(cepagri_df)

temp_error_dates <- cepagri_df[repeated_temps, 1]
length(temp_error_dates) # Quantidade de datas com temperatura repetida: 10003
temp_error_dates[1:145]
cepagri_df[cepagri_df$dt_dhora %in% temp_error_dates[1:145], ]
length(unique(as.Date(temp_error_dates)))
# Vamos olhar somente as horas ao longo dos dias, nao olhando para os intervalos de coleta dentro
# do periodo de 1 hora

# Verificando dados duplicados
cepag_df = data.frame(cepagri_df)
head(cepag_df)

dim(cepag_df) # 477230
sum(duplicated(cepag_df[, 1:8])) # 32558

dim(distinct(cepag_df, 
             vl_temp, 
             vl_vel_vento, 
             vl_umidade, 
             vl_stermica, 
             cd_hora, 
             cd_dia, 
             cd_ano, 
             cd_mes)) # 444672

# Remocao de linhas duplicadas
cepag_df <- distinct(cepag_df, 
                     vl_temp, 
                     vl_vel_vento, 
                     vl_umidade, 
                     vl_stermica, 
                     cd_hora, 
                     cd_dia, 
                     cd_ano, 
                     cd_mes); dim(cepag_df) # 444672

sum(consecutive(cepag_df$vl_temp, k=144)) # 430
head(cepag_df[consecutive(cepag_df$vl_temp, k=144), ])

filter_values <- !consecutive(cepag_df$vl_temp, k=144)
dim(cepag_df[filter_values, ]) # 444242 = 444672 - 430

cepag_df <- cepag_df[filter_values, ]
dim(cepag_df)

head(cepag_df)

### Analise dos dados
summary(cepag_df)

cepag_df$cd_hora <- as.factor(cepag_df$cd_hora)
cepag_df$cd_dia <- as.factor(cepag_df$cd_dia)
cepag_df$cd_mes <- as.factor(cepag_df$cd_mes)
cepag_df$cd_ano <- as.factor(cepag_df$cd_ano)

# Checando a evolucao temporal de algumas variaveis
# Temperatura
agg_infos_df <- cepag_df %>%
  group_by(cd_ano, cd_mes) %>%
  summarise(mean_temp=mean(vl_temp, na.rm=TRUE), 
            median_temp=median(vl_temp, na.rm=TRUE),
            max_temp=max(vl_temp, na.rm=TRUE), 
            min_temp=min(vl_temp, na.rm=TRUE),
            
            mean_umid=mean(vl_umidade, na.rm=TRUE), 
            median_umid=median(vl_umidade, na.rm=TRUE),
            max_umid=max(vl_umidade, na.rm=TRUE), 
            min_umid=min(vl_umidade, na.rm=TRUE),
            
            mean_vento=mean(vl_vel_vento, na.rm=TRUE), 
            median_vento=median(vl_vel_vento, na.rm=TRUE),
            max_vento=max(vl_vel_vento, na.rm=TRUE), 
            min_vento=min(vl_vel_vento, na.rm=TRUE),
            
            mean_stermica=mean(vl_stermica, na.rm=TRUE), 
            median_stermica=median(vl_stermica, na.rm=TRUE),
            max_stermica=max(vl_stermica, na.rm=TRUE), 
            min_stermica=min(vl_stermica, na.rm=TRUE)); head(agg_infos_df)

unique(agg_infos_df$cd_ano) # Checando os anos presentes
# Checando os anos com todos os meses 
for (y in unique(agg_infos_df$cd_ano)) {
  print(y)
  print(length(unique(cepagri_df[cepagri_df$cd_ano == y, 9])))
}

agginfos_18_23_df <- agg_infos_df[agg_infos_df$cd_ano %in% c(2018, 2019, 2020, 2021, 2022, 2023), ]
agginfos_18_23_df <- agginfos_18_23_df[order(agginfos_18_23_df$cd_ano, agginfos_18_23_df$cd_mes), ]
print(agginfos_18_23_df, n=24)

# Temperatura media ao longo dos meses
ggplot(agginfos_18_23_df, aes(x=cd_mes, y=mean_temp, group=cd_ano, color=as.factor(cd_ano))) +
  geom_point() +
  geom_line() +
  labs(x="Mês", y="Temperatura média", 
       title="Evolução da Temperatura por Mês", 
       color="Ano") +
  theme_minimal()
# Todos os anos parecem seguir a mesma tendencia ao longo dos meses, 
# exceto 2020, que possui picos nos meses 7 e 8

# Temperatura maxima ao longo dos meses
ggplot(agginfos_18_23_df, aes(x=cd_mes, y=max_temp, group=cd_ano, color=as.factor(cd_ano))) +
  geom_point() +
  geom_line() +
  labs(x="Mês", y="Temperatura máxima", 
       title="Evolução da Temperatura por Mês", 
       color="Ano") +
  theme_minimal()
# Mesmo possuindo a maior media nos meses 7 e 8, a temperatura maxima no ano de 
# 2020 teve tendencia de queda ate o mes 9, com valores mais baixos com relacao 
# aos outros anos nos meses 7, 8 e 9

cepag_df[is.na(cepag_df$vl_stermica), ]

# Comparacao entre temperatura e umidade
# (A umidade do ar interfere na ocorrência das chuvas, uma vez que, 
# em zonas com alta umidade relativa do ar, como de climas equatoriais e 
# tropicais, as chuvas tendem a ocorrer com maior frequência e volume)
plot1 <- ggplot(agginfos_18_23_df, 
                aes(x=cd_mes, 
                    y=mean_temp, 
                    group=cd_ano, color=as.factor(cd_ano))) +
        geom_point() +
        geom_line() +
        labs(x="Mês", y="Temperatura média", 
             title="Temperatura por Mês", 
             color="Ano") +
        theme_minimal()

plot2 <- ggplot(agginfos_18_23_df, 
                aes(x=cd_mes, 
                    y=mean_umid, 
                    group=cd_ano, color=as.factor(cd_ano))) +
  geom_point() +
  geom_line() +
  labs(x="Mês", y="Umidade média", 
       title="Umidade por Mês", 
       color="Ano") +
  theme_minimal()

plot3 <- ggplot(agginfos_18_23_df, 
                aes(x=cd_mes, 
                    y=mean_vento, 
                    group=cd_ano, color=as.factor(cd_ano))) +
  geom_point() +
  geom_line() +
  labs(x="Mês", y="Velocidade média do vento", 
       title="Velocidade do Vento por Mês", 
       color="Ano") +
  theme_minimal()

# Plot
plot1 + plot2 + plot_layout(ncol=2)
# Tivemos uma umidade nula nos meses 7, 8 e 9 para o ano de 2020, o que pode indicar a alta
# temperatura do mesmo ano nos meses 7 e 8 comparado com os outros anos

# Velocidade do Vento
plot3

is.na(agginfos_18_23_df$mean_stermica)
sum(is.na(agginfos_18_23_df$mean_stermica))
agginfos_18_23_df[!complete.cases(agginfos_18_23_df$mean_stermica), c(1, 2, 12, 13, 14)]

head(agginfos_18_23_df)
head(cepag_df)

# Relacao Velocidade vento, Sensacao termica, Temperatura e Umidade
plot4 <- ggplot(agginfos_18_23_df, 
                aes(x=cd_mes, 
                    y=mean_stermica, 
                    group=cd_ano, color=as.factor(cd_ano))) +
  geom_point() +
  geom_line() +
  labs(x="Mês", y="Sensação térmica média", 
       title="Sensação térmica por Mês", 
       color="Ano") +
  theme_minimal()

plot1 + plot2 + plot3 + plot4 + plot_layout(ncol=2)
# Aparentemente os periodos com baixa umidade indicam uma temperatura media 
# maior e consequentemente uma sensacao termica mais elevada, em particular 
# o ano de 2020. 
# Por outro lado, o ano de 2019 foi o ano com percepcao de temperatura mais baixa
# dado que seguiu a tendencia de outros anos, mas tendo pico de baixa temperatura 
# media no mes 7 e um periodo de 12 meses de estabilidade com relacao a umidade 
# e velocidade do vento

# Comparando 2019 x 2023 (período antes e após a pandemia)
# (Note que 2023 possui valores faltantes com relacao aos meses 7 em diante para a 
# coluna vl_stermica, então como ela se mostrou um reflexo da coluna vl_temp, nao vamos utilizar
# a mesma na analise)

plot5 <- ggplot(cepag_df[cepag_df$cd_ano == 2019, ], 
                aes(x=cd_mes, 
                    y=vl_temp, 
                    group=cd_mes)) +
  geom_boxplot() +
  geom_line(agginfos_18_23_df[agginfos_18_23_df$cd_ano == 2019, ], 
            mapping=aes(x=cd_mes, y=mean_temp, group=1), 
            color="blue") + 
  scale_fill_brewer(palette="Pastel1") +
  labs(x="Mês", y="Temperatura", 
       title="Distribuição de temperatura - 2019")

plot6 <- ggplot(cepag_df[cepag_df$cd_ano == 2023, ], 
                aes(x=cd_mes, 
                    y=vl_temp, 
                    group=cd_mes)) +
  geom_boxplot() +
  geom_line(agginfos_18_23_df[agginfos_18_23_df$cd_ano == 2023, ], 
            mapping=aes(x=cd_mes, y=mean_temp, group=1), 
            color="blue") + 
  scale_fill_brewer(palette="Pastel1") +
  labs(x="Mês", y="Temperatura", 
       title="Distribuição de temperatura - 2023")

plot5 + plot6 + plot_layout(ncol=1)

names(agginfos_18_23_df)
cepag19_df = data.frame(agginfos_18_23_df[agginfos_18_23_df$cd_ano == 2019, 1:14])
cepag23_df = data.frame(agginfos_18_23_df[agginfos_18_23_df$cd_ano == 2023, 1:14])

dim(cepag19_df)
dim(cepag23_df)

cepag19_df <- cepag19_df[order(cepag19_df$cd_ano, cepag19_df$cd_mes), ]
cepag23_df <- cepag23_df[order(cepag23_df$cd_ano, cepag23_df$cd_mes), ]

print(cepag19_df)
print(cepag23_df)

agg_diff_df <- merge(cepag23_df, cepag19_df, c("cd_mes"), all.x=TRUE)
agg_diff_df <- agg_diff_df[order(agg_diff_df$cd_mes), ]

print(agg_diff_df[, c(1, 2)])
names(agg_diff_df)

agg_diff_df$diff_mean_temp <- agg_diff_df$mean_temp.x - agg_diff_df$mean_temp.y
agg_diff_df$diff_max_temp <- agg_diff_df$max_temp.x - agg_diff_df$max_temp.y
agg_diff_df$diff_min_temp <- agg_diff_df$min_temp.x - agg_diff_df$min_temp.y

agg_diff_df$diff_mean_umid <- agg_diff_df$mean_umid.x - agg_diff_df$mean_umid.y
agg_diff_df$diff_max_umid <- agg_diff_df$max_umid.x - agg_diff_df$max_umid.y
agg_diff_df$diff_min_umid <- agg_diff_df$min_umid.x - agg_diff_df$min_umid.y

agg_diff_df <- agg_diff_df[, c("cd_mes",
                               "diff_mean_temp", 
                               "diff_max_temp",
                               "diff_min_temp",
                               "diff_mean_umid", 
                               "diff_max_umid",
                               "diff_min_umid")]

agg_diff_df

plot7 <- ggplot(agg_diff_df, 
                aes(x=cd_mes, 
                    y=diff_mean_temp, 
                    group=1)) +
        geom_line(aes(color="Temperatura")) +
        geom_line(agg_diff_df, 
                  mapping=aes(x=cd_mes, y=0, group=1),
                  color="gray", linetype="dashed") +
        geom_line(aes(y=diff_mean_umid, color="Umidade")) +
        labs(x="Mês", y="Diferença", 
             title="Relação de temperatura e umidade média: 2023 vs 2019")
        # scale_y_continuous(
        #   name="Diferença de temperatura média",
        #   sec.axis = sec_axis(~.*1, name="Diferença de umidade média")) +
        #   scale_fill_brewer(palette="Pastel1") +

plot7
# A temperatura média entre 2023 e 2019 foi bastante semelhante, mesmo com uma
# diferença consideravel em termos de umidade, no caso, 2019 com muito mais
# umidade ao longo do ano do que 2019

# Histograma de temperatura e umidade
plot8 <- ggplot() +
        geom_density(cepag_df[cepag_df$cd_ano == 2019, ],
                     mapping=aes(x=vl_temp, color="2019")) +
        geom_density(cepag_df[cepag_df$cd_ano == 2020, ],
                     mapping=aes(x=vl_temp, color="2020")) +
        geom_density(cepag_df[cepag_df$cd_ano == 2021, ],
                     mapping=aes(x=vl_temp, color="2021")) +
        geom_density(cepag_df[cepag_df$cd_ano == 2022, ],
                     mapping=aes(x=vl_temp, color="2022")) +
        geom_density(cepag_df[cepag_df$cd_ano == 2023, ],
                     mapping=aes(x=vl_temp, color="2023")) +
        # geom_density(color="blue") +
        labs(x="Temperatura", y="Distribuição", 
             title="Dist. de temperaturas observadas")
plot8

plot9 <- ggplot() +
  geom_density(cepag_df[cepag_df$cd_ano == 2019, ],
               mapping=aes(x=vl_temp, color="2019")) +
  geom_density(cepag_df[cepag_df$cd_ano == 2023, ],
               mapping=aes(x=vl_temp, color="2023")) +
  # geom_density(color="blue") +
  labs(x="Temperatura", y="Distribuição", 
       title="Dist. de temperaturas observadas")
plot9

plot10 <- ggplot() +
  geom_density(cepag_df[cepag_df$cd_ano == 2019, ],
               mapping=aes(x=vl_umidade, color="2019")) +
  geom_density(cepag_df[cepag_df$cd_ano == 2020, ],
               mapping=aes(x=vl_umidade, color="2020")) +
  geom_density(cepag_df[cepag_df$cd_ano == 2021, ],
               mapping=aes(x=vl_umidade, color="2021")) +
  geom_density(cepag_df[cepag_df$cd_ano == 2022, ],
               mapping=aes(x=vl_umidade, color="2022")) +
  geom_density(cepag_df[cepag_df$cd_ano == 2023, ],
               mapping=aes(x=vl_umidade, color="2023")) +
  # geom_density(color="blue") +
  labs(x="Umidade", y="Distribuição", 
       title="Dist. de umidades observadas")
plot10

plot11 <- ggplot() +
  geom_density(cepag_df[cepag_df$cd_ano == 2019, ],
               mapping=aes(x=vl_umidade, color="2019")) +
  geom_density(cepag_df[cepag_df$cd_ano == 2023, ],
               mapping=aes(x=vl_umidade, color="2023")) +
  # geom_density(color="blue") +
  labs(x="Umidade", y="Distribuição", 
       title="Dist. de umidades observadas")
plot11

plot8 + plot9 + plot10 + plot11 + plot_layout(ncol=2)

# Funcao para calcular a area estimada de uma distribuicao
get_dist_area <- function(ano, base_col) {
  
  filtered_vec <- cepag_df[cepag_df$cd_ano == ano, base_col]
  cepag_filtered_df <- filtered_vec
  distribution <- density(cepag_filtered_df)
  
  area <- integrate(function(x) approx(distribution$x, 
                                       distribution$y, 
                                       xout = x)$y, 
                    min(filtered_vec), 
                    max(filtered_vec))$value
  
  return(area)
  
}

get_dist_area(2019, "vl_temp")
get_dist_area(2019, "vl_umidade")

get_dist_area(2023, "vl_temp")
get_dist_area(2023, "vl_umidade")
# Apesar de valores diferentes, os mesmos ainda sao muito proximo, mostrado que de 
# modo geral 2023 e 2019 tiveram um comportamento semelhante em uma visao macro
# apesar de uma certa diferenca quando olhamos mensalmente


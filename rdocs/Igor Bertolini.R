# Carregando o pacote necessário
library(readr)

# Lendo os dados
setwd("C:/ESTAT/projeto-fantasma/")
getwd()
dados <- read.csv("C:/ESTAT/projeto-fantasma/banco/banco_final.csv")

# Visualizando as primeiras linhas dos dados
head(dados)
library(dplyr)

##1) Número de lançamentos a cada década por formato de lançamento;

str(dados)
library(ggplot2)
library(lubridate)
dados$date_aired <- as.Date(dados$date_aired)
ggplot(dados, aes(x = date_aired, fill = format)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Data de Exibição", y = "Contagem", fill = "Formato", title = "Número de Lançamentos por Formato ao Longo do Tempo")
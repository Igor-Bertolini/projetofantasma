# Carregando o pacote necessário
library(readr)

# Lendo os dados
setwd("C:/ESTAT/projeto-fantasma/")
getwd()
dados <- read.csv("C:/ESTAT/projeto-fantasma/banco/banco_final.csv")

# Visualizando as primeiras linhas dos dados
head(dados)
library(dplyr)
#-------------------------------------------------------------------------------------------------------#
##1) Número de lançamentos a cada década por formato de lançamento;

str(dados)
library(ggplot2)
library(lubridate)
dados$date_aired <- as.Date(dados$date_aired)
ggplot(dados, aes(x = date_aired, fill = format)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Data de Exibição", y = "Contagem", fill = "Formato", title = "Número de Lançamentos por Formato ao Longo do Tempo")

#--------------------------------------------------------------------------------------------------------#
##2) Variação da nota IMDB por temporada dos episódios;
library(ggplot2)
ggplot(dados, aes(x=title, y=imdb, group=season, color=season)) +
  geom_line() +labs(título="Variação da nota IMDB por temporada dos episódios",
                    x="Episódio",
                    y="Nota IMDB",
                    color="Temporada") +
  theme_minimal()
#---------------------------------------------------------------------------------------------------------#
## 3) Top 3 terrenos mais frequentes pela ativação da armadilha;
library(dplyr)

# Encontre os 3 terrenos mais frequentes
terrenos_frequentes <- dados %>%
  count(setting_terrain) %>%
  arrange(desc(n)) %>%
  head(3)

# Filtrar os dados para apenas esses terrenos
dados_filtrados <- dados %>%
  filter(setting_terrain %in% terrenos_frequentes$setting_terrain)

# Analisando a relação entre o terreno e a ativação da armadilha pela primeira vez

analise <- dados_filtrados %>%
  group_by(setting_terrain,set_a_trap) %>%
  summarise(Count = n(), .groups = "drop")

print(analise)

#para saber se há relacao, teste do qui-quadrado
library(chisq.posthoc.test)
tabela <- table(dados$setting_terrain, dados$set_a_trap)
resultado <- chisq.test(tabela)
print(resultado)
#portanto, nao há relacao entre o terreno e a ativação da armadilha pela primeira vez

#-----------------------------------------------------------------------------------------------------#
# 4) Relação entre as notas IMDB e engajamento;
library(ggplot2)
correlacao <- cor(dados$imdb, dados$engagement, method = "pearson")
print(paste("A correlação entre as notas IMDB e o engajamento é", round(correlacao, 2)))
ggplot(dados, aes(x = imdb, y = engagement)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Relação entre as notas IMDB e o engajamento",
       x = "Notas IMDB",
       y = "Engajamento")

#----------------------------------------------------------------------------------------------------#
#5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.
library(ggplot2)
df_long <- reshape2::melt(dados, id.vars = "engagement", measure.vars = c("daphnie_va", "velma_va", "shaggy_va", "scooby_va", "fred_va"))
ggplot(df_long, aes(x = variable, y = engagement)) +
  geom_boxplot() +
  labs(title = "Variação da nota de engajamento pelo personagem",
       x = "Personagem",
       y = "Nota de Engajamento")

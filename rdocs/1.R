dados <- read.csv("C:/ESTAT/projeto-fantasma/banco/banco_final.csv")

# Visualizando as primeiras linhas dos dados
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )
head(dados)

library(dplyr)

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


#-------------------------------------------------------------------------------------------------------#

##1) Número de lançamentos a cada década por formato de lançamento;

str(dados)

library(ggplot2)

library(lubridate)

dados <- dados %>%
  mutate(decade = floor(year(date_aired) / 10) * 10)

# Agora, vamos contar o número de lançamentos em cada década
contagem_por_decada <- dados %>%
  group_by(decade, format) %>%
  summarise(n_lancamentos = n())


unique(dados$format)

# Criando o gráfico
ggplot(contagem_por_decada) +
  aes(x = decade, y = n_lancamentos, group = format, colour = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Número de lançamentos") +
  scale_colour_manual(values = estat_colors,
                      labels = c("Serie" = "Série",
                                 "CrossOver" = "CrossOver",
                                 "Movie" = "Filme")) +
  scale_x_continuous(breaks = seq(min(contagem_por_decada$decade), max(contagem_por_decada$decade), by = 10)) +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")



#--------------------------------------------------------------------------------------------------------#

##2) Variação da nota IMDB por temporada dos episódios;


# Carregando a biblioteca necessária
library(ggplot2)


# Filtrando os dados
dados_filtrados <- dados %>%
  filter(season %in% c("1", "2", "3", "4"))

# Criando o boxplot
ggplot(dados_filtrados) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "imdb") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")


# Carregando a biblioteca necessária
library(dplyr)

# Filtrando os dados
dados_filtrados <- dados %>%
  filter(season %in% c("1", "2", "3", "4"))

# Calculando as estatísticas
estatisticas <- dados_filtrados %>%
  summarise(
    Media = mean(imdb, na.rm = TRUE),
    Desvio_Padrao = sd(imdb, na.rm = TRUE),
    Variancia = var(imdb, na.rm = TRUE),
    Minimo = min(imdb, na.rm = TRUE),
    Primeiro_Quartil = quantile(imdb, 0.25, na.rm = TRUE),
    Mediana = median(imdb, na.rm = TRUE),
    Terceiro_Quartil = quantile(imdb, 0.75, na.rm = TRUE),
    Maximo = max(imdb, na.rm = TRUE)
  )

# Imprimindo as estatísticas
print(estatisticas)





#---------------------------------------------------------------------------------------------------------#


## 3) Top 3 terrenos mais frequentes pela ativação da armadilha;


library(stringr)
library(dplyr)
library(forcats)
library(ggplot2)

# Traduzindo os nomes dos terrenos
dados$setting_terrain <- recode(dados$setting_terrain, "Forest" = "Floresta", "Urban" = "Urbano")

# Identificando os top 3 terrenos mais usados
top_terrenos <- dados %>%
  count(setting_terrain) %>%
  arrange(desc(n)) %>%
  head(3)

# Filtrando os dados para incluir apenas os top 3 terrenos
dados_filtrados <- dados %>%
  filter(setting_terrain %in% top_terrenos$setting_terrain)

# Contando a frequência de ativação da armadilha para cada terreno
contagem_por_terreno <- dados_filtrados %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(n = n(), .groups = "drop")

# Calculando as porcentagens
contagem_por_terreno <- contagem_por_terreno %>%
  mutate(percent = n / sum(n) * 100)

# Criando o gráfico
ggplot(contagem_por_terreno, aes(x = reorder(setting_terrain, -n), y = n, fill = trap_work_first)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Terreno", y = "Contagem", fill = "Armadilha ativada pela primeira vez") +
  theme_estat() +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.25)






#-----------------------------------------------------------------------------------------------------#


# 4) Relação entre as notas IMDB e engajamento;
library(ggplot2)

# Carrega a biblioteca
library(ggplot2)


# Calcula a correlação
correlacao <- cor(dados$imdb, dados$engagement, method = "pearson")
print(paste("A correlação entre as notas IMDB e o engajamento é", round(correlacao, 2)))

# Realiza o teste de correlação
teste_correlacao <- cor.test(dados$imdb, dados$engagement, method = "pearson")
print(teste_correlacao)

# Plota o gráfico com eixos invertidos
ggplot(dados, aes(y = imdb, x = engagement)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Relação entre o engajamento e as notas IMDB",
       y = "Notas IMDB",
       x = "Engajamento")
ggsave("disp_bi.pdf", width = 158, height = 93, units = "mm")


# Carrega a biblioteca
library(dplyr)

# Calcula as estatísticas
estatisticas <- dados %>%
  summarise(
    Media_IMDB = mean(imdb, na.rm = TRUE),
    Desvio_Padrao_IMDB = sd(imdb, na.rm = TRUE),
    Variancia_IMDB = var(imdb, na.rm = TRUE),
    Minimo_IMDB = min(imdb, na.rm = TRUE),
    Primeiro_Quartil_IMDB = quantile(imdb, 0.25, na.rm = TRUE),
    Mediana_IMDB = median(imdb, na.rm = TRUE),
    Terceiro_Quartil_IMDB = quantile(imdb, 0.75, na.rm = TRUE),
    Maximo_IMDB = max(imdb, na.rm = TRUE),
    Media_Engajamento = mean(engagement, na.rm = TRUE),
    Desvio_Padrao_Engajamento = sd(engagement, na.rm = TRUE),
    Variancia_Engajamento = var(engagement, na.rm = TRUE),
    Minimo_Engajamento = min(engagement, na.rm = TRUE),
    Primeiro_Quartil_Engajamento = quantile(engagement, 0.25, na.rm = TRUE),
    Mediana_Engajamento = median(engagement, na.rm = TRUE),
    Terceiro_Quartil_Engajamento = quantile(engagement, 0.75, na.rm = TRUE),
    Maximo_Engajamento = max(engagement, na.rm = TRUE)
  )

# Exibe as estatísticas
print(estatisticas)



#----------------------------------------------------------------------------------------------------#


library(tidyr)

#5 Gráfico relacionando Personagens que capturam o monstro e Engajamento

# Carregando a biblioteca necessária
library(tidyverse)

# Reestruturando os dados
dados_long <- dados %>%
  pivot_longer(
    cols = c(daphnie_va, fred_va, velma_va, shaggy_va, scooby_va, caught_not, caught_other), # Adicione as colunas relevantes aqui
    names_to = "character",
    values_to = "value"
  )

# Renomeando os valores na coluna de personagens
dados_long$character <- gsub("_va", "", dados_long$character)

# Substituindo "shaggy" por "salsicha"
dados_long$character <- gsub("shaggy", "salsicha", dados_long$character)

# Substituindo "caught_not" por "Nenhum" e "caught_other" por "Outros"
dados_long$character <- recode(dados_long$character, "caught_not" = "Nenhum", "caught_other" = "Outros")

# Filtrando os dados
dados_filtrados <- dados_long %>%
  filter(!is.na(value))

# Criando o boxplot
ggplot(dados_filtrados) +
  aes(x = character, y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem", y = "Engajamento") +
  theme_estat()
ggsave("boxplot_engajamento_personagem.pdf", width = 158, height = 93, units = "mm")


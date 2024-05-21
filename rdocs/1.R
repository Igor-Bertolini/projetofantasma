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

# Carregando a biblioteca necessária
library(dplyr)

# Filtrando os dados
dados_filtrados <- dados %>%
  filter(season %in% c("1", "2", "3", "4"))

# Carregando a biblioteca necessária
library(ggplot2)

# Carregando a biblioteca necessária
library(dplyr)


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







#---------------------------------------------------------------------------------------------------------#


## 3) Top 3 terrenos mais frequentes pela ativação da armadilha;


library(dplyr)


# Analisando a relação entre o terreno e a ativação da armadilha pela primeira vez




analise <- dados_filtrados %>%
  
  
  group_by(setting_terrain,set_a_trap) %>%
  
  
  summarise(Count = n(), .groups = "drop")




print(analise)




#para saber se há relacao, teste do qui-quadrado


library(chisq.posthoc.test)


tabela <- table(dados$setting_terrain, dados$set_a_trap)


resultado <- chisq.test(tabela)

print(tabela)
print(resultado)

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

# Criando o gráfico de barras
ggplot(contagem_por_terreno) +
  aes(x = setting_terrain, y = n, fill = trap_work_first) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Terreno", y = "Frequência de ativação da armadilha") +
  scale_fill_discrete(labels = c("Falso", "Verdadeiro")) +
  theme_estat()

  



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


library(tidyr)

# Primeiro, certifique-se de que os dados estão no formato correto
dados <- dados %>%
  pivot_longer(
    cols = c(daphnie_va, velma_va, shaggy_va, scooby_va, fred_va),
    names_to = "personagem",
    values_to = "nota_engajamento"
  )

# Em seguida, filtre os dados para incluir apenas os casos em que o monstro foi capturado
dados <- dados %>%
  filter(monster_name == 1)

# Agora, você pode criar o gráfico
ggplot(dados) +
  aes(x = data, y = nota_engajamento, color = personagem) +
  geom_line() +
  labs(x = "Data", y = "Nota de Engajamento", color = "Personagem") +
  theme_estat()


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
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")


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

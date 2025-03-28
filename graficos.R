library(ggplot2)
library(patchwork)
library(tidyverse)


ggraficos <- "c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/graficos/"

#2014

tabua_se_masc_2014 <- data.frame(idade=factor(c('0-4','5-9',
                                                '10-14',
                                                '15-19','20-24','25-29',
                                                '30-34','35-39','40-44',
                                                '45-49','50-54','55-59',
                                                '60-64','65-69','70-74','75-79', '80w'),
                                              levels = c('0-4','5-9',
                                                         '10-14','15-19',
                                                         '20-24','25-29',
                                                         '30-34','35-39',
                                                         '40-44','45-49',
                                                         '50-54','55-59',
                                                         '60-64','65-69',
                                                         '70-74','75-79',
                                                         '80w')),
                                 populacao=c(87567, 92108, 97028, 107137, 99862, 
                                             96885, 89686, 79136, 68924, 62633, 
                                             50654, 39424, 31061, 24009, 16173, 
                                             10587, 11051))

tabua_se_masc_2014$sexo <- "masculino"
tabua_se_masc_2014$populacao <- tabua_se_masc_2014$populacao*(-1)

tabua_se_fem_2014 <- data.frame(idade=factor(c('0-4','5-9',
                                               '10-14',
                                               '15-19','20-24','25-29',
                                               '30-34','35-39','40-44',
                                               '45-49','50-54','55-59',
                                               '60-64','65-69','70-74','75-79', '80w'),
                                             levels = c('0-4','5-9',
                                                        '10-14','15-19',
                                                        '20-24','25-29',
                                                        '30-34','35-39',
                                                        '40-44','45-49',
                                                        '50-54','55-59',
                                                        '60-64','65-69',
                                                        '70-74','75-79',
                                                        '80w')),
                                populacao=c(83766, 88583, 94174, 104802, 102134, 102497, 97312, 87548, 
                                            77032, 69532, 57467, 45831, 37217, 28558, 21020, 15122, 18495))


tabua_se_fem_2014$sexo <- "feminino"


dados2014 <- rbind(tabua_se_fem_2014,tabua_se_masc_2014)


#razão de sexo 2014 sergipe

rz_2014 <- data.frame(idade=factor(c('0-4','5-9',
                                     '10-14',
                                     '15-19','20-24','25-29',
                                     '30-34','35-39','40-44',
                                     '45-49','50-54','55-59',
                                     '60-64','65-69','70-74','75-79', '80w'),
                                   levels = c('0-4','5-9',
                                              '10-14','15-19',
                                              '20-24','25-29',
                                              '30-34','35-39',
                                              '40-44','45-49',
                                              '50-54','55-59',
                                              '60-64','65-69',
                                              '70-74','75-79',
                                              '80w')))

rz_2014$razao <- abs(tabua_se_masc_2014$populacao)/tabua_se_fem_2014$populacao
rz_2014$rotulo <- round(rz_2014$razao,3)

rz_14 <- ggplot(rz_2014, aes(x = idade, y = razao, group = 1)) +
  geom_line() +  # Plotando a linha
  geom_point()+
  geom_text(aes(label = rotulo), vjust = -0.8, size = 10, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  theme_bw()+
  theme(text = element_text(colour = "black"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        # plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20))


# 

ggsave(paste0(ggraficos,'rz14.pdf'),
       plot = rz_14,
       width = 20.42, 
       height = 8.25,
       units = "in")



#2023

tabua_se_masc_2023 <- data.frame(idade=factor(c('0-4','5-9',
                                                '10-14',
                                                '15-19','20-24','25-29',
                                                '30-34','35-39','40-44',
                                                '45-49','50-54','55-59',
                                                '60-64','65-69','70-74','75-79', '80w'),
                                              levels = c('0-4','5-9',
                                                         '10-14','15-19',
                                                         '20-24','25-29',
                                                         '30-34','35-39',
                                                         '40-44','45-49',
                                                         '50-54','55-59',
                                                         '60-64','65-69',
                                                         '70-74','75-79',
                                                         '80w')),
                                 populacao=c(86412, 86816, 88249, 92206, 
                                             97630, 101524, 94970, 92556, 84880, 73592, 63929, 
                                             55503, 42946, 31795, 22876, 14952, 13838))

tabua_se_masc_2023$sexo <- "masculino"
tabua_se_masc_2023$populacao <- tabua_se_masc_2023$populacao*(-1)

tabua_se_fem_2023 <- data.frame(idade=factor(c('0-4','5-9',
                                               '10-14',
                                               '15-19','20-24','25-29',
                                               '30-34','35-39','40-44',
                                               '45-49','50-54','55-59',
                                               '60-64','65-69','70-74','75-79', '80w'),
                                             levels = c('0-4','5-9',
                                                        '10-14','15-19',
                                                        '20-24','25-29',
                                                        '30-34','35-39',
                                                        '40-44','45-49',
                                                        '50-54','55-59',
                                                        '60-64','65-69',
                                                        '70-74','75-79',
                                                        '80w')),
                                populacao=c(82473, 83021, 84835, 89974, 97506, 
                                            104205, 101909, 101654, 95062, 84037, 74038, 
                                            64968, 51738, 39869, 30335, 21026, 25123))

tabua_se_fem_2023$sexo <- "feminino"


dados2023 <- rbind(tabua_se_fem_2023,tabua_se_masc_2023)



#razão de sexo 2014 sergipe

rz_2023 <- data.frame(idade=factor(c('0-4','5-9',
                                     '10-14',
                                     '15-19','20-24','25-29',
                                     '30-34','35-39','40-44',
                                     '45-49','50-54','55-59',
                                     '60-64','65-69','70-74','75-79', '80w'),
                                   levels = c('0-4','5-9',
                                              '10-14','15-19',
                                              '20-24','25-29',
                                              '30-34','35-39',
                                              '40-44','45-49',
                                              '50-54','55-59',
                                              '60-64','65-69',
                                              '70-74','75-79',
                                              '80w')))

rz_2023$razao <- abs(tabua_se_masc_2023$populacao)/tabua_se_fem_2023$populacao
rz_2023$rotulo <- round(rz_2023$razao,3)

rz_23 <- ggplot(rz_2023, aes(x = idade, y = razao, group = 1)) +
  geom_line() +  # Plotando a linha
  geom_point()+
  geom_text(aes(label = rotulo), vjust = -0.8, size = 10, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  theme_bw()+
  theme(text = element_text(colour = "black"),
                  axis.title = element_blank(),
                  legend.title = element_blank(),
                  # plot.title = element_text(hjust = 0.5, face = "bold"),
                  plot.caption = element_text(hjust = 0.5, size = 11),
                  legend.position = "bottom",
                  legend.text = element_text(size = 20),
                  axis.text.y = element_text(size = 20),
                  axis.title.y = element_text(size = 20),
                  axis.title.x = element_text(size = 20),
                  axis.text.x = element_text(size = 20))



ggsave(paste0(ggraficos, 'rz23.pdf'),
       plot = rz_23,
       width = 20.42, 
       height = 8.25,
       units = "in")



#pirmide etária 

dados2014$ano <- 2014
dados2023$ano <- 2023


dados2014$perc <- abs(dados2014$populacao)/ sum(abs(dados2014$populacao))*100
dados2023$perc <- abs(dados2023$populacao)/ sum(abs(dados2023$populacao))*100

dados2014$perc <- ifelse(dados2014$sexo == "masculino", dados2014$perc*(-1), dados2014$perc)
dados2023$perc <- ifelse(dados2023$sexo == "masculino", dados2023$perc*(-1), dados2023$perc)
# Juntando ambos os anos
dados_total <- rbind(dados2014,dados2023)


piramides <- ggplot(dados_total, aes(x = idade, y = perc, fill = interaction(ano, sexo))) +
  # Barra para a população masculina (negativa)
  geom_bar(data = dados_total[dados_total$sexo == "masculino", ], 
           aes(y = perc), 
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.9) +  # Aumenta a largura das barras masculinas
  
  # Barra para a população feminina (positiva)
  geom_bar(data = dados_total[dados_total$sexo == "feminino", ], 
           aes(y = perc), 
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.9) +  # Aumenta a largura das barras femininas
  
   
  geom_text(data = dados_total[dados_total$sexo == "masculino", ], 
        aes(label = abs(populacao)), 
            position = position_dodge(width = 1), 
            vjust = 0.5,hjust=1, size = 10, color = "black") +  
  
  
  geom_text(data = dados_total[dados_total$sexo == "feminino", ], 
            aes(label = abs(populacao)), 
           position = position_dodge(width = 1), 
            vjust = 0.5, hjust=-0.005,size = 10, color = "black") +  
  
  # Definição de cores
  scale_fill_manual(
    name = "",  # Nome da legenda
    values = c("2014.masculino" = "#7eaf14", 
               "2014.feminino" = "#f6796f",
               "2023.masculino" = "#41590d", 
               "2023.feminino" = "#cc2a1f"),
    labels = c("2014.masculino" = "Masculino - 2014",
               "2014.feminino" = "Feminino - 2014",
               "2023.masculino" = "Masculino - 2023",
               "2023.feminino" = "Feminino - 2023")
  )  +
  
  coord_flip() +
  scale_y_continuous(labels = abs) +  # Remove o sinal negativo dos rótulos
  geom_hline(yintercept = 0, color = "black", size = 1) +
  guides(color = guide_legend(
    override.aes = list(shape = NA, size = 1)  # Aumenta a espessura das linhas na legenda
  )) +
  theme(
    text = element_text(colour = "black"),
    axis.title = element_blank(),
    legend.title = element_text(size = 30),  
    legend.position = "bottom",
    legend.text = element_text(size = 30),
    legend.key.size = unit(1, "cm"),  # Aumenta o tamanho das amostras na legenda
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )




ggsave(paste0(ggraficos,'piramides.pdf'),
       plot = piramides,
       width = 35.42, 
       height = 15.25,
       units = "in")




# Criando o gráfico de barras
g1 <- ggplot(dados_ex, aes(x = Capitulo, y = ex, fill = interaction(Sexo, Ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = ex), vjust = -0.5, size = 6, position = position_dodge(width = 0.9)) +
  labs(x = "Capítulo", y = "Expectativa de Vida (ex)", fill = "") +  
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  scale_fill_discrete(labels = function(x) gsub("\\.", " ", x)) +  # Substitui "." por espaço
  guides(fill = guide_legend(
    override.aes = list(size = 1)  # Ajusta a espessura da legenda
  )) +
  theme(
    text = element_text(colour = "black"),
    axis.title = element_blank(),
    legend.title = element_text(size = 30),  
    legend.position = "bottom",
    legend.text = element_text(size = 30),
    legend.key.size = unit(1, "cm"),  # Aumenta o tamanho das amostras na legenda
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 30)
  )





ggsave(paste0(ggraficos, 'g1.pdf'),
       plot = g1,
       width = 18.42, 
       height = 8.25,
       units = "in")






# grafico comparativo ganho ex

dados_ex <- tibble(
  Capitulo = rep(c("Cap I (infec)", "Cap II (neop)", "Cap IV (endoc)", "Cap IX (circ)", "Cap X (resp)", "Cap XX (c.e)"), 4),
  ex = c(0.22, 0.18, 0.20, 0.40, 0.22, 0.91, 
         0.21, 0.34, 0.24, 0.41, 0.26, 0.27, 
         0.19, 0.29, 0.17, 0.38, 0.21, 0.67, 
         0.23, 0.41, 0.26, 0.45, 0.28, 0.27),
  Sexo = rep(c("Masculino", "Feminino", "Masculino", "Feminino"), each = 6),
  Ano = rep(c(2014, 2014, 2023, 2023), each = 6)
)

# Criando o gráfico de barras

g2 <- ggplot(dados_ex, aes(x = Capitulo, y = ex, 
                           fill = interaction(Sexo, Ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = ex, group = interaction(Sexo, Ano)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 8, check_overlap = TRUE) +
  labs(x = "Capítulo", y = "Ganho na expectativa de Vida", fill = "") +  
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  scale_fill_discrete(labels = function(x) gsub("\\.", " ", x)) +  # Substitui "." por espaço
  guides(fill = guide_legend(override.aes = list(size = 1))) +  # Ajusta legenda
  theme(
    text = element_text(colour = "black"),
    axis.title = element_blank(),
    legend.title = element_text(size = 30),  
    legend.position = "bottom",
    legend.text = element_text(size = 30),
    legend.key.size = unit(1, "cm"),  # Aumenta o tamanho das amostras na legenda
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 30)
  )


ggsave(paste0(ggraficos,'g2.pdf'),
       plot = g2,
       width = 20.42, 
       height = 10.25,
       units = "in")

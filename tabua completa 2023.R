### Libraries
library(tidyverse)
library(ggplot2)
library(xtable)
library(readr)
library(readxl)
library(patchwork)

setwd('c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/')

# 2023
#Homemns 

tabua_se_masc_2023 <- data.frame(idade=factor(c('0','1-4','5-9',
                                                '10-14',
                                                '15-19','20-24','25-29',
                                                '30-34','35-39','40-44',
                                                '45-49','50-54','55-59',
                                                '60-64','65-69','70-74','75-79', '80w'),
                                              levels = c('0','1-4','5-9',
                                                         '10-14','15-19',
                                                         '20-24','25-29',
                                                         '30-34','35-39',
                                                         '40-44','45-49',
                                                         '50-54','55-59',
                                                         '60-64','65-69',
                                                         '70-74','75-79',
                                                         '80w')),
                                 x=as.numeric(c(0,1,5,10,15,20,25,30,35,
                                                40,45,50,55,
                                                60,65,70,75,80)),
                                 n=as.numeric(c(1,4,5,5,5,5,5,5,5,5,5,5,
                                                5,5,5,5,5,0)),
                                 mortes=c(288, 47, 18, 29, 127, 278, 282, 259, 283, 
                                          333, 381, 438, 560, 666, 758, 746, 782, 1680),
                                 populacao=c(17069, 69343, 86816, 88249, 92206, 
                                             97630, 101524, 94970, 92556, 84880, 73592, 63929, 
                                             55503, 42946, 31795, 22876, 14952, 13838))




## Taxa de mortalidade por idade
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(nmx = mortes/populacao)
tabua_se_masc_2023$nmx <-
  round(tabua_se_masc_2023$nmx, digits = 6)

## Fator de separação
nr_linhas <- nrow(tabua_se_masc_2023)
tabua_se_masc_2023$nax <- 2.5

#### para 0a1

if(tabua_se_masc_2023$nmx[1]>=0.107){
  tabua_se_masc_2023$nax[1]=0.33
} else{
  tabua_se_masc_2023$nax[1]=0.045+2.684*tabua_se_masc_2023$nmx[1]
}
#### para 1a4
if(tabua_se_masc_2023$nmx[2]>=0.107){
  tabua_se_masc_2023$nax[2]=1.352
} else{
  tabua_se_masc_2023$nax[2] = 1.651-2.816*tabua_se_masc_2023$nmx[1]
}
#### para +a80
tabua_se_masc_2023$nax[nr_linhas] <-
  1/tabua_se_masc_2023$nmx[nr_linhas]

## Probabilidade de morte
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(nqx=(n*nmx)/(1+(n-nax)*nmx))
tabua_se_masc_2023$nqx[nr_linhas] <- 1

## Probabilidade de sobrevivência
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(npx = 1 - nqx)

## Sobrevivente à idade exata x
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(lx = 100000* cumprod(c(1, npx[-nr_linhas])))
tabua_se_masc_2023$lx <- round(tabua_se_masc_2023$lx, digits = 0)

## Óbitos na coorte de 100 mil nascidos vivos
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(ndx = c(-diff(lx), lx[nr_linhas]))
tabua_se_masc_2023$ndx <- round(tabua_se_masc_2023$ndx, digits = 0)

## Pessoas anos vividos
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(nLx = n*(lx-ndx)+nax*ndx)
tabua_se_masc_2023$nLx <- round(tabua_se_masc_2023$nLx, digits = 0)

## Pessoas anos a serem vividos
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(Tx = sum(nLx) - cumsum(nLx) + nLx)

## Esperança de vida ao nascer
tabua_se_masc_2023 <- tabua_se_masc_2023 %>%
  mutate(ex = Tx/lx)
tabua_se_masc_2023$ex <- round(tabua_se_masc_2023$ex, digits = 1)

print(
  xtable(tabua_se_masc_2023, digits = c(0, 0, 0, 0, 0, 0, 4, 2, 3, 3, 0, 0,0,0,2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)

#gráfico nqx

nqx_ser23_masc <- ggplot(tabua_se_masc_2023, aes(x = idade, y = round(log(nqx),3), group = 1)) +
  geom_line() +  # Plotando a linha
  geom_point()+
  ggtitle("Sexo masculino, Sergipe 2023")+
  geom_text(aes(label = round(nqx,3)), vjust = -0.8, size = 10, check_overlap = TRUE) +
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  theme(
    axis.text.x = element_text(size = 30),  
    axis.text.y = element_text(size = 40),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title.y = element_text(size = 40),
    axis.title.x = element_text(size = 30)
  )



saveRDS(tabua_se_masc_2023,"TabuaMascSE2023.RData")

### causa excluída

## importa a base com causas específicas ---
causas_masc_2023 <- read_excel("c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/obitos_sergipe_cid10_2023_masc.xlsx")

colnames(causas_masc_2023) <-
  c('idade',
    'cap1',
    'cap2',
    'cap4',
    'cap9',
    'cap10',
    'cap20')
base_causas_masc_2023 <- causas_masc_2023 %>%
  mutate(
    gr_idade = case_when(
      idade == '7 a 27 dias' ~ 0,
      idade == '28 a 364 dias' ~ 0,
      idade == '1 a 4 anos' ~ 1,
      idade == '5 a 9 anos' ~ 5,
      idade == '10 a 14 anos' ~ 10,
      idade == '15 a 19 anos' ~ 15,
      idade == '20 a 24 anos' ~ 20,
      idade == '25 a 29 anos' ~ 25,
      idade == '30 a 34 anos' ~ 30,
      idade == '35 a 39 anos' ~ 35,
      idade == '40 a 44 anos' ~ 40,
      idade == '45 a 49 anos' ~ 45,
      idade == '50 a 54 anos' ~ 50,
      idade == '55 a 59 anos' ~ 55,
      idade == '60 a 64 anos' ~ 60,
      idade == '65 a 69 anos' ~ 65,
      idade == '70 a 74 anos' ~ 70,
      idade == '75 a 79 anos' ~ 75,
      idade == '80 anos e mais' ~ 80
    )
  )

base_causas_masc_2023$cap1 <- as.numeric(base_causas_masc_2023$cap1)
base_causas_masc_2023$cap2 <- as.numeric(base_causas_masc_2023$cap2)
base_causas_masc_2023$cap4 <- as.numeric(base_causas_masc_2023$cap4)
base_causas_masc_2023$cap9 <- as.numeric(base_causas_masc_2023$cap9)
base_causas_masc_2023$cap10 <-
  as.numeric(base_causas_masc_2023$cap10)
base_causas_masc_2023$cap20 <-
  as.numeric(base_causas_masc_2023$cap20)
base_causas_masc_2023[is.na(base_causas_masc_2023)] <- 0
base_causas_masc_2023 <-
  base_causas_masc_2023 %>% group_by(gr_idade) %>%
  summarise(
    infec = sum(cap1),
    neop = sum(cap2),
    endoc = sum(cap4),
    circ = sum(cap9),
    resp = sum(cap10),
    ext = sum(cap20)
  )

print(
  xtable(
    base_causas_masc_2023,
    digits = c(0, 0, 0, 0, 0, 0, 0,0)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
str(base_causas_masc_2023)

save(base_causas_masc_2023, file = 'base_causas_masc_2023.RData')

### Montando a tábua com doenças infecciosas
TabuaMascSE2023 <-
  readRDS(file = "TabuaMascSE2023.RData") ## Carrega o arquivo

base_decr_masc_infec_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2023$n,
    obt = TabuaMascSE2023$ndx,
    lx = TabuaMascSE2023$lx,
    npx = TabuaMascSE2023$npx,
    nqx = TabuaMascSE2023$nqx,
    nax = TabuaMascSE2023$nax,
    ex = TabuaMascSE2023$ex,
    infec = base_causas_masc_2023$infec
  )

saveRDS(base_decr_masc_infec_2023, file = 'base_decr_masc_infec_2023.RData')

print(
  xtable(base_decr_masc_infec_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(obt_si = obt - infec)
### R_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(r_si = (obt - infec) / obt)
base_decr_masc_infec_2023$r_si <-
  round(base_decr_masc_infec_2023$r_si, digits = 5)
### P_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_infec_2023$npx_si <-
  round(base_decr_masc_infec_2023$npx_si, digits = 5)
### nqx_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_infec_2023$nqx_si <-
  round(base_decr_masc_infec_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2023)
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_infec_2023$lx_si <-
  round(base_decr_masc_infec_2023$lx_si, digits = 0)
### nax_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_infec_2023$nax_si[18] <-
  base_decr_masc_infec_2023$ex[18] / base_decr_masc_infec_2023$r_si[18]
base_decr_masc_infec_2023$nax_si <-
  round(base_decr_masc_infec_2023$nax_si, digits = 2)
### nLx_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_infec_2023$nLx_si[18] <-base_decr_masc_infec_2023$lx_si[18] * base_decr_masc_infec_2023$nax_si[18]
base_decr_masc_infec_2023$nLx_si <-
  round(base_decr_masc_infec_2023$nLx_si, digits = 0)
### Tx_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_infec_2023$Tx_si <-
  round(base_decr_masc_infec_2023$Tx_si, digits = 0)
### ex_si
base_decr_masc_infec_2023 <- base_decr_masc_infec_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_infec_2023$ex_si <-
  round(base_decr_masc_infec_2023$ex_si, digits = 2)
saveRDS(base_decr_masc_infec_2023, file = 'base_decr_masc_infec_2023.RData')

print(
  xtable(
    base_decr_masc_infec_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_infec_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com neoplasias
TabuaMascSE2023 <-
  readRDS(file = "TabuaMascSE2023.RData") ## Carrega o arquivo

base_decr_masc_neop_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2023$n,
    obt = TabuaMascSE2023$ndx,
    lx = TabuaMascSE2023$lx,
    npx = TabuaMascSE2023$npx,
    nqx = TabuaMascSE2023$nqx,
    nax = TabuaMascSE2023$nax,
    ex = TabuaMascSE2023$ex,
    neop = base_causas_masc_2023$neop
  )

saveRDS(base_decr_masc_neop_2023, file = 'base_decr_masc_neop_2023.RData')

print(
  xtable(base_decr_masc_neop_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(obt_si = obt - neop)
### R_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(r_si = (obt - neop) / obt)
base_decr_masc_neop_2023$r_si <-
  round(base_decr_masc_neop_2023$r_si, digits = 5)
### P_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_neop_2023$npx_si <-
  round(base_decr_masc_neop_2023$npx_si, digits = 5)
### nqx_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_neop_2023$nqx_si <-
  round(base_decr_masc_neop_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2023)
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_neop_2023$lx_si <-
  round(base_decr_masc_neop_2023$lx_si, digits = 0)
### nax_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_neop_2023$nax_si[18] <-
  base_decr_masc_neop_2023$ex[18] / base_decr_masc_neop_2023$r_si[18]
base_decr_masc_neop_2023$nax_si <-
  round(base_decr_masc_neop_2023$nax_si, digits = 2)
### nLx_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_neop_2023$nLx_si[18] <-base_decr_masc_neop_2023$lx_si[18] * base_decr_masc_neop_2023$nax_si[18]
base_decr_masc_neop_2023$nLx_si <-
  round(base_decr_masc_neop_2023$nLx_si, digits = 0)
### Tx_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_neop_2023$Tx_si <-
  round(base_decr_masc_neop_2023$Tx_si, digits = 0)
### ex_si
base_decr_masc_neop_2023 <- base_decr_masc_neop_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_neop_2023$ex_si <-
  round(base_decr_masc_neop_2023$ex_si, digits = 2)
saveRDS(base_decr_masc_neop_2023, file = 'base_decr_masc_neop_2023.RData')

print(
  xtable(
    base_decr_masc_neop_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_neop_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com endocrino
TabuaMascSE2023 <-
  readRDS(file = "TabuaMascSE2023.RData") ## Carrega o arquivo

base_decr_masc_endoc_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2023$n,
    obt = TabuaMascSE2023$ndx,
    lx = TabuaMascSE2023$lx,
    npx = TabuaMascSE2023$npx,
    nqx = TabuaMascSE2023$nqx,
    nax = TabuaMascSE2023$nax,
    ex = TabuaMascSE2023$ex,
    endoc = base_causas_masc_2023$endoc
  )

saveRDS(base_decr_masc_endoc_2023, file = 'base_decr_masc_endoc_2023.RData')

print(
  xtable(base_decr_masc_endoc_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(obt_si = obt - endoc)
### R_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(r_si = (obt - endoc) / obt)
base_decr_masc_endoc_2023$r_si <-
  round(base_decr_masc_endoc_2023$r_si, digits = 5)
### P_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_endoc_2023$npx_si <-
  round(base_decr_masc_endoc_2023$npx_si, digits = 5)
### nqx_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_endoc_2023$nqx_si <-
  round(base_decr_masc_endoc_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2023)
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_endoc_2023$lx_si <-
  round(base_decr_masc_endoc_2023$lx_si, digits = 0)
### nax_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_endoc_2023$nax_si[18] <-
  base_decr_masc_endoc_2023$ex[18] / base_decr_masc_endoc_2023$r_si[18]
base_decr_masc_endoc_2023$nax_si <-
  round(base_decr_masc_endoc_2023$nax_si, digits = 2)
### nLx_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_endoc_2023$nLx_si[18] <-base_decr_masc_endoc_2023$lx_si[18] * base_decr_masc_endoc_2023$nax_si[18]
base_decr_masc_endoc_2023$nLx_si <-
  round(base_decr_masc_endoc_2023$nLx_si, digits = 0)
### Tx_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_endoc_2023$Tx_si <-
  round(base_decr_masc_endoc_2023$Tx_si, digits = 0)
### ex_si
base_decr_masc_endoc_2023 <- base_decr_masc_endoc_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_endoc_2023$ex_si <-
  round(base_decr_masc_endoc_2023$ex_si, digits = 2)
saveRDS(base_decr_masc_endoc_2023, file = 'base_decr_masc_endoc_2023.RData')

print(
  xtable(
    base_decr_masc_endoc_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_endoc_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com circulatorio
TabuaMascSE2023 <-
  readRDS(file = "TabuaMascSE2023.RData") ## Carrega o arquivo

base_decr_masc_circ_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2023$n,
    obt = TabuaMascSE2023$ndx,
    lx = TabuaMascSE2023$lx,
    npx = TabuaMascSE2023$npx,
    nqx = TabuaMascSE2023$nqx,
    nax = TabuaMascSE2023$nax,
    ex = TabuaMascSE2023$ex,
    circ = base_causas_masc_2023$circ
  )

saveRDS(base_decr_masc_circ_2023, file = 'base_decr_masc_circ_2023.RData')

print(
  xtable(base_decr_masc_circ_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(obt_si = obt - circ)
### R_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(r_si = (obt - circ) / obt)
base_decr_masc_circ_2023$r_si <-
  round(base_decr_masc_circ_2023$r_si, digits = 5)
### P_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_circ_2023$npx_si <-
  round(base_decr_masc_circ_2023$npx_si, digits = 5)
### nqx_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_circ_2023$nqx_si <-
  round(base_decr_masc_circ_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2023)
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_circ_2023$lx_si <-
  round(base_decr_masc_circ_2023$lx_si, digits = 0)
### nax_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_circ_2023$nax_si[18] <-
  base_decr_masc_circ_2023$ex[18] / base_decr_masc_circ_2023$r_si[18]
base_decr_masc_circ_2023$nax_si <-
  round(base_decr_masc_circ_2023$nax_si, digits = 2)
### nLx_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_circ_2023$nLx_si[18] <-base_decr_masc_circ_2023$lx_si[18] * base_decr_masc_circ_2023$nax_si[18]
base_decr_masc_circ_2023$nLx_si <-
  round(base_decr_masc_circ_2023$nLx_si, digits = 0)
### Tx_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_circ_2023$Tx_si <-
  round(base_decr_masc_circ_2023$Tx_si, digits = 0)
### ex_si
base_decr_masc_circ_2023 <- base_decr_masc_circ_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_circ_2023$ex_si <-
  round(base_decr_masc_circ_2023$ex_si, digits = 2)
saveRDS(base_decr_masc_circ_2023, file = 'base_decr_masc_circ_2023.RData')

print(
  xtable(
    base_decr_masc_circ_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_circ_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com respirtorio
TabuaMascSE2023 <-
  readRDS(file = "TabuaMascSE2023.RData") ## Carrega o arquivo

base_decr_masc_resp_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2023$n,
    obt = TabuaMascSE2023$ndx,
    lx = TabuaMascSE2023$lx,
    npx = TabuaMascSE2023$npx,
    nqx = TabuaMascSE2023$nqx,
    nax = TabuaMascSE2023$nax,
    ex = TabuaMascSE2023$ex,
    resp = base_causas_masc_2023$resp
  )

saveRDS(base_decr_masc_resp_2023, file = 'base_decr_masc_resp_2023.RData')

print(
  xtable(base_decr_masc_resp_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(obt_si = obt - resp)
### R_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(r_si = (obt - resp) / obt)
base_decr_masc_resp_2023$r_si <-
  round(base_decr_masc_resp_2023$r_si, digits = 5)
### P_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_resp_2023$npx_si <-
  round(base_decr_masc_resp_2023$npx_si, digits = 5)
### nqx_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_resp_2023$nqx_si <-
  round(base_decr_masc_resp_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2023)
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_resp_2023$lx_si <-
  round(base_decr_masc_resp_2023$lx_si, digits = 0)
### nax_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_resp_2023$nax_si[18] <-
  base_decr_masc_resp_2023$ex[18] / base_decr_masc_resp_2023$r_si[18]
base_decr_masc_resp_2023$nax_si <-
  round(base_decr_masc_resp_2023$nax_si, digits = 2)
### nLx_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_resp_2023$nLx_si[18] <-base_decr_masc_resp_2023$lx_si[18] * base_decr_masc_resp_2023$nax_si[18]
base_decr_masc_resp_2023$nLx_si <-
  round(base_decr_masc_resp_2023$nLx_si, digits = 0)
### Tx_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_resp_2023$Tx_si <-
  round(base_decr_masc_resp_2023$Tx_si, digits = 0)
### ex_si
base_decr_masc_resp_2023 <- base_decr_masc_resp_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_resp_2023$ex_si <-
  round(base_decr_masc_resp_2023$ex_si, digits = 2)
saveRDS(base_decr_masc_resp_2023, file = 'base_decr_masc_resp_2023.RData')

print(
  xtable(
    base_decr_masc_resp_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_resp_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com externas
TabuaMascSE2023 <-
  readRDS(file = "TabuaMascSE2023.RData") ## Carrega o arquivo

base_decr_masc_ext_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2023$n,
    obt = TabuaMascSE2023$ndx,
    lx = TabuaMascSE2023$lx,
    npx = TabuaMascSE2023$npx,
    nqx = TabuaMascSE2023$nqx,
    nax = TabuaMascSE2023$nax,
    ex = TabuaMascSE2023$ex,
    ext = base_causas_masc_2023$ext
  )

saveRDS(base_decr_masc_ext_2023, file = 'base_decr_masc_ext_2023.RData')

print(
  xtable(base_decr_masc_ext_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(obt_si = obt - ext)
### R_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(r_si = (obt - ext) / obt)
base_decr_masc_ext_2023$r_si <-
  round(base_decr_masc_ext_2023$r_si, digits = 5)
### P_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_ext_2023$npx_si <-
  round(base_decr_masc_ext_2023$npx_si, digits = 5)
### nqx_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_ext_2023$nqx_si <-
  round(base_decr_masc_ext_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2023)
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_ext_2023$lx_si <-
  round(base_decr_masc_ext_2023$lx_si, digits = 0)
### nax_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_ext_2023$nax_si[18] <-
  base_decr_masc_ext_2023$ex[18] / base_decr_masc_ext_2023$r_si[18]
base_decr_masc_ext_2023$nax_si <-
  round(base_decr_masc_ext_2023$nax_si, digits = 2)
### nLx_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_ext_2023$nLx_si[18] <-base_decr_masc_ext_2023$lx_si[18] * base_decr_masc_ext_2023$nax_si[18]
base_decr_masc_ext_2023$nLx_si <-
  round(base_decr_masc_ext_2023$nLx_si, digits = 0)
### Tx_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_ext_2023$Tx_si <-
  round(base_decr_masc_ext_2023$Tx_si, digits = 0)
### ex_si
base_decr_masc_ext_2023 <- base_decr_masc_ext_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_ext_2023$ex_si <-
  round(base_decr_masc_ext_2023$ex_si, digits = 2)
saveRDS(base_decr_masc_ext_2023, file = 'base_decr_masc_ext_2023.RData')

print(
  xtable(
    base_decr_masc_ext_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_ext_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)










###################################################################################################################################################

#mulheres

tabua_se_fem_2023 <- data.frame(idade=factor(c('0','1-4','5-9',
                                               '10-14',
                                               '15-19','20-24','25-29',
                                               '30-34','35-39','40-44',
                                               '45-49','50-54','55-59',
                                               '60-64','65-69','70-74','75-79', '80w'),
                                             levels = c('0','1-4','5-9',
                                                        '10-14','15-19',
                                                        '20-24','25-29',
                                                        '30-34','35-39',
                                                        '40-44','45-49',
                                                        '50-54','55-59',
                                                        '60-64','65-69',
                                                        '70-74','75-79',
                                                        '80w')),
                                x=as.numeric(c(0,1,5,10,15,20,25,30,35,
                                               40,45,50,55,
                                               60,65,70,75,80)),
                                n=as.numeric(c(1,4,5,5,5,5,5,5,5,5,5,5,
                                               5,5,5,5,5,0)),
                                mortes=c(239, 38, 22, 21, 28, 54, 57, 84, 117, 168, 174, 
                                         319, 377, 419, 465, 563, 654, 2323),
                                populacao=c(16283, 66190, 83021, 84835, 89974, 97506, 
                                            104205, 101909, 101654, 95062, 84037, 74038, 
                                            64968, 51738, 39869, 30335, 21026, 25123))




## Taxa de mortalidade por idade
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(nmx = mortes/populacao)
tabua_se_fem_2023$nmx <-
  round(tabua_se_fem_2023$nmx, digits = 6)

## Fator de separação
nr_linhas <- nrow(tabua_se_fem_2023)
tabua_se_fem_2023$nax <- 2.5

#### para 0a1

if(tabua_se_fem_2023$nmx[1]>=0.107){
  tabua_se_fem_2023$nax[1]=0.33
} else{
  tabua_se_fem_2023$nax[1]=0.045+2.684*tabua_se_fem_2023$nmx[1]
}
#### para 1a4
if(tabua_se_fem_2023$nmx[2]>=0.107){
  tabua_se_fem_2023$nax[2]=1.352
} else{
  tabua_se_fem_2023$nax[2] = 1.651-2.816*tabua_se_fem_2023$nmx[1]
}
#### para +a80
tabua_se_fem_2023$nax[nr_linhas] <-
  1/tabua_se_fem_2023$nmx[nr_linhas]

## Probabilidade de morte
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(nqx=(n*nmx)/(1+(n-nax)*nmx))
tabua_se_fem_2023$nqx[nr_linhas] <- 1

## Probabilidade de sobrevivência
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(npx = 1 - nqx)

## Sobrevivente à idade exata x
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(lx = 100000* cumprod(c(1, npx[-nr_linhas])))
tabua_se_fem_2023$lx <- round(tabua_se_fem_2023$lx, digits = 0)

## Óbitos na coorte de 100 mil nascidos vivos
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(ndx = c(-diff(lx), lx[nr_linhas]))
tabua_se_fem_2023$ndx <- round(tabua_se_fem_2023$ndx, digits = 0)

## Pessoas anos vividos
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(nLx = n*(lx-ndx)+nax*ndx)
tabua_se_fem_2023$nLx <- round(tabua_se_fem_2023$nLx, digits = 0)

## Pessoas anos a serem vividos
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(Tx = sum(nLx) - cumsum(nLx) + nLx)

## Esperança de vida ao nascer
tabua_se_fem_2023 <- tabua_se_fem_2023 %>%
  mutate(ex = Tx/lx)
tabua_se_fem_2023$ex <- round(tabua_se_fem_2023$ex, digits = 1)

print(
  xtable(tabua_se_fem_2023, digits = c(0, 0, 0, 0, 0, 0, 4, 2, 3, 3, 0, 0,0,0,2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)



#gráfico nqx

nqx_ser23_fem <- ggplot(tabua_se_fem_2023, aes(x = idade, y = round(log(nqx),3), group = 1)) +
  geom_line() +  # Plotando a linha
  geom_point()+
  ggtitle("Sexo feminino, Sergipe 2023")+
  geom_text(aes(label = round(nqx,3)), vjust = -0.8, size = 10, check_overlap = TRUE) +
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  theme(
    axis.text.x = element_text(size = 30),  
    axis.text.y = element_text(size = 40),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title.y = element_text(size = 40),
    axis.title.x = element_text(size = 30)
  )


ggraficos <- "c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/graficos/"

nqx23 <- (nqx_ser23_masc/nqx_ser23_fem)+
  theme(text = element_text(colour = "black"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        # plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 30),
        legend.position = "bottom",
        legend.text = element_text(size = 11))



ggsave(paste0(ggraficos, 'nqx23.pdf'),
       plot = nqx23,
       width = 30, 
       height = 20.25,
       units = "in")


saveRDS(tabua_se_fem_2023,"TabuafemSE2023.RData")

### causa excluída

## importa a base com causas específicas ---
causas_fem_2023 <- read_excel("c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/obitos_sergipe_cid10_2023_fem.xlsx")

colnames(causas_fem_2023) <-
  c('idade',
    'cap1',
    'cap2',
    'cap4',
    'cap9',
    'cap10',
    'cap20')
base_causas_fem_2023 <- causas_fem_2023 %>%
  mutate(
    gr_idade = case_when(
      idade == '7 a 27 dias' ~ 0,
      idade == '28 a 364 dias' ~ 0,
      idade == '1 a 4 anos' ~ 1,
      idade == '5 a 9 anos' ~ 5,
      idade == '10 a 14 anos' ~ 10,
      idade == '15 a 19 anos' ~ 15,
      idade == '20 a 24 anos' ~ 20,
      idade == '25 a 29 anos' ~ 25,
      idade == '30 a 34 anos' ~ 30,
      idade == '35 a 39 anos' ~ 35,
      idade == '40 a 44 anos' ~ 40,
      idade == '45 a 49 anos' ~ 45,
      idade == '50 a 54 anos' ~ 50,
      idade == '55 a 59 anos' ~ 55,
      idade == '60 a 64 anos' ~ 60,
      idade == '65 a 69 anos' ~ 65,
      idade == '70 a 74 anos' ~ 70,
      idade == '75 a 79 anos' ~ 75,
      idade == '80 anos e mais' ~ 80
    )
  )

base_causas_fem_2023$cap1 <- as.numeric(base_causas_fem_2023$cap1)
base_causas_fem_2023$cap2 <- as.numeric(base_causas_fem_2023$cap2)
base_causas_fem_2023$cap4 <- as.numeric(base_causas_fem_2023$cap4)
base_causas_fem_2023$cap9 <- as.numeric(base_causas_fem_2023$cap9)
base_causas_fem_2023$cap10 <-
  as.numeric(base_causas_fem_2023$cap10)
base_causas_fem_2023$cap20 <-
  as.numeric(base_causas_fem_2023$cap20)
base_causas_fem_2023[is.na(base_causas_fem_2023)] <- 0
base_causas_fem_2023 <-
  base_causas_fem_2023 %>% group_by(gr_idade) %>%
  summarise(
    infec = sum(cap1),
    neop = sum(cap2),
    endoc = sum(cap4),
    circ = sum(cap9),
    resp = sum(cap10),
    ext = sum(cap20)
  )

print(
  xtable(
    base_causas_fem_2023,
    digits = c(0, 0, 0, 0, 0, 0, 0,0)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
str(base_causas_fem_2023)

save(base_causas_fem_2023, file = 'base_causas_fem_2023.RData')

### Montando a tábua com doenças infecciosas
TabuafemSE2023 <-
  readRDS(file = "TabuafemSE2023.RData") ## Carrega o arquivo

base_decr_fem_infec_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2023$n,
    obt = TabuafemSE2023$ndx,
    lx = TabuafemSE2023$lx,
    npx = TabuafemSE2023$npx,
    nqx = TabuafemSE2023$nqx,
    nax = TabuafemSE2023$nax,
    ex = TabuafemSE2023$ex,
    infec = base_causas_fem_2023$infec
  )

saveRDS(base_decr_fem_infec_2023, file = 'base_decr_fem_infec_2023.RData')

print(
  xtable(base_decr_fem_infec_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(obt_si = obt - infec)
### R_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(r_si = (obt - infec) / obt)
base_decr_fem_infec_2023$r_si <-
  round(base_decr_fem_infec_2023$r_si, digits = 5)
### P_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_infec_2023$npx_si <-
  round(base_decr_fem_infec_2023$npx_si, digits = 5)
### nqx_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_infec_2023$nqx_si <-
  round(base_decr_fem_infec_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2023)
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_infec_2023$lx_si <-
  round(base_decr_fem_infec_2023$lx_si, digits = 0)
### nax_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_infec_2023$nax_si[18] <-
  base_decr_fem_infec_2023$ex[18] / base_decr_fem_infec_2023$r_si[18]
base_decr_fem_infec_2023$nax_si <-
  round(base_decr_fem_infec_2023$nax_si, digits = 2)
### nLx_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_infec_2023$nLx_si[18] <-base_decr_fem_infec_2023$lx_si[18] * base_decr_fem_infec_2023$nax_si[18]
base_decr_fem_infec_2023$nLx_si <-
  round(base_decr_fem_infec_2023$nLx_si, digits = 0)
### Tx_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_infec_2023$Tx_si <-
  round(base_decr_fem_infec_2023$Tx_si, digits = 0)
### ex_si
base_decr_fem_infec_2023 <- base_decr_fem_infec_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_infec_2023$ex_si <-
  round(base_decr_fem_infec_2023$ex_si, digits = 2)
saveRDS(base_decr_fem_infec_2023, file = 'base_decr_fem_infec_2023.RData')

print(
  xtable(
    base_decr_fem_infec_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_infec_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com neoplasias
TabuafemSE2023 <-
  readRDS(file = "TabuafemSE2023.RData") ## Carrega o arquivo

base_decr_fem_neop_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2023$n,
    obt = TabuafemSE2023$ndx,
    lx = TabuafemSE2023$lx,
    npx = TabuafemSE2023$npx,
    nqx = TabuafemSE2023$nqx,
    nax = TabuafemSE2023$nax,
    ex = TabuafemSE2023$ex,
    neop = base_causas_fem_2023$neop
  )

saveRDS(base_decr_fem_neop_2023, file = 'base_decr_fem_neop_2023.RData')

print(
  xtable(base_decr_fem_neop_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(obt_si = obt - neop)
### R_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(r_si = (obt - neop) / obt)
base_decr_fem_neop_2023$r_si <-
  round(base_decr_fem_neop_2023$r_si, digits = 5)
### P_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_neop_2023$npx_si <-
  round(base_decr_fem_neop_2023$npx_si, digits = 5)
### nqx_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_neop_2023$nqx_si <-
  round(base_decr_fem_neop_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2023)
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_neop_2023$lx_si <-
  round(base_decr_fem_neop_2023$lx_si, digits = 0)
### nax_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_neop_2023$nax_si[18] <-
  base_decr_fem_neop_2023$ex[18] / base_decr_fem_neop_2023$r_si[18]
base_decr_fem_neop_2023$nax_si <-
  round(base_decr_fem_neop_2023$nax_si, digits = 2)
### nLx_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_neop_2023$nLx_si[18] <-base_decr_fem_neop_2023$lx_si[18] * base_decr_fem_neop_2023$nax_si[18]
base_decr_fem_neop_2023$nLx_si <-
  round(base_decr_fem_neop_2023$nLx_si, digits = 0)
### Tx_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_neop_2023$Tx_si <-
  round(base_decr_fem_neop_2023$Tx_si, digits = 0)
### ex_si
base_decr_fem_neop_2023 <- base_decr_fem_neop_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_neop_2023$ex_si <-
  round(base_decr_fem_neop_2023$ex_si, digits = 2)
saveRDS(base_decr_fem_neop_2023, file = 'base_decr_fem_neop_2023.RData')

print(
  xtable(
    base_decr_fem_neop_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_neop_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com endocrino
TabuafemSE2023 <-
  readRDS(file = "TabuafemSE2023.RData") ## Carrega o arquivo

base_decr_fem_endoc_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2023$n,
    obt = TabuafemSE2023$ndx,
    lx = TabuafemSE2023$lx,
    npx = TabuafemSE2023$npx,
    nqx = TabuafemSE2023$nqx,
    nax = TabuafemSE2023$nax,
    ex = TabuafemSE2023$ex,
    endoc = base_causas_fem_2023$endoc
  )

saveRDS(base_decr_fem_endoc_2023, file = 'base_decr_fem_endoc_2023.RData')

print(
  xtable(base_decr_fem_endoc_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(obt_si = obt - endoc)
### R_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(r_si = (obt - endoc) / obt)
base_decr_fem_endoc_2023$r_si <-
  round(base_decr_fem_endoc_2023$r_si, digits = 5)
### P_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_endoc_2023$npx_si <-
  round(base_decr_fem_endoc_2023$npx_si, digits = 5)
### nqx_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_endoc_2023$nqx_si <-
  round(base_decr_fem_endoc_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2023)
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_endoc_2023$lx_si <-
  round(base_decr_fem_endoc_2023$lx_si, digits = 0)
### nax_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_endoc_2023$nax_si[18] <-
  base_decr_fem_endoc_2023$ex[18] / base_decr_fem_endoc_2023$r_si[18]
base_decr_fem_endoc_2023$nax_si <-
  round(base_decr_fem_endoc_2023$nax_si, digits = 2)
### nLx_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_endoc_2023$nLx_si[18] <-base_decr_fem_endoc_2023$lx_si[18] * base_decr_fem_endoc_2023$nax_si[18]
base_decr_fem_endoc_2023$nLx_si <-
  round(base_decr_fem_endoc_2023$nLx_si, digits = 0)
### Tx_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_endoc_2023$Tx_si <-
  round(base_decr_fem_endoc_2023$Tx_si, digits = 0)
### ex_si
base_decr_fem_endoc_2023 <- base_decr_fem_endoc_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_endoc_2023$ex_si <-
  round(base_decr_fem_endoc_2023$ex_si, digits = 2)
saveRDS(base_decr_fem_endoc_2023, file = 'base_decr_fem_endoc_2023.RData')

print(
  xtable(
    base_decr_fem_endoc_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_endoc_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com circulatorio
TabuafemSE2023 <-
  readRDS(file = "TabuafemSE2023.RData") ## Carrega o arquivo

base_decr_fem_circ_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2023$n,
    obt = TabuafemSE2023$ndx,
    lx = TabuafemSE2023$lx,
    npx = TabuafemSE2023$npx,
    nqx = TabuafemSE2023$nqx,
    nax = TabuafemSE2023$nax,
    ex = TabuafemSE2023$ex,
    circ = base_causas_fem_2023$circ
  )

saveRDS(base_decr_fem_circ_2023, file = 'base_decr_fem_circ_2023.RData')

print(
  xtable(base_decr_fem_circ_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(obt_si = obt - circ)
### R_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(r_si = (obt - circ) / obt)
base_decr_fem_circ_2023$r_si <-
  round(base_decr_fem_circ_2023$r_si, digits = 5)
### P_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_circ_2023$npx_si <-
  round(base_decr_fem_circ_2023$npx_si, digits = 5)
### nqx_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_circ_2023$nqx_si <-
  round(base_decr_fem_circ_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2023)
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_circ_2023$lx_si <-
  round(base_decr_fem_circ_2023$lx_si, digits = 0)
### nax_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_circ_2023$nax_si[18] <-
  base_decr_fem_circ_2023$ex[18] / base_decr_fem_circ_2023$r_si[18]
base_decr_fem_circ_2023$nax_si <-
  round(base_decr_fem_circ_2023$nax_si, digits = 2)
### nLx_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_circ_2023$nLx_si[18] <-base_decr_fem_circ_2023$lx_si[18] * base_decr_fem_circ_2023$nax_si[18]
base_decr_fem_circ_2023$nLx_si <-
  round(base_decr_fem_circ_2023$nLx_si, digits = 0)
### Tx_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_circ_2023$Tx_si <-
  round(base_decr_fem_circ_2023$Tx_si, digits = 0)
### ex_si
base_decr_fem_circ_2023 <- base_decr_fem_circ_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_circ_2023$ex_si <-
  round(base_decr_fem_circ_2023$ex_si, digits = 2)
saveRDS(base_decr_fem_circ_2023, file = 'base_decr_fem_circ_2023.RData')

print(
  xtable(
    base_decr_fem_circ_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_circ_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com respirtorio
TabuafemSE2023 <-
  readRDS(file = "TabuafemSE2023.RData") ## Carrega o arquivo

base_decr_fem_resp_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2023$n,
    obt = TabuafemSE2023$ndx,
    lx = TabuafemSE2023$lx,
    npx = TabuafemSE2023$npx,
    nqx = TabuafemSE2023$nqx,
    nax = TabuafemSE2023$nax,
    ex = TabuafemSE2023$ex,
    resp = base_causas_fem_2023$resp
  )

saveRDS(base_decr_fem_resp_2023, file = 'base_decr_fem_resp_2023.RData')

print(
  xtable(base_decr_fem_resp_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(obt_si = obt - resp)
### R_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(r_si = (obt - resp) / obt)
base_decr_fem_resp_2023$r_si <-
  round(base_decr_fem_resp_2023$r_si, digits = 5)
### P_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_resp_2023$npx_si <-
  round(base_decr_fem_resp_2023$npx_si, digits = 5)
### nqx_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_resp_2023$nqx_si <-
  round(base_decr_fem_resp_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2023)
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_resp_2023$lx_si <-
  round(base_decr_fem_resp_2023$lx_si, digits = 0)
### nax_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_resp_2023$nax_si[18] <-
  base_decr_fem_resp_2023$ex[18] / base_decr_fem_resp_2023$r_si[18]
base_decr_fem_resp_2023$nax_si <-
  round(base_decr_fem_resp_2023$nax_si, digits = 2)
### nLx_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_resp_2023$nLx_si[18] <-base_decr_fem_resp_2023$lx_si[18] * base_decr_fem_resp_2023$nax_si[18]
base_decr_fem_resp_2023$nLx_si <-
  round(base_decr_fem_resp_2023$nLx_si, digits = 0)
### Tx_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_resp_2023$Tx_si <-
  round(base_decr_fem_resp_2023$Tx_si, digits = 0)
### ex_si
base_decr_fem_resp_2023 <- base_decr_fem_resp_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_resp_2023$ex_si <-
  round(base_decr_fem_resp_2023$ex_si, digits = 2)
saveRDS(base_decr_fem_resp_2023, file = 'base_decr_fem_resp_2023.RData')

print(
  xtable(
    base_decr_fem_resp_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_resp_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com externas
TabuafemSE2023 <-
  readRDS(file = "TabuafemSE2023.RData") ## Carrega o arquivo

base_decr_fem_ext_2023 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2023$n,
    obt = TabuafemSE2023$ndx,
    lx = TabuafemSE2023$lx,
    npx = TabuafemSE2023$npx,
    nqx = TabuafemSE2023$nqx,
    nax = TabuafemSE2023$nax,
    ex = TabuafemSE2023$ex,
    ext = base_causas_fem_2023$ext
  )

saveRDS(base_decr_fem_ext_2023, file = 'base_decr_fem_ext_2023.RData')

print(
  xtable(base_decr_fem_ext_2023, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(obt_si = obt - ext)
### R_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(r_si = (obt - ext) / obt)
base_decr_fem_ext_2023$r_si <-
  round(base_decr_fem_ext_2023$r_si, digits = 5)
### P_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_ext_2023$npx_si <-
  round(base_decr_fem_ext_2023$npx_si, digits = 5)
### nqx_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_ext_2023$nqx_si <-
  round(base_decr_fem_ext_2023$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2023)
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_ext_2023$lx_si <-
  round(base_decr_fem_ext_2023$lx_si, digits = 0)
### nax_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_ext_2023$nax_si[18] <-
  base_decr_fem_ext_2023$ex[18] / base_decr_fem_ext_2023$r_si[18]
base_decr_fem_ext_2023$nax_si <-
  round(base_decr_fem_ext_2023$nax_si, digits = 2)
### nLx_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_ext_2023$nLx_si[18] <-base_decr_fem_ext_2023$lx_si[18] * base_decr_fem_ext_2023$nax_si[18]
base_decr_fem_ext_2023$nLx_si <-
  round(base_decr_fem_ext_2023$nLx_si, digits = 0)
### Tx_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_ext_2023$Tx_si <-
  round(base_decr_fem_ext_2023$Tx_si, digits = 0)
### ex_si
base_decr_fem_ext_2023 <- base_decr_fem_ext_2023 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_ext_2023$ex_si <-
  round(base_decr_fem_ext_2023$ex_si, digits = 2)
saveRDS(base_decr_fem_ext_2023, file = 'base_decr_fem_ext_2023.RData')

print(
  xtable(
    base_decr_fem_ext_2023,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_ext_2023 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


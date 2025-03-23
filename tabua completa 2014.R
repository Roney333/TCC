### Libraries
library(tidyverse)
library(ggplot2)
library(xtable)
library(readr)
library(readxl)
library(patchwork)

setwd('c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/')

# 2014
#Homemns 

tabua_se_masc_2014 <- data.frame(idade=factor(c('0','1-4','5-9',
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
                                 mortes=c(304, 38, 31, 26, 275, 341, 357, 318, 
                                          333, 331, 359, 438, 446, 528, 537, 
                                          587, 514, 1351),
                                 populacao=c(17495,70072, 92108, 97028, 107137, 99862, 
                                             96885, 89686, 79136, 68924, 62633, 
                                             50654, 39424, 31061, 24009, 16173, 
                                             10587, 11051))




## Taxa de mortalidade por idade
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(nmx = mortes/populacao)
tabua_se_masc_2014$nmx <-
  round(tabua_se_masc_2014$nmx, digits = 6)

## Fator de separação
nr_linhas <- nrow(tabua_se_masc_2014)
tabua_se_masc_2014$nax <- 2.5

#### para 0a1

if(tabua_se_masc_2014$nmx[1]>=0.107){
  tabua_se_masc_2014$nax[1]=0.33
} else{
  tabua_se_masc_2014$nax[1]=0.045+2.684*tabua_se_masc_2014$nmx[1]
}
#### para 1a4
if(tabua_se_masc_2014$nmx[2]>=0.107){
  tabua_se_masc_2014$nax[2]=1.352
} else{
  tabua_se_masc_2014$nax[2] = 1.651-2.816*tabua_se_masc_2014$nmx[1]
}
#### para +a80
tabua_se_masc_2014$nax[nr_linhas] <-
  1/tabua_se_masc_2014$nmx[nr_linhas]

## Probabilidade de morte
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(nqx=(n*nmx)/(1+(n-nax)*nmx))
tabua_se_masc_2014$nqx[nr_linhas] <- 1

## Probabilidade de sobrevivência
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(npx = 1 - nqx)

## Sobrevivente à idade exata x
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(lx = 100000* cumprod(c(1, npx[-nr_linhas])))
tabua_se_masc_2014$lx <- round(tabua_se_masc_2014$lx, digits = 0)

## Óbitos na coorte de 100 mil nascidos vivos
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(ndx = c(-diff(lx), lx[nr_linhas]))
tabua_se_masc_2014$ndx <- round(tabua_se_masc_2014$ndx, digits = 0)

## Pessoas anos vividos
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(nLx = n*(lx-ndx)+nax*ndx)
tabua_se_masc_2014$nLx <- round(tabua_se_masc_2014$nLx, digits = 0)

## Pessoas anos a serem vividos
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(Tx = sum(nLx) - cumsum(nLx) + nLx)

## Esperança de vida ao nascer
tabua_se_masc_2014 <- tabua_se_masc_2014 %>%
  mutate(ex = Tx/lx)
tabua_se_masc_2014$ex <- round(tabua_se_masc_2014$ex, digits = 1)


print(
  xtable(tabua_se_masc_2014, digits = c(0, 0, 0, 0, 0, 0, 4, 2, 3, 3, 0, 0,0,0,2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)



#gráfico nqx

nqx_ser14_masc <- ggplot(tabua_se_masc_2014, aes(x = idade, y = round(log(nqx),3), group = 1)) +
  geom_line() +  # Plotando a linha
  geom_point()+
  ggtitle("Sexo masculino, Sergipe 2014")+
  geom_text(aes(label = round(nqx,3)), vjust = -0.8, size = 10, check_overlap = TRUE) +
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1)))  +
  theme(
    axis.text.x = element_text(size = 30),  
    axis.text.y = element_text(size = 40),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title.y = element_text(size = 40),
    axis.title.x = element_text(size = 30)
  )

saveRDS(tabua_se_masc_2014,"TabuaMascSE2014.RData")

### causa excluída

## importa a base com causas específicas ---
causas_masc_2014 <- read_excel("c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/obitos_sergipe_cid10_2014_masc.xlsx")

colnames(causas_masc_2014) <-
  c('idade',
    'cap1',
    'cap2',
    'cap4',
    'cap9',
    'cap10',
    'cap20')
base_causas_masc_2014 <- causas_masc_2014 %>%
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

base_causas_masc_2014$cap1 <- as.numeric(base_causas_masc_2014$cap1)
base_causas_masc_2014$cap2 <- as.numeric(base_causas_masc_2014$cap2)
base_causas_masc_2014$cap4 <- as.numeric(base_causas_masc_2014$cap4)
base_causas_masc_2014$cap9 <- as.numeric(base_causas_masc_2014$cap9)
base_causas_masc_2014$cap10 <-
  as.numeric(base_causas_masc_2014$cap10)
base_causas_masc_2014$cap20 <-
  as.numeric(base_causas_masc_2014$cap20)
base_causas_masc_2014[is.na(base_causas_masc_2014)] <- 0
base_causas_masc_2014 <-
  base_causas_masc_2014 %>% group_by(gr_idade) %>%
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
    base_causas_masc_2014,
    digits = c(0, 0, 0, 0, 0, 0, 0,0)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
str(base_causas_masc_2014)

save(base_causas_masc_2014, file = 'base_causas_masc_2014.RData')

### Montando a tábua com doenças infecciosas
TabuaMascSE2014 <-
  readRDS(file = "TabuaMascSE2014.RData") ## Carrega o arquivo

base_decr_masc_infec_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2014$n,
    obt = TabuaMascSE2014$ndx,
    lx = TabuaMascSE2014$lx,
    npx = TabuaMascSE2014$npx,
    nqx = TabuaMascSE2014$nqx,
    nax = TabuaMascSE2014$nax,
    ex = TabuaMascSE2014$ex,
    infec = base_causas_masc_2014$infec
  )

saveRDS(base_decr_masc_infec_2014, file = 'base_decr_masc_infec_2014.RData')

print(
  xtable(base_decr_masc_infec_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(obt_si = obt - infec)
### R_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(r_si = (obt - infec) / obt)
base_decr_masc_infec_2014$r_si <-
  round(base_decr_masc_infec_2014$r_si, digits = 5)
### P_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_infec_2014$npx_si <-
  round(base_decr_masc_infec_2014$npx_si, digits = 5)
### nqx_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_infec_2014$nqx_si <-
  round(base_decr_masc_infec_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2014)
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_infec_2014$lx_si <-
  round(base_decr_masc_infec_2014$lx_si, digits = 0)
### nax_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_infec_2014$nax_si[18] <-
  base_decr_masc_infec_2014$ex[18] / base_decr_masc_infec_2014$r_si[18]
base_decr_masc_infec_2014$nax_si <-
  round(base_decr_masc_infec_2014$nax_si, digits = 2)
### nLx_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_infec_2014$nLx_si[18] <-base_decr_masc_infec_2014$lx_si[18] * base_decr_masc_infec_2014$nax_si[18]
base_decr_masc_infec_2014$nLx_si <-
  round(base_decr_masc_infec_2014$nLx_si, digits = 0)
### Tx_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_infec_2014$Tx_si <-
  round(base_decr_masc_infec_2014$Tx_si, digits = 0)
### ex_si
base_decr_masc_infec_2014 <- base_decr_masc_infec_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_infec_2014$ex_si <-
  round(base_decr_masc_infec_2014$ex_si, digits = 2)
saveRDS(base_decr_masc_infec_2014, file = 'base_decr_masc_infec_2014.RData')

print(
  xtable(
    base_decr_masc_infec_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_infec_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com neoplasias
TabuaMascSE2014 <-
  readRDS(file = "TabuaMascSE2014.RData") ## Carrega o arquivo

base_decr_masc_neop_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2014$n,
    obt = TabuaMascSE2014$ndx,
    lx = TabuaMascSE2014$lx,
    npx = TabuaMascSE2014$npx,
    nqx = TabuaMascSE2014$nqx,
    nax = TabuaMascSE2014$nax,
    ex = TabuaMascSE2014$ex,
    neop = base_causas_masc_2014$neop
  )

saveRDS(base_decr_masc_neop_2014, file = 'base_decr_masc_neop_2014.RData')

print(
  xtable(base_decr_masc_neop_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(obt_si = obt - neop)
### R_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(r_si = (obt - neop) / obt)
base_decr_masc_neop_2014$r_si <-
  round(base_decr_masc_neop_2014$r_si, digits = 5)
### P_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_neop_2014$npx_si <-
  round(base_decr_masc_neop_2014$npx_si, digits = 5)
### nqx_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_neop_2014$nqx_si <-
  round(base_decr_masc_neop_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2014)
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_neop_2014$lx_si <-
  round(base_decr_masc_neop_2014$lx_si, digits = 0)
### nax_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_neop_2014$nax_si[18] <-
  base_decr_masc_neop_2014$ex[18] / base_decr_masc_neop_2014$r_si[18]
base_decr_masc_neop_2014$nax_si <-
  round(base_decr_masc_neop_2014$nax_si, digits = 2)
### nLx_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_neop_2014$nLx_si[18] <-base_decr_masc_neop_2014$lx_si[18] * base_decr_masc_neop_2014$nax_si[18]
base_decr_masc_neop_2014$nLx_si <-
  round(base_decr_masc_neop_2014$nLx_si, digits = 0)
### Tx_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_neop_2014$Tx_si <-
  round(base_decr_masc_neop_2014$Tx_si, digits = 0)
### ex_si
base_decr_masc_neop_2014 <- base_decr_masc_neop_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_neop_2014$ex_si <-
  round(base_decr_masc_neop_2014$ex_si, digits = 2)
saveRDS(base_decr_masc_neop_2014, file = 'base_decr_masc_neop_2014.RData')

print(
  xtable(
    base_decr_masc_neop_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_neop_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com endocrino
TabuaMascSE2014 <-
  readRDS(file = "TabuaMascSE2014.RData") ## Carrega o arquivo

base_decr_masc_endoc_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2014$n,
    obt = TabuaMascSE2014$ndx,
    lx = TabuaMascSE2014$lx,
    npx = TabuaMascSE2014$npx,
    nqx = TabuaMascSE2014$nqx,
    nax = TabuaMascSE2014$nax,
    ex = TabuaMascSE2014$ex,
    endoc = base_causas_masc_2014$endoc
  )

saveRDS(base_decr_masc_endoc_2014, file = 'base_decr_masc_endoc_2014.RData')

print(
  xtable(base_decr_masc_endoc_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(obt_si = obt - endoc)
### R_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(r_si = (obt - endoc) / obt)
base_decr_masc_endoc_2014$r_si <-
  round(base_decr_masc_endoc_2014$r_si, digits = 5)
### P_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_endoc_2014$npx_si <-
  round(base_decr_masc_endoc_2014$npx_si, digits = 5)
### nqx_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_endoc_2014$nqx_si <-
  round(base_decr_masc_endoc_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2014)
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_endoc_2014$lx_si <-
  round(base_decr_masc_endoc_2014$lx_si, digits = 0)
### nax_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_endoc_2014$nax_si[18] <-
  base_decr_masc_endoc_2014$ex[18] / base_decr_masc_endoc_2014$r_si[18]
base_decr_masc_endoc_2014$nax_si <-
  round(base_decr_masc_endoc_2014$nax_si, digits = 2)
### nLx_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_endoc_2014$nLx_si[18] <-base_decr_masc_endoc_2014$lx_si[18] * base_decr_masc_endoc_2014$nax_si[18]
base_decr_masc_endoc_2014$nLx_si <-
  round(base_decr_masc_endoc_2014$nLx_si, digits = 0)
### Tx_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_endoc_2014$Tx_si <-
  round(base_decr_masc_endoc_2014$Tx_si, digits = 0)
### ex_si
base_decr_masc_endoc_2014 <- base_decr_masc_endoc_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_endoc_2014$ex_si <-
  round(base_decr_masc_endoc_2014$ex_si, digits = 2)
saveRDS(base_decr_masc_endoc_2014, file = 'base_decr_masc_endoc_2014.RData')

print(
  xtable(
    base_decr_masc_endoc_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_endoc_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com circulatorio
TabuaMascSE2014 <-
  readRDS(file = "TabuaMascSE2014.RData") ## Carrega o arquivo

base_decr_masc_circ_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2014$n,
    obt = TabuaMascSE2014$ndx,
    lx = TabuaMascSE2014$lx,
    npx = TabuaMascSE2014$npx,
    nqx = TabuaMascSE2014$nqx,
    nax = TabuaMascSE2014$nax,
    ex = TabuaMascSE2014$ex,
    circ = base_causas_masc_2014$circ
  )

saveRDS(base_decr_masc_circ_2014, file = 'base_decr_masc_circ_2014.RData')

print(
  xtable(base_decr_masc_circ_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(obt_si = obt - circ)
### R_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(r_si = (obt - circ) / obt)
base_decr_masc_circ_2014$r_si <-
  round(base_decr_masc_circ_2014$r_si, digits = 5)
### P_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_circ_2014$npx_si <-
  round(base_decr_masc_circ_2014$npx_si, digits = 5)
### nqx_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_circ_2014$nqx_si <-
  round(base_decr_masc_circ_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2014)
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_circ_2014$lx_si <-
  round(base_decr_masc_circ_2014$lx_si, digits = 0)
### nax_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_circ_2014$nax_si[18] <-
  base_decr_masc_circ_2014$ex[18] / base_decr_masc_circ_2014$r_si[18]
base_decr_masc_circ_2014$nax_si <-
  round(base_decr_masc_circ_2014$nax_si, digits = 2)
### nLx_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_circ_2014$nLx_si[18] <-base_decr_masc_circ_2014$lx_si[18] * base_decr_masc_circ_2014$nax_si[18]
base_decr_masc_circ_2014$nLx_si <-
  round(base_decr_masc_circ_2014$nLx_si, digits = 0)
### Tx_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_circ_2014$Tx_si <-
  round(base_decr_masc_circ_2014$Tx_si, digits = 0)
### ex_si
base_decr_masc_circ_2014 <- base_decr_masc_circ_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_circ_2014$ex_si <-
  round(base_decr_masc_circ_2014$ex_si, digits = 2)
saveRDS(base_decr_masc_circ_2014, file = 'base_decr_masc_circ_2014.RData')

print(
  xtable(
    base_decr_masc_circ_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_circ_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com respirtorio
TabuaMascSE2014 <-
  readRDS(file = "TabuaMascSE2014.RData") ## Carrega o arquivo

base_decr_masc_resp_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2014$n,
    obt = TabuaMascSE2014$ndx,
    lx = TabuaMascSE2014$lx,
    npx = TabuaMascSE2014$npx,
    nqx = TabuaMascSE2014$nqx,
    nax = TabuaMascSE2014$nax,
    ex = TabuaMascSE2014$ex,
    resp = base_causas_masc_2014$resp
  )

saveRDS(base_decr_masc_resp_2014, file = 'base_decr_masc_resp_2014.RData')

print(
  xtable(base_decr_masc_resp_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(obt_si = obt - resp)
### R_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(r_si = (obt - resp) / obt)
base_decr_masc_resp_2014$r_si <-
  round(base_decr_masc_resp_2014$r_si, digits = 5)
### P_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_resp_2014$npx_si <-
  round(base_decr_masc_resp_2014$npx_si, digits = 5)
### nqx_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_resp_2014$nqx_si <-
  round(base_decr_masc_resp_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2014)
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_resp_2014$lx_si <-
  round(base_decr_masc_resp_2014$lx_si, digits = 0)
### nax_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_resp_2014$nax_si[18] <-
  base_decr_masc_resp_2014$ex[18] / base_decr_masc_resp_2014$r_si[18]
base_decr_masc_resp_2014$nax_si <-
  round(base_decr_masc_resp_2014$nax_si, digits = 2)
### nLx_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_resp_2014$nLx_si[18] <-base_decr_masc_resp_2014$lx_si[18] * base_decr_masc_resp_2014$nax_si[18]
base_decr_masc_resp_2014$nLx_si <-
  round(base_decr_masc_resp_2014$nLx_si, digits = 0)
### Tx_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_resp_2014$Tx_si <-
  round(base_decr_masc_resp_2014$Tx_si, digits = 0)
### ex_si
base_decr_masc_resp_2014 <- base_decr_masc_resp_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_resp_2014$ex_si <-
  round(base_decr_masc_resp_2014$ex_si, digits = 2)
saveRDS(base_decr_masc_resp_2014, file = 'base_decr_masc_resp_2014.RData')

print(
  xtable(
    base_decr_masc_resp_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_resp_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com externas
TabuaMascSE2014 <-
  readRDS(file = "TabuaMascSE2014.RData") ## Carrega o arquivo

base_decr_masc_ext_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuaMascSE2014$n,
    obt = TabuaMascSE2014$ndx,
    lx = TabuaMascSE2014$lx,
    npx = TabuaMascSE2014$npx,
    nqx = TabuaMascSE2014$nqx,
    nax = TabuaMascSE2014$nax,
    ex = TabuaMascSE2014$ex,
    ext = base_causas_masc_2014$ext
  )

saveRDS(base_decr_masc_ext_2014, file = 'base_decr_masc_ext_2014.RData')

print(
  xtable(base_decr_masc_ext_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(obt_si = obt - ext)
### R_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(r_si = (obt - ext) / obt)
base_decr_masc_ext_2014$r_si <-
  round(base_decr_masc_ext_2014$r_si, digits = 5)
### P_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_masc_ext_2014$npx_si <-
  round(base_decr_masc_ext_2014$npx_si, digits = 5)
### nqx_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_masc_ext_2014$nqx_si <-
  round(base_decr_masc_ext_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_masc_2014)
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_masc_ext_2014$lx_si <-
  round(base_decr_masc_ext_2014$lx_si, digits = 0)
### nax_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_masc_ext_2014$nax_si[18] <-
  base_decr_masc_ext_2014$ex[18] / base_decr_masc_ext_2014$r_si[18]
base_decr_masc_ext_2014$nax_si <-
  round(base_decr_masc_ext_2014$nax_si, digits = 2)
### nLx_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_masc_ext_2014$nLx_si[18] <-base_decr_masc_ext_2014$lx_si[18] * base_decr_masc_ext_2014$nax_si[18]
base_decr_masc_ext_2014$nLx_si <-
  round(base_decr_masc_ext_2014$nLx_si, digits = 0)
### Tx_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_masc_ext_2014$Tx_si <-
  round(base_decr_masc_ext_2014$Tx_si, digits = 0)
### ex_si
base_decr_masc_ext_2014 <- base_decr_masc_ext_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_masc_ext_2014$ex_si <-
  round(base_decr_masc_ext_2014$ex_si, digits = 2)
saveRDS(base_decr_masc_ext_2014, file = 'base_decr_masc_ext_2014.RData')

print(
  xtable(
    base_decr_masc_ext_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_masc_ext_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)










###################################################################################################################################################

#mulheres

tabua_se_fem_2014 <- data.frame(idade=factor(c('0','1-4','5-9',
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
                                 mortes=c(235, 46, 12, 27, 50, 60, 78, 96, 119, 139, 194, 237, 277, 351, 367, 448, 544, 1831),
                                 populacao=c(16713, 67053, 88583, 94174, 104802, 102134, 102497, 97312, 87548, 
                                             77032, 69532, 57467, 45831, 37217, 28558, 21020, 15122, 18495))




## Taxa de mortalidade por idade
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(nmx = mortes/populacao)
tabua_se_fem_2014$nmx <-
  round(tabua_se_fem_2014$nmx, digits = 6)

## Fator de separação
nr_linhas <- nrow(tabua_se_fem_2014)
tabua_se_fem_2014$nax <- 2.5

#### para 0a1

if(tabua_se_fem_2014$nmx[1]>=0.107){
  tabua_se_fem_2014$nax[1]=0.33
} else{
  tabua_se_fem_2014$nax[1]=0.045+2.684*tabua_se_fem_2014$nmx[1]
}
#### para 1a4
if(tabua_se_fem_2014$nmx[2]>=0.107){
  tabua_se_fem_2014$nax[2]=1.352
} else{
  tabua_se_fem_2014$nax[2] = 1.651-2.816*tabua_se_fem_2014$nmx[1]
}
#### para +a80
tabua_se_fem_2014$nax[nr_linhas] <-
  1/tabua_se_fem_2014$nmx[nr_linhas]

## Probabilidade de morte
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(nqx=(n*nmx)/(1+(n-nax)*nmx))
tabua_se_fem_2014$nqx[nr_linhas] <- 1

## Probabilidade de sobrevivência
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(npx = 1 - nqx)

## Sobrevivente à idade exata x
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(lx = 100000* cumprod(c(1, npx[-nr_linhas])))
tabua_se_fem_2014$lx <- round(tabua_se_fem_2014$lx, digits = 0)

## Óbitos na coorte de 100 mil nascidos vivos
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(ndx = c(-diff(lx), lx[nr_linhas]))
tabua_se_fem_2014$ndx <- round(tabua_se_fem_2014$ndx, digits = 0)

## Pessoas anos vividos
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(nLx = n*(lx-ndx)+nax*ndx)
tabua_se_fem_2014$nLx <- round(tabua_se_fem_2014$nLx, digits = 0)

## Pessoas anos a serem vividos
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(Tx = sum(nLx) - cumsum(nLx) + nLx)

## Esperança de vida ao nascer
tabua_se_fem_2014 <- tabua_se_fem_2014 %>%
  mutate(ex = Tx/lx)
tabua_se_fem_2014$ex <- round(tabua_se_fem_2014$ex, digits = 1)

print(
  xtable(tabua_se_fem_2014, digits = c(0, 0, 0, 0, 0, 0, 4, 2, 3, 3, 0, 0,0,0,2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


#gráfico nqx

nqx_ser14_fem <- ggplot(tabua_se_fem_2014, aes(x = idade, y = round(log(nqx),3), group = 1)) +
  geom_line() +  
  geom_point()+
  ggtitle("Sexo feminino, Sergipe 2014")+
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


nqx14 <- (nqx_ser14_masc/nqx_ser14_fem)+
  theme(text = element_text(colour = "black"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        # plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 30),
        legend.position = "bottom",
        legend.text = element_text(size = 11))

ggraficos <- "c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/graficos/"


ggsave(paste0(ggraficos,'nqx14.pdf'),
       plot = nqx14,
       width = 30, 
       height = 20.25,
       units = "in")

saveRDS(tabua_se_fem_2014,"TabuafemSE2014.RData")

### causa excluída

## importa a base com causas específicas ---
causas_fem_2014 <- read_excel("c:/Users/roney/OneDrive/Área de Trabalho/TCC CORRETO/obitos_sergipe_cid10_2014_fem.xlsx")

colnames(causas_fem_2014) <-
  c('idade',
    'cap1',
    'cap2',
    'cap4',
    'cap9',
    'cap10',
    'cap20')
base_causas_fem_2014 <- causas_fem_2014 %>%
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

base_causas_fem_2014$cap1 <- as.numeric(base_causas_fem_2014$cap1)
base_causas_fem_2014$cap2 <- as.numeric(base_causas_fem_2014$cap2)
base_causas_fem_2014$cap4 <- as.numeric(base_causas_fem_2014$cap4)
base_causas_fem_2014$cap9 <- as.numeric(base_causas_fem_2014$cap9)
base_causas_fem_2014$cap10 <-
  as.numeric(base_causas_fem_2014$cap10)
base_causas_fem_2014$cap20 <-
  as.numeric(base_causas_fem_2014$cap20)
base_causas_fem_2014[is.na(base_causas_fem_2014)] <- 0
base_causas_fem_2014 <-
  base_causas_fem_2014 %>% group_by(gr_idade) %>%
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
    base_causas_fem_2014,
    digits = c(0, 0, 0, 0, 0, 0, 0,0)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

str(base_causas_fem_2014)

save(base_causas_fem_2014, file = 'base_causas_fem_2014.RData')

### Montando a tábua com doenças infecciosas
TabuafemSE2014 <-
  readRDS(file = "TabuafemSE2014.RData") ## Carrega o arquivo

base_decr_fem_infec_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2014$n,
    obt = TabuafemSE2014$ndx,
    lx = TabuafemSE2014$lx,
    npx = TabuafemSE2014$npx,
    nqx = TabuafemSE2014$nqx,
    nax = TabuafemSE2014$nax,
    ex = TabuafemSE2014$ex,
    infec = base_causas_fem_2014$infec
  )

saveRDS(base_decr_fem_infec_2014, file = 'base_decr_fem_infec_2014.RData')

print(
  xtable(base_decr_fem_infec_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(obt_si = obt - infec)
### R_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(r_si = (obt - infec) / obt)
base_decr_fem_infec_2014$r_si <-
  round(base_decr_fem_infec_2014$r_si, digits = 5)
### P_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_infec_2014$npx_si <-
  round(base_decr_fem_infec_2014$npx_si, digits = 5)
### nqx_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_infec_2014$nqx_si <-
  round(base_decr_fem_infec_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2014)
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_infec_2014$lx_si <-
  round(base_decr_fem_infec_2014$lx_si, digits = 0)
### nax_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_infec_2014$nax_si[18] <-
  base_decr_fem_infec_2014$ex[18] / base_decr_fem_infec_2014$r_si[18]
base_decr_fem_infec_2014$nax_si <-
  round(base_decr_fem_infec_2014$nax_si, digits = 2)
### nLx_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_infec_2014$nLx_si[18] <-base_decr_fem_infec_2014$lx_si[18] * base_decr_fem_infec_2014$nax_si[18]
base_decr_fem_infec_2014$nLx_si <-
  round(base_decr_fem_infec_2014$nLx_si, digits = 0)
### Tx_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_infec_2014$Tx_si <-
  round(base_decr_fem_infec_2014$Tx_si, digits = 0)
### ex_si
base_decr_fem_infec_2014 <- base_decr_fem_infec_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_infec_2014$ex_si <-
  round(base_decr_fem_infec_2014$ex_si, digits = 2)
saveRDS(base_decr_fem_infec_2014, file = 'base_decr_fem_infec_2014.RData')

print(
  xtable(
    base_decr_fem_infec_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_infec_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com neoplasias
TabuafemSE2014 <-
  readRDS(file = "TabuafemSE2014.RData") ## Carrega o arquivo

base_decr_fem_neop_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2014$n,
    obt = TabuafemSE2014$ndx,
    lx = TabuafemSE2014$lx,
    npx = TabuafemSE2014$npx,
    nqx = TabuafemSE2014$nqx,
    nax = TabuafemSE2014$nax,
    ex = TabuafemSE2014$ex,
    neop = base_causas_fem_2014$neop
  )

saveRDS(base_decr_fem_neop_2014, file = 'base_decr_fem_neop_2014.RData')

print(
  xtable(base_decr_fem_neop_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(obt_si = obt - neop)
### R_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(r_si = (obt - neop) / obt)
base_decr_fem_neop_2014$r_si <-
  round(base_decr_fem_neop_2014$r_si, digits = 5)
### P_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_neop_2014$npx_si <-
  round(base_decr_fem_neop_2014$npx_si, digits = 5)
### nqx_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_neop_2014$nqx_si <-
  round(base_decr_fem_neop_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2014)
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_neop_2014$lx_si <-
  round(base_decr_fem_neop_2014$lx_si, digits = 0)
### nax_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_neop_2014$nax_si[18] <-
  base_decr_fem_neop_2014$ex[18] / base_decr_fem_neop_2014$r_si[18]
base_decr_fem_neop_2014$nax_si <-
  round(base_decr_fem_neop_2014$nax_si, digits = 2)
### nLx_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_neop_2014$nLx_si[18] <-base_decr_fem_neop_2014$lx_si[18] * base_decr_fem_neop_2014$nax_si[18]
base_decr_fem_neop_2014$nLx_si <-
  round(base_decr_fem_neop_2014$nLx_si, digits = 0)
### Tx_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_neop_2014$Tx_si <-
  round(base_decr_fem_neop_2014$Tx_si, digits = 0)
### ex_si
base_decr_fem_neop_2014 <- base_decr_fem_neop_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_neop_2014$ex_si <-
  round(base_decr_fem_neop_2014$ex_si, digits = 2)
saveRDS(base_decr_fem_neop_2014, file = 'base_decr_fem_neop_2014.RData')

print(
  xtable(
    base_decr_fem_neop_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_neop_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com endocrino
TabuafemSE2014 <-
  readRDS(file = "TabuafemSE2014.RData") ## Carrega o arquivo

base_decr_fem_endoc_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2014$n,
    obt = TabuafemSE2014$ndx,
    lx = TabuafemSE2014$lx,
    npx = TabuafemSE2014$npx,
    nqx = TabuafemSE2014$nqx,
    nax = TabuafemSE2014$nax,
    ex = TabuafemSE2014$ex,
    endoc = base_causas_fem_2014$endoc
  )

saveRDS(base_decr_fem_endoc_2014, file = 'base_decr_fem_endoc_2014.RData')

print(
  xtable(base_decr_fem_endoc_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(obt_si = obt - endoc)
### R_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(r_si = (obt - endoc) / obt)
base_decr_fem_endoc_2014$r_si <-
  round(base_decr_fem_endoc_2014$r_si, digits = 5)
### P_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_endoc_2014$npx_si <-
  round(base_decr_fem_endoc_2014$npx_si, digits = 5)
### nqx_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_endoc_2014$nqx_si <-
  round(base_decr_fem_endoc_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2014)
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_endoc_2014$lx_si <-
  round(base_decr_fem_endoc_2014$lx_si, digits = 0)
### nax_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_endoc_2014$nax_si[18] <-
  base_decr_fem_endoc_2014$ex[18] / base_decr_fem_endoc_2014$r_si[18]
base_decr_fem_endoc_2014$nax_si <-
  round(base_decr_fem_endoc_2014$nax_si, digits = 2)
### nLx_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_endoc_2014$nLx_si[18] <-base_decr_fem_endoc_2014$lx_si[18] * base_decr_fem_endoc_2014$nax_si[18]
base_decr_fem_endoc_2014$nLx_si <-
  round(base_decr_fem_endoc_2014$nLx_si, digits = 0)
### Tx_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_endoc_2014$Tx_si <-
  round(base_decr_fem_endoc_2014$Tx_si, digits = 0)
### ex_si
base_decr_fem_endoc_2014 <- base_decr_fem_endoc_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_endoc_2014$ex_si <-
  round(base_decr_fem_endoc_2014$ex_si, digits = 2)
saveRDS(base_decr_fem_endoc_2014, file = 'base_decr_fem_endoc_2014.RData')

print(
  xtable(
    base_decr_fem_endoc_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_endoc_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com circulatorio
TabuafemSE2014 <-
  readRDS(file = "TabuafemSE2014.RData") ## Carrega o arquivo

base_decr_fem_circ_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2014$n,
    obt = TabuafemSE2014$ndx,
    lx = TabuafemSE2014$lx,
    npx = TabuafemSE2014$npx,
    nqx = TabuafemSE2014$nqx,
    nax = TabuafemSE2014$nax,
    ex = TabuafemSE2014$ex,
    circ = base_causas_fem_2014$circ
  )

saveRDS(base_decr_fem_circ_2014, file = 'base_decr_fem_circ_2014.RData')

print(
  xtable(base_decr_fem_circ_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(obt_si = obt - circ)
### R_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(r_si = (obt - circ) / obt)
base_decr_fem_circ_2014$r_si <-
  round(base_decr_fem_circ_2014$r_si, digits = 5)
### P_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_circ_2014$npx_si <-
  round(base_decr_fem_circ_2014$npx_si, digits = 5)
### nqx_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_circ_2014$nqx_si <-
  round(base_decr_fem_circ_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2014)
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_circ_2014$lx_si <-
  round(base_decr_fem_circ_2014$lx_si, digits = 0)
### nax_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_circ_2014$nax_si[18] <-
  base_decr_fem_circ_2014$ex[18] / base_decr_fem_circ_2014$r_si[18]
base_decr_fem_circ_2014$nax_si <-
  round(base_decr_fem_circ_2014$nax_si, digits = 2)
### nLx_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_circ_2014$nLx_si[18] <-base_decr_fem_circ_2014$lx_si[18] * base_decr_fem_circ_2014$nax_si[18]
base_decr_fem_circ_2014$nLx_si <-
  round(base_decr_fem_circ_2014$nLx_si, digits = 0)
### Tx_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_circ_2014$Tx_si <-
  round(base_decr_fem_circ_2014$Tx_si, digits = 0)
### ex_si
base_decr_fem_circ_2014 <- base_decr_fem_circ_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_circ_2014$ex_si <-
  round(base_decr_fem_circ_2014$ex_si, digits = 2)
saveRDS(base_decr_fem_circ_2014, file = 'base_decr_fem_circ_2014.RData')

print(
  xtable(
    base_decr_fem_circ_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_circ_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com respirtorio
TabuafemSE2014 <-
  readRDS(file = "TabuafemSE2014.RData") ## Carrega o arquivo

base_decr_fem_resp_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2014$n,
    obt = TabuafemSE2014$ndx,
    lx = TabuafemSE2014$lx,
    npx = TabuafemSE2014$npx,
    nqx = TabuafemSE2014$nqx,
    nax = TabuafemSE2014$nax,
    ex = TabuafemSE2014$ex,
    resp = base_causas_fem_2014$resp
  )

saveRDS(base_decr_fem_resp_2014, file = 'base_decr_fem_resp_2014.RData')

print(
  xtable(base_decr_fem_resp_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(obt_si = obt - resp)
### R_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(r_si = (obt - resp) / obt)
base_decr_fem_resp_2014$r_si <-
  round(base_decr_fem_resp_2014$r_si, digits = 5)
### P_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_resp_2014$npx_si <-
  round(base_decr_fem_resp_2014$npx_si, digits = 5)
### nqx_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_resp_2014$nqx_si <-
  round(base_decr_fem_resp_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2014)
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_resp_2014$lx_si <-
  round(base_decr_fem_resp_2014$lx_si, digits = 0)
### nax_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_resp_2014$nax_si[18] <-
  base_decr_fem_resp_2014$ex[18] / base_decr_fem_resp_2014$r_si[18]
base_decr_fem_resp_2014$nax_si <-
  round(base_decr_fem_resp_2014$nax_si, digits = 2)
### nLx_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_resp_2014$nLx_si[18] <-base_decr_fem_resp_2014$lx_si[18] * base_decr_fem_resp_2014$nax_si[18]
base_decr_fem_resp_2014$nLx_si <-
  round(base_decr_fem_resp_2014$nLx_si, digits = 0)
### Tx_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_resp_2014$Tx_si <-
  round(base_decr_fem_resp_2014$Tx_si, digits = 0)
### ex_si
base_decr_fem_resp_2014 <- base_decr_fem_resp_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_resp_2014$ex_si <-
  round(base_decr_fem_resp_2014$ex_si, digits = 2)
saveRDS(base_decr_fem_resp_2014, file = 'base_decr_fem_resp_2014.RData')

print(
  xtable(
    base_decr_fem_resp_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_resp_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


### Montando a tábua com externas
TabuafemSE2014 <-
  readRDS(file = "TabuafemSE2014.RData") ## Carrega o arquivo

base_decr_fem_ext_2014 <-
  data.frame(
    gr_idade = as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
    n = TabuafemSE2014$n,
    obt = TabuafemSE2014$ndx,
    lx = TabuafemSE2014$lx,
    npx = TabuafemSE2014$npx,
    nqx = TabuafemSE2014$nqx,
    nax = TabuafemSE2014$nax,
    ex = TabuafemSE2014$ex,
    ext = base_causas_fem_2014$ext
  )

saveRDS(base_decr_fem_ext_2014, file = 'base_decr_fem_ext_2014.RData')

print(
  xtable(base_decr_fem_ext_2014, digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0)),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)

### d_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(obt_si = obt - ext)
### R_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(r_si = (obt - ext) / obt)
base_decr_fem_ext_2014$r_si <-
  round(base_decr_fem_ext_2014$r_si, digits = 5)
### P_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(npx_si = npx ^ r_si)
base_decr_fem_ext_2014$npx_si <-
  round(base_decr_fem_ext_2014$npx_si, digits = 5)
### nqx_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(nqx_si = 1 - (npx ^ r_si))

base_decr_fem_ext_2014$nqx_si <-
  round(base_decr_fem_ext_2014$nqx_si, digits = 5)
### lx_si
nr_linhas <- nrow(base_causas_fem_2014)
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(lx_si = 100000 * cumprod(c(1, npx_si[-nr_linhas])))
base_decr_fem_ext_2014$lx_si <-
  round(base_decr_fem_ext_2014$lx_si, digits = 0)
### nax_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(nax_si = ifelse(
    gr_idade <= 5 | gr_idade == 80,
    n + (r_si * (nqx / nqx_si) * (nax - n)),
    ifelse(
      gr_idade > 5 | gr_idade < 80,
      ((-5 / 24) * lag(obt_si) + 2.5 * obt_si + (5 /24) * lead(obt_si)) / obt_si,
      'erro'
    )
  ))

base_decr_fem_ext_2014$nax_si[18] <-
  base_decr_fem_ext_2014$ex[18] / base_decr_fem_ext_2014$r_si[18]
base_decr_fem_ext_2014$nax_si <-
  round(base_decr_fem_ext_2014$nax_si, digits = 2)
### nLx_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(nLx_si = (n * (lx_si - obt_si) + nax_si * obt_si))
base_decr_fem_ext_2014$nLx_si[18] <-base_decr_fem_ext_2014$lx_si[18] * base_decr_fem_ext_2014$nax_si[18]
base_decr_fem_ext_2014$nLx_si <-
  round(base_decr_fem_ext_2014$nLx_si, digits = 0)
### Tx_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(Tx_si = sum(nLx_si) - cumsum(nLx_si) + nLx_si)
base_decr_fem_ext_2014$Tx_si <-
  round(base_decr_fem_ext_2014$Tx_si, digits = 0)
### ex_si
base_decr_fem_ext_2014 <- base_decr_fem_ext_2014 %>%
  mutate(ex_si = Tx_si / lx_si)
base_decr_fem_ext_2014$ex_si <-
  round(base_decr_fem_ext_2014$ex_si, digits = 2)
saveRDS(base_decr_fem_ext_2014, file = 'base_decr_fem_ext_2014.RData')

print(
  xtable(
    base_decr_fem_ext_2014,
    digits = c(0, 0, 0, 0, 0, 5, 2, 2, 2, 0, 0, 5, 5, 5, 0, 2, 0, 0, 2)
  ),
  include.rownames = FALSE,
  format.args = list(big.mark = ".", decimal.mark = ',')
)
## seleciona colunas para tabela do markdown
tabua <- base_decr_fem_ext_2014 %>%
  select(1, 8, 10:18)
saveRDS(tabua, file = 'tabua.RData')

print(
  xtable(tabua, digits = c(0, 0, 2, 0, 5, 5, 5, 0, 2, 0, 0, 2)),
  include.rownames = F,
  format.args = list(big.mark = '.', decimal.mark = ',')
)


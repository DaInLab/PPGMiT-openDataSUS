# Universidade Estadual Paulista - UNESP
# PPGMiT FAAC-UNESP
# Projeto de Mestrado Profissional
# Author: Marcelo Santos
# Fonte : https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023
# Advisor: Joao Pedro Albino
# Alterações feitas em: 25/04/2023

# Carregando as bibliotecas necessárias
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")   
if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
if (!("readr") %in% installed.packages()) install.packages("readr")
library(ggplot2) # graficos
library(dplyr)   # manipulacao de dados
library(readr) 

# Diretorio da Trabalho
setwd('D:/GoogleDrive/Mestrado_unesp/PPGMiT-openDataSUS')

# Importacao dos arquivos anuais : INFLUD* 2021, 2022 e 2023 (2.351.198)
df_opendatasus<-list.files(path="./dados",pattern="*.csv", full.names = TRUE) %>%
  lapply(read.csv,stringsAsFactors=F, sep=';') %>%
  bind_rows

# Analise do Dataframe
class(df_opendatasus)
str(df_opendatasus) # apenas int e char
summary(df_opendatasus)

# Variaveis que serao utilizadas :
# Filtros : 78-Classificacao Final do caso (5-SRAG por COVID-19)
#           80- Evolucaçao do caso    (2-Óbito)

df_covid_mortalidade <- df_opendatasus %>% select(NU_IDADE_N,
                                      CS_SEXO,
                                      CS_GESTANT,
                                      CS_ESCOL_N,
                                      #Classificacao e Evolucao
                                      CLASSI_FIN, 
                                      EVOLUCAO,
                                      #Internacao
                                      HOSPITAL,
                                      DT_INTERNA,
                                      SG_UF_INTE,
                                      ID_MN_INTE,
                                      CO_MU_INTE,
                                      UTI,
                                      DT_ENTUTI,
                                      DT_SAIDUTI,
                                      #Sintomas
                                      FEBRE,
                                      TOSSE,
                                      GARGANTA,
                                      DISPNEIA,
                                      DESC_RESP,
                                      SATURACAO,
                                      DIARREIA,
                                      VOMITO,
                                      DOR_ABD,
                                      FADIGA,
                                      PERD_OLFT,
                                      PERD_PALA,
                                      #OUTROS_SIN,
                                      #OUTROS_DES,
                                      #Fator de Risco
                                      FATOR_RISC,
                                      PUERPERA,
                                      #CARDIOPAT,
                                      HEMATOLOGI,
                                      SIND_DOWN,
                                      HEPATICA,
                                      ASMA,
                                      DIABETES,
                                      NEUROLOGIC,
                                      PNEUMOPATI,
                                      IMUNODEPRE,
                                      RENAL,
                                      OBESIDADE,
                                      OBES_IMC,
                                      OUT_MORBI,
                                      #MORD_DESC,
                                      #Obito
                                      DT_EVOLUCA,
                                      DT_ENCERRA)  %>% filter(CLASSI_FIN == "5", EVOLUCAO == "2") 


View(df_covid_mortalidade)
summary(df_covid_mortalidade)



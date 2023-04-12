# Universidade Estadual Paulista - UNESP
# PPGMiT FAAC-UNESP
#
# Prof. Joao Pedro Albino
#
#           Marcelo Santos
# Fonte : https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023


<<<<<<< HEAD
# Import packages
#library(ggplot2) # graficos
=======
# Carregando as bibliotecas necessárias
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")   
if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
if (!("readr") %in% installed.packages()) install.packages("readr")
library(ggplot2) # graficos
>>>>>>> 9cb3c0a (Atualizando via MacOsx: 2023-04-11.)
library(dplyr) # manipulacao de dados
library(readr) 

# Diretorio da Trabalho
<<<<<<< HEAD
setwd('D:/GoogleDrive/Mestrado_unesp/PPGMiT-openDataSUS')
=======
#setwd('D:/GoogleDrive/Mestrado_unesp/openDataSUS')
>>>>>>> 9cb3c0a (Atualizando via MacOsx: 2023-04-11.)

# Importacao dos arquivos anuais : INFLUD* 2021, 2022 e 2023 (2.339.381)
df_opendatasus<-list.files(path="./dados",pattern="*.csv", full.names = TRUE) %>%
  lapply(read.csv,stringsAsFactors=F, sep=';') %>%
  bind_rows

# Analise do Dataframe
class(df_opendatasus)
View(df_opendatasus)
str(df_opendatasus) # apenas int e char
summary(df_opendatasus)

# Filtro - 78-Classificacao Final do caso (5-SRAG por COVID-19)
df_covid <- df_opendatasus %>% filter(CLASSI_FIN == "5")
<<<<<<< HEAD


=======
View(df_covid)
summary(df_covid)
>>>>>>> 9cb3c0a (Atualizando via MacOsx: 2023-04-11.)





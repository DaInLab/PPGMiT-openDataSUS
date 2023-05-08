# Universidade Estadual Paulista - UNESP
# PPGMiT FAAC-UNESP
# Projeto de Mestrado Profissional
# Author: Marcelo Santos
# Fonte : https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023
# Advisor: Joao Pedro Albino
# Alterações feitas em: 07/05/2023

# Diretorio da Trabalho
setwd('D:/GoogleDrive/Mestrado_unesp/PPGMiT-openDataSUS')

# Carregando as bibliotecas necessárias
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")   
if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
if (!("readr") %in% installed.packages()) install.packages("readr")
if (!("geobr") %in% installed.packages()) install.packages("geobr")
if (!("scales") %in% installed.packages()) install.packages("scales")
if (!("ggspatial") %in% installed.packages()) install.packages("ggspatial")

library(ggplot2) # graficos
library(dplyr)   # manipulacao de dados
library(readr) 
library(geobr)
library(scales)
library(ggspatial)

# Importacao dos arquivos anuais : INFLUD* 2021, 2022 e 2023 (2.351.198)
df_opendatasus<-list.files(path="./dados",pattern="*.csv", full.names = TRUE) %>%
  lapply(read.csv,stringsAsFactors=F, sep=';') %>%
  bind_rows

# Analise do Dataframe
class(df_opendatasus)
str(df_opendatasus) # apenas int e char
summary(df_opendatasus)

# Criando Variavel Ano Notificacao
ANO_NOTIFIC <- format(as.Date(df_opendatasus$DT_NOTIFIC, format='%d/%m/%Y'), format="%Y")
df_opendatasus['ANO_NOTIFIC'] <- ANO_NOTIFIC
rm(ANO_NOTIFIC)

# Criando Variavel Ano Evolucao
ANO_EVOLUCA <- format(as.Date(df_opendatasus$DT_EVOLUCA, format='%d/%m/%Y'), format="%Y")
df_opendatasus['ANO_EVOLUCA'] <- ANO_EVOLUCA
rm(ANO_EVOLUCA)

# Criando Variavel Ano Internacao
ANO_INTERNA <- format(as.Date(df_opendatasus$DT_INTERNA, format='%d/%m/%Y'), format="%Y")
df_opendatasus['ANO_INTERNA'] <- ANO_INTERNA
rm(ANO_INTERNA)

# Criando Variavel Ano Encerramento
ANO_ENCERRA <- format(as.Date(df_opendatasus$DT_ENCERRA, format='%d/%m/%Y'), format="%Y")
df_opendatasus['ANO_ENCERRA'] <- ANO_ENCERRA
rm(ANO_ENCERRA)

# Numero Total de Notificacoes 
num_notificacoes <- as.numeric(count(df_opendatasus))
num_notificacoes # 2351198

num_notificacoes_ano <- aggregate(df_opendatasus$ANO_NOTIFIC,by=list(df_opendatasus$ANO_NOTIFIC), FUN=length)
num_notificacoes_ano <- setNames(num_notificacoes_ano, c("Ano", "Qtde"))
num_notificacoes_ano
#  Ano    Qtde
# 1 2021 1703789
# 2 2022  579161
# 3 2023   68243

# 2021
num_notificacoes_2021 <- as.numeric(num_notificacoes_ano[num_notificacoes_ano$Ano == "2021",2])
num_notificacoes_2021 # 1703789

# 2022
num_notificacoes_2022 <- as.numeric(num_notificacoes_ano[num_notificacoes_ano$Ano == "2022",2])
num_notificacoes_2022 # 579161

# 2023
num_notificacoes_2023 <- as.numeric(num_notificacoes_ano[num_notificacoes_ano$Ano == "2023",2])
num_notificacoes_2023 # 68243

# Numero de notificacoes diferentes de 2021, 2022, 2023
num_notificacoes_dif <- as.numeric(nrow(df_opendatasus[df_opendatasus$ANO_NOTIFIC !='2021' & df_opendatasus$ANO_NOTIFIC !='2022' & df_opendatasus$ANO_NOTIFIC !='2023' ,]))
# 5


# Variaveis que serao utilizadas :
#   Periodo - Ano
#   Classificacao e Evolucao
#   Dados Gerais
#   Internacao
#   Sintomas
#   Fatores de Risco
#   
# Filtros : Ano Notificacao : 2021, 2022 e 2023
#           Ano Evolucao    : 2021, 2022 e 2023
#           Ano Internacao    : Maior igual Ano Notificacao 
#           78-Classificacao Final do caso (5-SRAG por COVID-19)
#           80- Evolucaçao do caso    (2-Óbito)
#           

# 442.890 obs - 51 variaveis
df_covid_mortalidade <- df_opendatasus %>% select(
                                      # Periodo - Ano
                                      ANO_NOTIFIC,
                                      ANO_INTERNA,
                                      ANO_EVOLUCA, # Ano Obito
                                      ANO_ENCERRA,
                                      # Classificacao e Evolucao
                                      CLASSI_FIN, 
                                      EVOLUCAO,
                                      # Dados Gerais
                                      NU_IDADE_N,
                                      CS_SEXO,
                                      CS_GESTANT,
                                      CS_ESCOL_N,
                                      CS_RACA,
                                      # Notificacao
                                      ID_MUNICIP,
                                      CO_MUN_NOT,
                                      SG_UF_NOT,
                                      # Internacao
                                      HOSPITAL,
                                      SG_UF_INTE,
                                      ID_MN_INTE,
                                      CO_MU_INTE,
                                      UTI,
                                      DT_ENTUTI,
                                      DT_SAIDUTI,
                                      # Sintomas
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
                                      OUTRO_SIN,
                                      OUTRO_DES,
                                      #Fator de Risco
                                      FATOR_RISC,
                                      PUERPERA,
                                      CARDIOPATI,
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
                                      MORB_DESC
                                      )  %>% filter((ANO_NOTIFIC == "2021" | ANO_NOTIFIC == "2022" | ANO_NOTIFIC == "2023"),
                                                    (ANO_EVOLUCA == "2021" | ANO_EVOLUCA == "2022" | ANO_EVOLUCA == "2023"),
                                                    CLASSI_FIN == "5", 
                                                    EVOLUCAO == "2") 

names(df_covid_mortalidade)
summary(df_covid_mortalidade)


# Verificacao de Todas as variaveis

# ANO_NOTIFIC - Ano Notificacao
distinct(tbl_df(df_covid_mortalidade$ANO_NOTIFIC))
# 2021 2022 2023

# ANO_INTERNA - Ano Internacao
distinct(tbl_df(df_covid_mortalidade$ANO_INTERNA))
# 2021 NA 2020 2022 2023

# ANO_EVOLUCAO - Ano Evolucao, no caso Ano do Obito
distinct(tbl_df(df_covid_mortalidade$ANO_EVOLUCA))
# 2021 NA 2022 2023

# ANO_ENCERRA - Ano Encerramento da Notificacao
distinct(tbl_df(df_covid_mortalidade$ANO_ENCERRA))
# 2021 NA 2022 2023

# CLASSI_FIN - Classificacao Final do caso (5-SRAG por COVID-19)
distinct(tbl_df(df_covid_mortalidade$CLASSI_FIN))
# 5

# EVOLUC - Evolucaçao do caso    (2-Óbito)
distinct(tbl_df(df_covid_mortalidade$EVOLUC))
# 2

# NU_IDADE_N
distinct(tbl_df(df_covid_mortalidade$NU_IDADE_N))

# CS_SEXO
distinct(tbl_df(df_covid_mortalidade$CS_SEXO))
# 1 M    
# 2 F    
# 3 I  

# CS_GESTANT
distinct(tbl_df(df_covid_mortalidade$CS_GESTANT))
# 1     6
# 2     5
# 3     9
# 4     2
# 5     3
# 6     0
# 7     1
# 8     4

# CS_ESCOL_N,
distinct(tbl_df(df_covid_mortalidade$CS_ESCOL_N))
# 1     9
# 2     1
# 3     0
# 4    NA
# 5     4
# 6     3
# 7     2
# 8     5

# CS_RACA
distinct(tbl_df(df_covid_mortalidade$CS_RACA))
# 1     4
# 2     9
# 3     1
# 4     2
# 5     3
# 6     5


# Grafico 01_mapa_Obitos_BR
estado <- read_state(year=2020)

num_obitos_estado <- aggregate(df_covid_mortalidade$SG_UF_INTE,by=list(df_covid_mortalidade$SG_UF_INTE), FUN=length)
num_obitos_estado <- setNames(num_obitos_estado, c("abbrev_state", "Qtde"))
num_obitos_estado

ggplot()+
  geom_sf(data=dados_mapa, aes(fill=Qtde), color = 'gray')+
  scale_fill_distiller(palette="Reds", direction = 1, name= "Num Óbitos", labels = label_number(drop0trailing = T, big.mark = ""))+
  labs(x=NULL, y=NULL)+
  labs(title = 'Mapa de Óbitos por Estado', size=10)+
  geom_sf_text(data=mapa, aes(label = abbrev_state), size = 3.5, color = "black")+
  scale_size_continuous(labels= comma)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
  annotation_scale()+
  annotation_north_arrow(location='tl')

# Grafico 02 : Mapa de Obitos por Municipio
municipios <- read_municipality(year=2020)

municipios
  
num_obitos_municipio <- df_covid_mortalidade %>%
                          count(ID_MUNICIP, SG_UF_NOT,CO_MUN_NOT, sort=TRUE)

municipios

filter(municipios, name_muni == 'Bauru')
filter(num_obitos_municipio, ID_MUNICIP == 'BAURU') # 350600 

# Liberar memoria
rm(df_opendatasus)

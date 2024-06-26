# Universidade Estadual Paulista - UNESP
# PPGMiT FAAC-UNESP
# Projeto de Mestrado Profissional
# Author: Marcelo Santos
# Fonte : https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023
# Advisor: Joao Pedro Albino
# Alterações feitas em: 25/06/2023

# Diretorio da Trabalho
setwd('D:/GoogleDrive/Mestrado/PPGMiT-openDataSUS')

# Carregando as bibliotecas necessárias
if (!("ggplot2") %in% installed.packages()) install.packages("ggplot2")   
if (!("dplyr") %in% installed.packages()) install.packages("dplyr")
if (!("readr") %in% installed.packages()) install.packages("readr")
if (!("geobr") %in% installed.packages()) install.packages("geobr")
if (!("scales") %in% installed.packages()) install.packages("scales")
if (!("ggspatial") %in% installed.packages()) install.packages("ggspatial")
if (!("zoo") %in% installed.packages()) install.packages("zoo")

library(ggplot2) # visualizacao dos dados
library(dplyr)   # preparacao dos dados
library(readr)   # importacao dos dados
library(lubridate) # Datas
library(geobr)   # mapa BR
library(scales)  # mapa BR
library(ggspatial) # mapa BR
library(zoo) # calcular rollmean

# Dataframe : df_opendatasus
# Todos os registros do OPENDATASUS (sem filtro)
# Importacao dos arquivos anuais : INFLUD* 2021, 2022 e 2023 (2.430.671)
df_opendatasus<-list.files(path="./dados",pattern="INFLUD", full.names = TRUE) %>%
  lapply(read.csv,stringsAsFactors=F, sep=';') %>%
  bind_rows

# Ajustando tipo das Variaveis Data
df_opendatasus$DT_NOTIFIC <- format(as.Date(df_opendatasus$DT_NOTIFIC, format='%d/%m/%Y'))
df_opendatasus$DT_INTERNA <- format(as.Date(df_opendatasus$DT_INTERNA, format='%d/%m/%Y'))
df_opendatasus$DT_EVOLUCA <- format(as.Date(df_opendatasus$DT_EVOLUCA, format='%d/%m/%Y'))
df_opendatasus$DT_ENCERRA <- format(as.Date(df_opendatasus$DT_ENCERRA, format='%d/%m/%Y'))

# Criando Variaveis : Ano Notificacao, Ano Evolucao, Ano Internacao e Ano Encerramento do Caso
df_opendatasus['ANO_NOTIFIC'] <- year(df_opendatasus$DT_NOTIFIC)
df_opendatasus['ANO_EVOLUCA'] <- year(df_opendatasus$DT_EVOLUCA)
df_opendatasus['ANO_INTERNA'] <- year(df_opendatasus$DT_INTERNA)
df_opendatasus['ANO_ENCERRA'] <- year(df_opendatasus$DT_ENCERRA)

# Dataframe : df_covid (1.327.358 - 55 variaveis)
# Variaveis que serao utilizadas :
#   Periodo - Ano
#   Classificacao e Evolucao
#   Dados Gerais
#   Internacao
#   Sintomas
#   Fatores de Risco
#   
# Filtros : 78-Classificacao Final do caso (5-SRAG por COVID-19)
#           Ano Notificacoes Ano : 2021, 2022, 2023
#           Ano Evolucao : 2021, 2022, 2023
# 
df_covid <- df_opendatasus %>% select(
  # DataS 
  DT_NOTIFIC,
  DT_INTERNA,
  DT_EVOLUCA,
  DT_ENCERRA,
  # Anos
  ANO_NOTIFIC,
  ANO_INTERNA,
  ANO_EVOLUCA,
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
              CLASSI_FIN == "5")   %>% arrange(DT_NOTIFIC)
# Liberar memoria
rm(df_opendatasus)

# Criando arquivo do dataframe df_covid (Classificacao Final : SRAG por COVID-19)
write_csv2(df_covid, file='./dados/COVID.csv')

# Dataframe : df_covid_2021
df_covid_2021 <- df_covid %>% filter(ANO_EVOLUCA == "2021")

# Dataframe : df_covid_2022
df_covid_2022 <- df_covid %>% filter(ANO_EVOLUCA == "2022")

# Dataframe : df_covid_2023
df_covid_2023 <- df_covid %>% filter(ANO_EVOLUCA == "2023")

# Dataframe : df_covid_obitos (444.951 - 55 variaveis)
# Variaveis que serao utilizadas :
#   Periodo - Ano
#   Classificacao e Evolucao
#   Dados Gerais
#   Internacao
#   Sintomas
#   Fatores de Risco
#   
# Filtros : 80- Evolucaçao do caso    (2-Óbito) no dataframe df_covid
#           
df_obitos <- df_covid %>% filter(EVOLUCAO == "2")

# # Criando arquivo do dataframe df_covid_obitos (Classificacao Final : SRAG por COVID-19 e Evolucao do Caso : Obito)
write_csv2(df_obitos, file='./dados/COVID_OBITOS.csv')

# Dataframe : df_covid_obitos_2021
df_obitos_2021 <- df_obitos %>% filter(ANO_EVOLUCA == "2021")

# Dataframe : df_covid_obitos_2022
df_obitos_2022 <- df_obitos %>% filter(ANO_EVOLUCA == "2022")

# Dataframe : df_covid_obitos_2023
df_obitos_2023 <- df_obitos %>% filter(ANO_EVOLUCA == "2023")

# Numero Total de Notificacoes COVID
num_notificacoes <- as.numeric(count(df_covid))
num_notificacoes # 1.327.358

df_notificacoes_ano <- aggregate(df_covid$ANO_NOTIFIC,by=list(df_covid$ANO_NOTIFIC), FUN=length)
df_notificacoes_ano <- setNames(df_notificacoes_ano, c("Ano", "Qtde"))
df_notificacoes_ano

# 2021
num_notificacoes_2021 <- as.numeric(df_notificacoes_ano$Qtde[df_notificacoes_ano$Ano == '2021'])
# 2022
num_notificacoes_2022 <- as.numeric(df_notificacoes_ano$Qtde[df_notificacoes_ano$Ano == '2022'])
# 2023
num_notificacoes_2023 <- as.numeric(df_notificacoes_ano$Qtde[df_notificacoes_ano$Ano == '2023'])

# Numero Total de Obitos COVID
num_obitos <- as.numeric(count(df_obitos))
num_obitos # 444.951

num_obitos_2021 <- as.numeric(count(df_obitos_2021))
num_obitos_2021 # 375.209

num_obitos_2022 <- as.numeric(count(df_obitos_2022))
num_obitos_2022 # 63.919

# 2023
num_obitos_2023 <- as.numeric(count(df_obitos_2023))
num_obitos_2023 # 5823


# Media Idade Obitos
media_idade_obitos <- round(mean(df_obitos$NU_IDADE_N),1)
media_idade_obitos_2021 <- round(mean(df_obitos_2021$NU_IDADE_N),1)
media_idade_obitos_2022 <- round(mean(df_obitos_2022$NU_IDADE_N),1)
media_idade_obitos_2023 <- round(mean(df_obitos_2023$NU_IDADE_N),1)

# Sexo Obitos
df_obitos_sexo <- as.data.frame(table(df_obitos$ANO_EVOLUCA,df_obitos$CS_SEXO)) %>% arrange(desc(Var1), desc(Freq))
df_obitos_sexo <- setNames(df_obitos_sexo, c("Ano", "Sexo", "Qtde"))
df_obitos_sexo['Label'] <- ifelse(df_obitos_sexo$Sexo == "M", "Masculino",ifelse(df_obitos_sexo$Sexo == "F", "Feminino", "Indefinido" ))

# Sexo Obitos Geral
df_obitos_sexo_geral <- df_obitos_sexo %>% group_by(Label) %>% 
                        summarise(Qtde=sum(Qtde)) %>%
                        arrange(desc(Qtde))
df_obitos_sexo_geral['Percentual'] = round((df_obitos_sexo_geral$Qtde/sum(df_obitos_sexo_geral$Qtde))* 100,1)
df_obitos_sexo_geral

# Sexo Obitos 2021
df_obitos_sexo_2021 <- df_obitos_sexo %>% 
  filter(Ano == '2021') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_sexo_2021['Percentual'] = round((df_obitos_sexo_2021$Qtde/sum(df_obitos_sexo_2021$Qtde))* 100,1)
df_obitos_sexo_2021

# Sexo Obitos 2022
df_obitos_sexo_2022 <- df_obitos_sexo %>% 
  filter(Ano == '2022') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_sexo_2022['Percentual'] = round((df_obitos_sexo_2022$Qtde/sum(df_obitos_sexo_2022$Qtde))* 100,1)
df_obitos_sexo_2022

# Sexo Obitos 2023
df_obitos_sexo_2023 <- df_obitos_sexo %>% 
  filter(Ano == '2023') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_sexo_2023['Percentual'] = round((df_obitos_sexo_2023$Qtde/sum(df_obitos_sexo_2023$Qtde))* 100,1)
df_obitos_sexo_2023

# Escolaridade
df_obitos_escol <- as.data.frame(table(df_obitos$ANO_EVOLUCA, df_obitos$CS_ESCOL_N)) %>% arrange(desc(Var1),desc(Freq))
df_obitos_escol <- setNames(df_obitos_escol, c("Ano","Escolaridade", "Qtde"))
df_obitos_escol['Label'] <- ifelse(df_obitos_escol$Escolaridade == 0, "Sem Escolaridade",
                                  ifelse(df_obitos_escol$Escolaridade == 1, "Ensino Fundamental (1o Ciclo)", 
                                  ifelse(df_obitos_escol$Escolaridade == 2, "Ensino Fundamental (2o Ciclo)",
                                  ifelse(df_obitos_escol$Escolaridade == 3, "Ensino Médio",
                                  ifelse(df_obitos_escol$Escolaridade == 4, "Ensino Superior", "Ignorado")))))

# Escolaridade Geral
df_obitos_escol_geral <- df_obitos_escol %>% group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_escol_geral['Percentual'] <- round(df_obitos_escol_geral$Qtde / sum(df_obitos_escol_geral$Qtde) * 100,1)
df_obitos_escol_geral

# Escolaridade 2021
df_obitos_escol_2021 <- df_obitos_escol %>% 
  filter(Ano == '2021') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_escol_2021['Percentual'] = round((df_obitos_escol_2021$Qtde/sum(df_obitos_escol_2021$Qtde))* 100,1)
df_obitos_escol_2021

# Escolaridade 2022
df_obitos_escol_2022 <- df_obitos_escol %>% 
  filter(Ano == '2022') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_escol_2022['Percentual'] = round((df_obitos_escol_2022$Qtde/sum(df_obitos_escol_2022$Qtde))* 100,1)
df_obitos_escol_2022

# Escolaridade 2023
df_obitos_escol_2023 <- df_obitos_escol %>% 
  filter(Ano == '2023') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_escol_2023['Percentual'] = round((df_obitos_escol_2023$Qtde/sum(df_obitos_escol_2023$Qtde))* 100,1)
df_obitos_escol_2023

# Etnia (Raca/cor)
df_obitos_etnia <- as.data.frame(table(df_obitos$ANO_EVOLUCA,df_obitos$CS_RACA)) %>% arrange(desc(Var1),desc(Freq))
df_obitos_etnia <- setNames(df_obitos_etnia, c("Ano","Raca", "Qtde"))
df_obitos_etnia['Label'] <- ifelse(df_obitos_etnia$Raca == 1, "Branca",
                           ifelse(df_obitos_etnia$Raca == 2, "Preta", 
                           ifelse(df_obitos_etnia$Raca == 3, "Amarela",
                           ifelse(df_obitos_etnia$Raca == 4, "Parda",
                           ifelse(df_obitos_etnia$Raca == 5, "Indígena",
                           ifelse(df_obitos_etnia$Raca == 9, "Ignorado", "Ignorado"))))))



# Etnia Geral
df_obitos_etnia_geral <- df_obitos_etnia %>% group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_etnia_geral['Percentual'] <- round(df_obitos_etnia_geral$Qtde / sum(df_obitos_etnia_geral$Qtde) * 100,1)
df_obitos_etnia_geral

# Etnia 2021
df_obitos_etnia_2021 <- df_obitos_etnia %>% 
  filter(Ano == '2021') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_etnia_2021['Percentual'] = round((df_obitos_etnia_2021$Qtde/sum(df_obitos_etnia_2021$Qtde))* 100,1)
df_obitos_etnia_2021

# Etnia 2022
df_obitos_etnia_2022 <- df_obitos_etnia %>% 
  filter(Ano == '2022') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_etnia_2022['Percentual'] = round((df_obitos_etnia_2022$Qtde/sum(df_obitos_etnia_2022$Qtde))* 100,1)
df_obitos_etnia_2022

# Etnia 2023
df_obitos_etnia_2023 <- df_obitos_etnia %>% 
  filter(Ano == '2023') %>%
  group_by(Label) %>% 
  summarise(Qtde=sum(Qtde)) %>%
  arrange(desc(Qtde))
df_obitos_etnia_2023['Percentual'] = round((df_obitos_etnia_2023$Qtde/sum(df_obitos_etnia_2023$Qtde))* 100,1)
df_obitos_etnia_2023

#########################

# Grafico 01_mapa_Obitos_BR
estado <- read_state(year=2020)

num_obitos_estado <- aggregate(df_obitos$SG_UF_INTE,by=list(df_obitos$SG_UF_INTE), FUN=length)
num_obitos_estado <- setNames(num_obitos_estado, c("abbrev_state", "Qtde"))
num_obitos_estado

dados_mapa <- estado %>% inner_join(num_obitos_estado)
dados_mapa

ggplot()+
  geom_sf(data=dados_mapa, aes(fill=Qtde), color = 'gray')+
  scale_fill_distiller(palette="Reds", direction = 1, name= "Num Óbitos", labels = label_number(drop0trailing = T, big.mark = ""))+
  labs(x=NULL, y=NULL)+
  labs(title = 'Mapa de Óbitos por Estado', size=10)+
  geom_sf_text(data=estado, aes(label = abbrev_state), size = 3.5, color = "black")+
  scale_size_continuous(labels= comma)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
  annotation_scale()+
  annotation_north_arrow(location='tl')

# Grafico 02_idades_obitos
summary(df_obitos$NU_IDADE_N)

df_obitos %>% 
  ggplot(aes(x = NU_IDADE_N)) +
  geom_histogram()

# Grafico Evolucao Obitos
obitos_2022 <- df_covid_obitos %>%
        filter(DT_EVOLUCA >= '01-01-2022' & DT_EVOLUCA < '01-03-2022')

num_obitos <- aggregate(obitos_2021$DT_EVOLUCA,by=list(obitos_2021$DT_EVOLUCA), FUN=length)
num_obitos <- setNames(num_obitos, c("Data", "Qtde"))

obitos <- num_obitos %>%
          select(Data, Qtde) %>%
          mutate(media_movel_15=rollmean(Qtde, k = 15, fill = NA),
                media_movel_30=rollmean(Qtde, k = 30, fill = NA),
                media_movel_45=rollmean(Qtde, k = 30, fill = NA))

ggplot(obitos, aes(x=Data, Qtde))+
  geom_col()+ geom_line(aes)
  

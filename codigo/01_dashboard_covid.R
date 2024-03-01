# Universidade Estadual Paulista - UNESP
# PPGMiT FAAC-UNESP
# Projeto de Mestrado Profissional
# Author: Marcelo Santos
# Fonte : https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023
# Advisor: Joao Pedro Albino
# Alterações feitas em: 25/06/2023

# Diretorio da Trabalho
setwd('D:/GoogleDrive/Mestrado/PPGMIT-openDataSUS')

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
#library(lubridate) # Datas
#library(geobr)   # mapa BR
#library(scales)  # mapa BR
#library(ggspatial) # mapa BR
#library(zoo) # calcular rollmean

# Dataframe : df_covid
# Importacao dos arquivos anuais : INFLUD* 2021, 2022 e 2023 (1498040)
# Filtro : 78-Classificacao Final do caso (5-SRAG por COVID-19)
# Tratamento : variaveis data e criacao variaveis Ano
df_covid <-list.files(path="./dados",pattern="INFLUD*", full.names = TRUE) %>%
  lapply(read.csv,sep=';', 
         stringsAsFactors=F, 
         colClasses = 'character') %>% 
  bind_rows %>% 
  select(   # DataS 
            DT_EVOLUCA,
            DT_NOTIFIC,
            # Classificacao e Evolucao
            CLASSI_FIN, 
            EVOLUCAO,
            # Dados Gerais
            NU_IDADE_N,
            CS_SEXO,
            CS_ESCOL_N,
            CS_RACA,
            # Local Notificacao 
            SG_UF_NOT,
            ID_MUNICIP,
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
            MORB_DESC) %>%
  filter(CLASSI_FIN == "5") %>%
  mutate(DT_NOTIFIC = format(as.Date(DT_NOTIFIC, format='%d/%m/%Y')),
         DT_EVOLUCA = format(as.Date(DT_EVOLUCA, format='%d/%m/%Y')),
         ANO_NOTIFIC = as.numeric(format(as.Date(DT_NOTIFIC), "%Y")),
         ANO_EVOLUCA = as.numeric(format(as.Date(DT_EVOLUCA), "%Y"))
         )

# Filtros : Ano Evolucao : 2021, 2022, 2023
df_covid <- df_covid  %>% 
  filter(ANO_EVOLUCA == 2021 | ANO_EVOLUCA == 2022 | ANO_EVOLUCA == 2023) %>% 
  arrange(ANO_EVOLUCA,SG_UF_NOT, ID_MUNICIP)  

# Criando arquivo do Dataframe df_covid (Classificacao Final : SRAG por COVID-19)
write_csv2(df_covid, file='./dados/COVID.csv')

## Criando arquivo do dataframe df_municipios (derivado do df_covid_obitos)
df_municipios <-   distinct(df_covid, SG_UF_NOT, ID_MUNICIP) %>% 
                   arrange(SG_UF_NOT, ID_MUNICIP)
write_csv2(df_municipios, file='./dados/MUNICIPIOS.csv')
rm(df_municipios)

# Dataframe : df_covid_obitos (449.248 - 43 variaveis)
# Filtros : 80- Evolucaçao do caso    (2-Óbito) no dataframe df_covid
df_obitos <- df_covid %>% 
  filter(EVOLUCAO == "2") %>%
  mutate( NU_IDADE_N = as.numeric(NU_IDADE_N)) %>%
  arrange(ANO_EVOLUCA,SG_UF_NOT, ID_MUNICIP)

# # Criando arquivo do dataframe df_covid_obitos (Classificacao Final : SRAG por COVID-19 e Evolucao do Caso : Obito)
write_csv2(df_obitos, file='./dados/COVID_OBITOS.csv')

#
df_dashboard <- df_obitos %>% 
  select(ANO_EVOLUCA, 
         SG_UF_NOT, 
         ID_MUNICIP, 
         CS_SEXO,
         CS_ESCOL_N,
         CS_RACA)

# # Criando arquivo do dataframe df_covid_obitos (Classificacao Final : SRAG por COVID-19 e Evolucao do Caso : Obito)
write_csv2(df_dashboard, file='./dados/DASHBOARD.csv')

# Graficos
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
df_sexo$fraction = df_sexo$count / sum(df_sexo$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=CS_SEXo)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart


df_sexo <- aggregate(df_obitos$CS_SEXO,
                            by=list(CS_SEXO=df_obitos$CS_SEXO), FUN=length) 


## Totais

## Totais 2021
# Totais Notificacoes
df_totais_not_2021 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2021) %>%
  group_by (SG_UF_NOT, ID_MUNICIP ) %>%
  summarise(tot_notifica_2021 = n(), .groups ='drop') 

# Totais 1-Cura
df_totais_cura_2021 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2021 & EVOLUCAO=="1") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_cura_2021 = n(), .groups ='drop')  

# Totais 2-obito
df_totais_obito_2021 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2021 & EVOLUCAO=="2") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_obito_2021 = n(), .groups ='drop')  

# Totais 3-obito outras causas
df_totais_outra_2021 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2021 & EVOLUCAO=="3") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
    summarise(tot_outra_2021 = n(), .groups ='drop')  

# Totais 9-ignorado
df_totais_ignorado_2021 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2021 & EVOLUCAO=="9") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_ignorado_2021 = n(), .groups ='drop')  

## Totais 2022
# Totais Notificacoes
df_totais_not_2022 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2022) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_notifica_2022 = n(), .groups ='drop') 

# Totais 1-Cura
df_totais_cura_2022 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2022 & EVOLUCAO=="1") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_cura_2022 = n(), .groups ='drop')  

# Totais 2-obito
df_totais_obito_2022 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2022 & EVOLUCAO=="2") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_obito_2022 = n(), .groups ='drop') 

# Totais 3-obito outras causas
df_totais_outra_2022 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2022 & EVOLUCAO=="3") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_outra_2022 = n(), .groups ='drop') 

# Totais 9-ignorado
df_totais_ignorado_2022 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2022 & EVOLUCAO=="9") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_ignorado_2022 = n(), .groups ='drop') 

## Totais 2023
# Totais Notificacoes
df_totais_not_2023 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2023) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_notifica_2023 = n(), .groups ='drop') 

# Totais 1-Cura
df_totais_cura_2023 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2023 & EVOLUCAO=="1") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_cura_2023 = n(), .groups ='drop')  

# Totais 2-obito
df_totais_obito_2023 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2023 & EVOLUCAO=="2") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_obito_2023 = n(), .groups ='drop') 

# Totais 3-obito outras causas
df_totais_outra_2023 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2023 & EVOLUCAO=="3") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_outra_2023 = n(), .groups ='drop') 

# Totais 9-ignorado
df_totais_ignorado_2023 <- df_covid %>% 
  filter(ANO_EVOLUCA == 2023 & EVOLUCAO=="9") %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(tot_ignorado_2023 = n(), .groups ='drop') 

df_totais <- distinct(df_covid, SG_UF_NOT, ID_MUNICIP)

df_totais <- df_totais %>% 
  left_join(df_totais_not_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                              'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_totais_cura_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                              'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_totais_obito_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                                'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_totais_outra_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                               'ID_MUNICIP' = 'ID_MUNICIP'))  %>%
  left_join(df_totais_ignorado_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                                  'ID_MUNICIP' = 'ID_MUNICIP')) %>%  
  left_join(df_totais_not_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_totais_cura_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                              'ID_MUNICIP' = 'ID_MUNICIP'))  %>%
  left_join(df_totais_obito_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                               'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_totais_outra_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                               'ID_MUNICIP' = 'ID_MUNICIP'))  %>%
  left_join(df_totais_ignorado_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                                  'ID_MUNICIP' = 'ID_MUNICIP')) %>%  
  left_join(df_totais_not_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_totais_cura_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                              'ID_MUNICIP' = 'ID_MUNICIP'))  %>%
  left_join(df_totais_obito_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                               'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_totais_outra_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                               'ID_MUNICIP' = 'ID_MUNICIP'))  %>%
  left_join(df_totais_ignorado_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                                  'ID_MUNICIP' = 'ID_MUNICIP')) %>%  
  arrange(SG_UF_NOT, ID_MUNICIP)
  
# Media de idade Obitos
df_obitos_md_idade_2021 <- df_obitos %>% 
  filter(ANO_EVOLUCA == 2021 & NU_IDADE_N > 0) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(obitos_md_idade_2021 = round(mean(NU_IDADE_N),1), .groups ='drop')

df_obitos_md_idade_2022 <- df_obitos %>% 
  filter(ANO_EVOLUCA == 2022 & NU_IDADE_N > 0) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(obitos_md_idade_2022 = round(mean(NU_IDADE_N),1), .groups ='drop')

df_obitos_md_idade_2023 <- df_obitos %>% 
  filter(ANO_EVOLUCA == 2023 & NU_IDADE_N > 0) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(obitos_md_idade_2023 = round(mean(NU_IDADE_N),1), .groups ='drop') %>%
  arrange(SG_UF_NOT, ID_MUNICIP)

df_totais <- df_totais %>% 
  left_join(df_obitos_md_idade_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_md_idade_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                              'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_md_idade_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                               'ID_MUNICIP' = 'ID_MUNICIP'))
  
# Quantidade Sexo
df_obitos_sexo <- aggregate(df_obitos$CS_SEXO,
                            by=list(ANO_EVOLUCA = df_obitos$ANO_EVOLUCA, SG_UF_NOT=df_obitos$SG_UF_NOT, ID_MUNICIP=df_obitos$ID_MUNICIP,CS_SEXO=df_obitos$CS_SEXO), FUN=length) %>%
  arrange(SG_UF_NOT, ID_MUNICIP)

# 1-Masculino Sexo do paciente. Campo Obrigatório CS_SEXO
# 2-Feminino
# 9-Ignorado

# 2021
df_obitos_qt_sexo_M_2021 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2021 & CS_SEXO == "M") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_M_2021=x) 

df_obitos_qt_sexo_F_2021 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2021 & CS_SEXO == "F") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_F_2021=x) 

df_obitos_qt_sexo_I_2021 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2021 & CS_SEXO == "I") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_I_2021=x) 

# 2022
df_obitos_qt_sexo_M_2022 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2022 & CS_SEXO == "M") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_M_2022=x) 

df_obitos_qt_sexo_F_2022 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2022 & CS_SEXO == "F") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_F_2022=x) 

df_obitos_qt_sexo_I_2022 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2022 & CS_SEXO == "I") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_I_2022=x) 

# 2023
df_obitos_qt_sexo_M_2023 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2023 & CS_SEXO == "M") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_M_2023=x) 

df_obitos_qt_sexo_F_2023 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2023 & CS_SEXO == "F") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_F_2023=x) 

df_obitos_qt_sexo_I_2023 <- df_obitos_sexo %>% 
  filter( ANO_EVOLUCA == 2023 & CS_SEXO == "I") %>% 
  select(SG_UF_NOT, ID_MUNICIP, qt_sexo_I_2023=x) 

df_obitos_sexo_2023 <- df_obitos_sexo_2023 %>%
  select(SG_UF_NOT, ID_MUNICIP, sexo_2023=CS_SEXO)  

df_totais <- df_totais %>% 
  left_join(df_obitos_qt_sexo_M_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_F_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_I_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_M_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_F_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_I_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_M_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_F_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_qt_sexo_I_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) 

# Escolaridade
# 0-Sem escolaridade/Analfabeto
# 1-Fundamental 1º ciclo (1ª a 5ª série)
# 2-Fundamental 2º ciclo (6ª a 9ª série)
# 3- Médio (1º ao 3º ano)
# 4-Superior
# 5-Não se aplica
# 9-Ignorado
df_obitos_escolaridade <- aggregate(df_obitos$CS_ESCOL_N,
                            by=list(ANO_EVOLUCA = df_obitos$ANO_EVOLUCA, SG_UF_NOT=df_obitos$SG_UF_NOT, ID_MUNICIP=df_obitos$ID_MUNICIP,CS_ESCOL_N=df_obitos$CS_ESCOL_N), FUN=length) %>%
  arrange(SG_UF_NOT, ID_MUNICIP)

# Ignorado
df_obitos_escolaridade$CS_ESCOL_N[df_obitos_escolaridade$CS_ESCOL_N == ""] <- 9

# 2021
# 0-Sem escolaridade/Analfabeto
df_obitos_escola_0_2021 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_ESCOL_N == "0")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_0_2021 = sum(x), .groups ='drop')

# 1-Fundamental 1º ciclo (1ª a 5ª série)
df_obitos_escola_1_2021 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_ESCOL_N == "1")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_1_2021 = sum(x), .groups ='drop')

# 2-Fundamental 2º ciclo (6ª a 9ª série)
df_obitos_escola_2_2021 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_ESCOL_N == "2")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_2_2021 = sum(x), .groups ='drop')

# 3- Médio (1º ao 3º ano)
df_obitos_escola_3_2021 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_ESCOL_N == "3")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_3_2021 = sum(x), .groups ='drop')

# 4-Superior
df_obitos_escola_4_2021 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_ESCOL_N == "4")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_4_2021 = sum(x), .groups ='drop')

# 5-Não se aplica
df_obitos_escola_5_2021 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_ESCOL_N == "5")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_5_2021 = sum(x), .groups ='drop')

# 9 ou "" - Ignorado
df_obitos_escola_9_2021 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_ESCOL_N == "9" )) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_9_2021 = sum(x), .groups ='drop')

# 2022
# 0-Sem escolaridade/Analfabeto
df_obitos_escola_0_2022 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_ESCOL_N == "0")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_0_2022 = sum(x), .groups ='drop')

# 1-Fundamental 1º ciclo (1ª a 5ª série)
df_obitos_escola_1_2022 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_ESCOL_N == "1")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_1_2022 = sum(x), .groups ='drop')

# 2-Fundamental 2º ciclo (6ª a 9ª série)
df_obitos_escola_2_2022 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_ESCOL_N == "2")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_2_2022 = sum(x), .groups ='drop')

# 3- Médio (1º ao 3º ano)
df_obitos_escola_3_2022 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_ESCOL_N == "3")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_3_2022 = sum(x), .groups ='drop')

# 4-Superior
df_obitos_escola_4_2022 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_ESCOL_N == "4")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_4_2022 = sum(x), .groups ='drop')

# 5-Não se aplica
df_obitos_escola_5_2022 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_ESCOL_N == "5")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_5_2022 = sum(x), .groups ='drop')

# 9 ou "" - Ignorado
df_obitos_escola_9_2022 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_ESCOL_N == "9" )) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_9_2022 = sum(x), .groups ='drop')

# 2023
# 0-Sem escolaridade/Analfabeto
df_obitos_escola_0_2023 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_ESCOL_N == "0")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_0_2023 = sum(x), .groups ='drop')

# 1-Fundamental 1º ciclo (1ª a 5ª série)
df_obitos_escola_1_2023 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_ESCOL_N == "1")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_1_2023 = sum(x), .groups ='drop')

# 2-Fundamental 2º ciclo (6ª a 9ª série)
df_obitos_escola_2_2023 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_ESCOL_N == "2")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_2_2023 = sum(x), .groups ='drop')

# 3- Médio (1º ao 3º ano)
df_obitos_escola_3_2023 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_ESCOL_N == "3")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_3_2023 = sum(x), .groups ='drop')

# 4-Superior
df_obitos_escola_4_2023 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_ESCOL_N == "4")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_4_2023 = sum(x), .groups ='drop')

# 5-Não se aplica
df_obitos_escola_5_2023 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_ESCOL_N == "5")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_5_2023 = sum(x), .groups ='drop')

# 9 ou "" - Ignorado
df_obitos_escola_9_2023 <-   
  df_obitos_escolaridade %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_ESCOL_N == "9" )) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_escola_9_2023 = sum(x), .groups ='drop')


df_totais <- df_totais %>% 
  left_join(df_obitos_escola_0_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_1_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_2_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_3_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_4_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_5_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_9_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                             'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_0_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_1_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_2_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_3_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_4_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_5_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_9_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_0_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_1_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_2_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_3_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_4_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_5_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_escola_9_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) 
  
# Raça
# 1-Branca 
# 2-Preta
# 3-Amarela
# 4-Parda
# 5-Indígena
# 9-Ignorada

df_obitos_raca <- aggregate(df_obitos$CS_RACA,
                                    by=list(ANO_EVOLUCA = df_obitos$ANO_EVOLUCA, SG_UF_NOT=df_obitos$SG_UF_NOT, ID_MUNICIP=df_obitos$ID_MUNICIP,CS_RACA=df_obitos$CS_RACA), FUN=length) %>%
  arrange(SG_UF_NOT, ID_MUNICIP)  

# 2021  

# 1-Branca 
df_obitos_raca_1_2021 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_RACA == "1")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_1_2021 = sum(x), .groups ='drop')

# 2-Preta
df_obitos_raca_2_2021 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_RACA == "2")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_2_2021 = sum(x), .groups ='drop')

# 3-Amarela
df_obitos_raca_3_2021 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_RACA == "3")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_3_2021 = sum(x), .groups ='drop')

# 4-Parda
df_obitos_raca_4_2021 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_RACA == "4")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_4_2021 = sum(x), .groups ='drop')

# 5-Indígena
df_obitos_raca_5_2021 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_RACA == "5")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_5_2021 = sum(x), .groups ='drop')

# 9-Ignorada
df_obitos_raca_9_2021 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2021 & (CS_RACA == "9")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_9_2021 = sum(x), .groups ='drop')

# 2022  

# 1-Branca 
df_obitos_raca_1_2022 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_RACA == "1")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_1_2022 = sum(x), .groups ='drop')

# 2-Preta
df_obitos_raca_2_2022 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_RACA == "2")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_2_2022 = sum(x), .groups ='drop')

# 3-Amarela
df_obitos_raca_3_2022 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_RACA == "3")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_3_2022 = sum(x), .groups ='drop')

# 4-Parda
df_obitos_raca_4_2022 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_RACA == "4")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_4_2022 = sum(x), .groups ='drop')

# 5-Indígena
df_obitos_raca_5_2022 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_RACA == "5")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_5_2022 = sum(x), .groups ='drop')

# 9-Ignorada
df_obitos_raca_9_2022 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2022 & (CS_RACA == "9")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_9_2022 = sum(x), .groups ='drop')

# 2023  
# 1-Branca 
df_obitos_raca_1_2023 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_RACA == "1")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_1_2023 = sum(x), .groups ='drop')

# 2-Preta
df_obitos_raca_2_2023 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_RACA == "2")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_2_2023 = sum(x), .groups ='drop')

# 3-Amarela
df_obitos_raca_3_2023 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_RACA == "3")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_3_2023 = sum(x), .groups ='drop')

# 4-Parda
df_obitos_raca_4_2023 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_RACA == "4")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_4_2023 = sum(x), .groups ='drop')

# 5-Indígena
df_obitos_raca_5_2023 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_RACA == "5")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_5_2023 = sum(x), .groups ='drop')

# 9-Ignorada
df_obitos_raca_9_2023 <-   
  df_obitos_raca %>% 
  filter(ANO_EVOLUCA == 2023 & (CS_RACA == "9")) %>%
  group_by (SG_UF_NOT, ID_MUNICIP) %>%
  summarise(qt_raca_9_2023 = sum(x), .groups ='drop')

df_totais <- df_totais %>% 
  left_join(df_obitos_raca_1_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_1_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_2_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_3_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_4_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_5_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_9_2021, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                            'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_1_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_2_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_3_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_4_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_5_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_9_2022, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_1_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_2_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_3_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_4_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_5_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP')) %>%
  left_join(df_obitos_raca_9_2023, by = c('SG_UF_NOT' = 'SG_UF_NOT'  , 
                                          'ID_MUNICIP' = 'ID_MUNICIP'))
                                            

df_totais[is.na(df_totais)] <- 0


write_csv2(df_totais, file='./dados/COVID_TOTAIS.csv')

####


Municipios <- distinct(df_totais, SG_UF_NOT, ID_MUNICIP)

Municipios

######


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
  

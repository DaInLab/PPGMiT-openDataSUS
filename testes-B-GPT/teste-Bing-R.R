# Carregar pacotes necessários
if (!("tidyverse") %in% installed.packages()) install.packages("tidyverse")
if (!("rvest") %in% installed.packages()) install.packages("rvest")
library(tidyverse)
library(rvest)

# Definir URL do site do Ministério da Saúde
url <- "https://covid.saude.gov.br/"

# Extrair tabela de casos e óbitos por faixa etária
tabela <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="wrapper"]/div/div[3]/div[1]/div[2]/div[2]/div/div/div/table') %>%
  html_table() # %>%
#  .[[1]]

# Converter colunas numéricas para formato correto
tabela <- tabela %>%
  mutate(across(c(2:5), ~as.numeric(gsub("\\.", "", .))))

# Calcular taxa de mortalidade por faixa etária
tabela <- tabela %>%
  mutate(taxa_mortalidade = (Obitos / Casos) * 100)

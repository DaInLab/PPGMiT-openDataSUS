## UM DASHBOARD DE DADOS ABERTOS DE ÓBITOS POR COVID-19 NO BRASIL
Relatório Técnico apresentado ao Programa de Pós-graduação em Mídia e Tecnologia (PPGMiT) – FAAC – UNESP – Bauru para obtenção do título de Mestre em Mídia e Tecnologia, sob a orientação do Prof. Associado João Pedro Albino. 
Aluno Pos Graduação: Marcelo José dos Santos 
https://www.linkedin.com/in/marcelo-j-santos/

Este trabalho propõe o desenvolvimento de um painel de informações de acesso público, chamado dashboard, que apresenta dados abertos sobre os óbitos ocorridos durante a pandemia de COVID-19 no Brasil. 
Utilizou-se uma metodologia de Ciência de Dados e uma análise exploratória de um conjunto de dados abertos existente no repositório do Ministério da Saúde do Brasil (OpenDataSUS). 
Este dashboard oferece uma exibição de dados abertos consolidados por meio de artefatos estatísticos e gráficos criados utilizando a linguagem de programação R.

# Fonte : https://opendatasus.saude.gov.br/dataset/srag-2021-a-2023
# Projeto Linguagem R : PPGMiT-openDataSUS.Rproj
# Github : https://github.com/DaInLab/PPGMiT-openDataSUS.git
# Internet : https://ppgmitdashboardcovid.shinyapps.io/dashboard_covid_novo/

# Pastas do projeto : 

\documentos : Contem todos os documentos do projeto : openDataSUS, apresentacao da Defesa, documento da Defesa, Dicionario de dados
\dados : Contem os arquivos com os dados brutos do projeto.
         - opendataSUS : INFLUD23*, INFLUD22*, INFLUD21*
         - arquivos de gerados no processamento dos dados brutos : MUNICIPIOS.CSV, COVID.CSV, COVID_OBITOS.CSV, COVID_TOTAIS.CSV, DASHBOARD.CSV

\codigo : Contem arquivo do codigo fonte usado na importacao e processamento dos dados brutos 
          01_dashboard_covid.R 
\ : Pasta raiz onde contem o arquivo codigo fonte do dashboard 
          dashboard_covid_novo.Rmd

Bauru 25 de Março de 2024.


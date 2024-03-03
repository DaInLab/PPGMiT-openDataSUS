# load obitos
library(ggplot2) # Visualizacao dos dados
library(dplyr)   # Preparacao de dados
library(readr)   # Importacao dos dados
library(flexdashboard)
library(shiny)
library(shinyWidgets)
library(knitr)
library(kableExtra)
library(plotly)

df_dashboard <- read.csv('./dados/DASHBOARD.csv', sep=";")

df_totais <- read.csv('./dados/COVID_TOTAIS.csv', sep=";") 

dados$qt_sexo_M_2021

dados <- df_totais

# 1- Masculino 2-Feminino 3-Ignorado

switch("2021",
       "2021" = {sexo <- c(sum(as.numeric(dados$qt_sexo_M_2021)), sum(as.numeric(dados$qt_sexo_F_2021)), sum(as.numeric(dados$qt_sexo_I_2021)))},
       "2022" = {sexo <- c(sum(as.numeric(dados$qt_sexo_M_2022)), sum(as.numeric(dados$qt_sexo_F_2022)), sum(as.numeric(dados$qt_sexo_I_2022)))},
       "2023" = {sexo <- c(sum(as.numeric(dados$qt_sexo_M_2023)), sum(as.numeric(dados$qt_sexo_F_2023)), sum(as.numeric(dados$qt_sexo_I_2023)))}
)


dados <- df_dashboard

df_sexo <- aggregate(dados$CS_SEXO,
                     by=list(CS_SEXO=dados$CS_SEXO), FUN=length) 

df_sexo



#  Raça 1-Branca  2-Preta 3-Amarela 4-Parda 5-Indígena 9-Ignorada
switch("2021",
       "2021" = {raca <- c(sum(as.numeric(dados$qt_raca_1_2021)), sum(as.numeric(dados$qt_raca_2_2021)), sum(as.numeric(dados$qt_raca_3_2021)),sum(as.numeric(dados$qt_raca_4_2021)), sum(as.numeric(dados$qt_raca_5_2021)),sum(as.numeric(dados$qt_raca_9_2021)))},
       "2022" = {raca <- c(sum(as.numeric(dados$qt_raca_1_2022)), sum(as.numeric(dados$qt_raca_2_2022)), sum(as.numeric(dados$qt_raca_3_2022)),sum(as.numeric(dados$qt_raca_4_2022)), sum(as.numeric(dados$qt_raca_5_2022)),sum(as.numeric(dados$qt_raca_9_2022)))},
       "2023" = {raca <- c(sum(as.numeric(dados$qt_raca_1_2023)), sum(as.numeric(dados$qt_raca_2_2023)), sum(as.numeric(dados$qt_raca_3_2023)),sum(as.numeric(dados$qt_raca_4_2023)), sum(as.numeric(dados$qt_raca_5_2023)),sum(as.numeric(dados$qt_raca_9_2023)))}
)

maior_raca <- which(raca == max(as.numeric(raca)))




raca <- data.frame(raca=c("Branca", 
                          "Preta",
                          "Amarela",
                          "Parda",
                          "Indígena",
                          "Ignorada"),
                   qtde= c(as.numeric(sum(as.numeric(dados$qt_raca_1_2021))),
                           as.numeric(sum(as.numeric(dados$qt_raca_2_2021))),
                           as.numeric(sum(as.numeric(dados$qt_raca_3_2021))),
                           as.numeric(sum(as.numeric(dados$qt_raca_4_2021))),
                           as.numeric(sum(as.numeric(dados$qt_raca_5_2021))),
                           as.numeric(sum(as.numeric(dados$qt_raca_9_2021))))
                           
                           ,
                           sum(as.numeric(dados$qt_raca_2_2021)))   

raca 

c('Branca',as.numeric(sum(as.numeric(dados$qt_raca_1_2021))),
          'Preta',as.numeric(sum(as.numeric(dados$qt_raca_2_2021))))

raca

dados$qt_raca_1_2021

str(raca)




, sum(as.numeric(dados$qt_raca_2_2021)), sum(as.numeric('Amarela',dados$qt_raca_3_2021)),sum('Parda, as.numeric(dados$qt_raca_4_2021)), sum(as.numeric('Indígena', dados$qt_raca_5_2021)),sum(as.numeric('Ignorada',dados$qt_raca_9_2021)))


```{r}
renderGauge({ 
  dados <- df_dados_totais()
  switch(input$FiltroAno,
         "2021" = {perc_ignora <- if (sum(dados$tot_notifica_2021)==0) {0} else {(sum(dados$tot_ignora_2021)/sum(dados$tot_notifica_2021))* 100}},
         "2022" = {perc_ignora <- if (sum(dados$tot_notifica_2022)==0) {0} else {(sum(dados$tot_ignora_2022)/sum(dados$tot_notifica_2022))* 100}},
         "2023" = {perc_ignora <- if (sum(dados$tot_notifica_2023)==0) {0} else {(sum(dados$tot_ignora_2023)/sum(dados$tot_notifica_2023))* 100}}
  )
  
  gauge(perc_ignora, min = 0, max = 100, symbol = '%', label="Outra Causa%", gaugeSectors(
    success = c(0, 19), warning = c(20, 59), danger = c(60, 100),
    colors = c("blue","yellow", "red")))
})

```






dados <- df_totais

switch(2021,
       "2021" = {raca <- c(sum(as.numeric(dados$qt_raca_1_2021)), sum(as.numeric(dados$qt_raca_2_2021)), sum(as.numeric(dados$qt_raca_3_2021)),sum(as.numeric(dados$qt_raca_4_2021)), sum(as.numeric(dados$qt_raca_5_2021)),sum(as.numeric(dados$qt_raca_9_2021))},
       "2022" = {raca <- c(sum(dados$qt_raca_1_2022), sum(dados$qt_raca_2_2022), sum(dados$qt_raca_3_2022),sum(dados$qt_raca_4_2022), sum(dados$qt_raca_5_2022),sum(dados$qt_raca_9_2022))},
       "2023" = {raca <- c(sum(dados$qt_raca_1_2023), sum(dados$qt_raca_2_2023), sum(dados$qt_raca_3_2023),sum(dados$qt_raca_4_2023), sum(dados$qt_raca_5_2023),sum(dados$qt_raca_9_2023))}
)

raca <- c(sum(dados$qt_raca_1_2021), sum(dados$qt_raca_2_2021), sum(dados$qt_raca_3_2021),sum(dados$qt_raca_4_2021), sum(dados$qt_raca_5_2021),sum(dados$qt_raca_9_2021))

raca


df_sintomas <- data.frame(SINTOMAS= c("TOSSE","FEBRE","GARGANTA","DISPNEIA","DESC_RESP","SATURACAO","DIARREIA",
                                      "VOMITO","DOR_ABD","FADIGA","PERD_OLFT","PERD_PALA"), 
                          DESCRICAO= c("Tosse","Febre","Dor de Garganta","Dispnéia","Desconforto Respiratório","Saturação <95%","Diarréia",
                                       "Vômito","Dor Abdominal","Fadiga","Perda de Olfato","Perda de Paladar"),
                          QTD=c(0))
df_sintomas$QTD[df_sintomas$SINTOMAS=="TOSSE"] <- dados %>% filter(TOSSE=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="FEBRE"] <- dados %>% filter(FEBRE=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="GARGANTA"] <- dados %>% filter(GARGANTA=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="DISPNEIA"] <- dados %>% filter(DISPNEIA=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="DESC_RESP"] <- dados %>% filter(DESC_RESP=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="SATURACAO"] <- dados %>% filter(SATURACAO=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="DIARREIA"] <- dados %>% filter(DIARREIA=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="VOMITO"] <- dados %>% filter(VOMITO=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="DOR_ABD"] <- dados %>% filter(DOR_ABD=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="FADIGA"] <- dados %>% filter(FADIGA=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="PERD_OLFT"] <- dados %>% filter(PERD_OLFT=="1") %>% count()
df_sintomas$QTD[df_sintomas$SINTOMAS=="PERD_PALA"] <- dados %>% filter(PERD_PALA=="1") %>% count()


df_sintomas <- df_sintomas %>% 
  filter(as.numeric(QTD)>0) %>%
  select(QTD, DESCRICAO) %>% 
  arrange(desc(as.numeric(QTD)))



df_sintomas$rank <- row_number(df_sintomas)

df_sintomas %>%
  arrange(as.numeric(rank))



df_fatores <- data.frame(
  FATOR= c("PUERPERA", "CARDIOPATI","HEMATOLOGI","SIND_DOWN","HEPATICA","ASMA","DIABETES","NEUROLOGIC","PNEUMOPATI","IMUNODEPRE","RENAL",
           "OBESIDADE"),
  DESCRICAO= c("Puérpera", "Doença Cardiovascular","Doença Hematológica","Síndrome de Down","Doença Hepática", "Asma","Diabetes Mellitus","Doença Neurológica","Pneumopatia","Imunodeficiência","Doença Renal","Obesidade"),
  QTD=c(0))

df_fatores$QTD[df_fatores$FATOR=="PUERPERA"] <- dados %>% filter(PUERPERA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="CARDIOPATI"] <- dados %>% filter(CARDIOPATI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="HEMATOLOGI"] <- dados %>% filter(HEMATOLOGI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="SIND_DOWN"] <- dados %>% filter(SIND_DOWN=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="HEPATICA"] <- dados %>% filter(HEPATICA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="ASMA"] <- dados %>% filter(ASMA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="DIABETES"] <- dados %>% filter(DIABETES=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="NEUROLOGIC"] <- dados %>% filter(NEUROLOGIC=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="PNEUMOPATI"] <- dados %>% filter(PNEUMOPATI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="IMUNODEPRE"] <- dados %>% filter(IMUNODEPRE=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="RENAL"] <- dados %>% filter(RENAL=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="OBESIDADE"] <- dados %>% filter(OBESIDADE=="1") %>% count()

df_fatores$QTD <- as.numeric(df_fatores$QTD)

df_fatores %>%
mutate( (QTD/sum(QTD))*100)            


df_fatores <- df_fatores %>%
  summarise(as.numeric(QTD)/sum(as.numeric(QTD))

as.numeric(df_fatores$QTD)/sum(as.numeric(df_fatores$QTD))*100            
            
  df_fatores  

  mutate(round(QTD / sum(QTD) * 100))

df_fatores

df_fatores

  group_by(DESCRICAO) %>% 
  summarize(n = QTD, perc = round(unname(n) / sum(unname(n)) * 100,1))


str(df_fatores)







df_fatores




colors <- c("pink", "orange", "blue")

# set the marker properties, including the colors and line width
marker <- list(colors = colors)

# define the text and hover information to be displayed on the chart
textinfo <- "label+percent"
hoverinfo <- "text"

df_fatores <- dados %>%
  group_by(FAIXA_ETARIA) %>% summarize(n = length(NU_IDADE_N))

if (nrow(df_faixa_etaria) == 0) { chart <- plot_ly(df_faixa_etaria, labels = ~0, values = ~0, type = "pie",
                                                   hole = 0.5, marker = marker,
                                                   textinfo = textinfo, hoverinfo = hoverinfo)
ggplotly(chart)}
else {
  
  chart <- plot_ly(
    data = df_fatores,
    x = ~ DESCRICAO,
    y = ~ n,
    type = 'bar',
    colors = c('#6bbabf', '#60ab3d'),
    text = ~ paste("<br><b>Óbitos:</b>", n , "<br><b></b>", paste(round(unname(n) / sum(unname(n)) * 100,1), "%")),
    hovertemplate = paste('%{text}<extra></extra>')
  ) %>% layout(barmode = 'group', yaxis= list(showticklabels = FALSE, title = ""), xaxis = list(title="Faixa Etária"))
  
  
  ggplotly(chart)













dados <- dados %>% 
  filter(ID_MUNICIP =="AGUA BRANCA")

dados

df_fatores <- data.frame(
  FATOR= c("PUERPERA", "CARDIOPATI","HEMATOLOGI","SIND_DOWN","HEPATICA","ASMA","DIABETES","NEUROLOGIC","PNEUMOPATI","IMUNODEPRE","RENAL",
           "OBESIDADE"),
  DESCRICAO= c("Puérpera", "Doença Cardiovascular","Doença Hematológica","Síndrome de Down","Doença Hepática", "Asma","Diabetes Mellitus","Doença Neurológica","Pneumopatia","Imunodeficiência","Doença Renal","Obesidade"),
  QTD=c(0))


df_fatores$QTD[df_fatores$FATOR=="PUERPERA"] <- dados %>% filter(PUERPERA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="CARDIOPATI"] <- dados %>% filter(CARDIOPATI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="HEMATOLOGI"] <- dados %>% filter(HEMATOLOGI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="SIND_DOWN"] <- dados %>% filter(SIND_DOWN=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="HEPATICA"] <- dados %>% filter(HEPATICA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="ASMA"] <- dados %>% filter(ASMA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="DIABETES"] <- dados %>% filter(DIABETES=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="NEUROLOGIC"] <- dados %>% filter(NEUROLOGIC=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="PNEUMOPATI"] <- dados %>% filter(PNEUMOPATI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="IMUNODEPRE"] <- dados %>% filter(IMUNODEPRE=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="RENAL"] <- dados %>% filter(RENAL=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="OBESIDADE"] <- dados %>% filter(OBESIDADE=="1") %>% count()

df_fatores 

df_fatores <- df_fatores %>% 
  filter(as.numeric(QTD)>0) %>%
  select(rank, DESCRICAO) %>% 
  arrange(QTD) 

df_fatores$rank <- 1:nrow(df_fatores)

df_fatores <- select(df_fatores, rank, DESCRICAO)

colnames(df_fatores) = c("", "Principais Fatores de Risco")

if (nrow(df_fatores)> 0) {df_fatores[1:3, ]}
else {head("")}








df_fatores <- data.frame(
  FATOR= c("PUERPERA", "CARDIOPATI","HEMATOLOGI","SIND_DOWN","HEPATICA","ASMA","DIABETES","NEUROLOGIC","PNEUMOPATI","IMUNODEPRE","RENAL",
           "OBESIDADE"),
  DESCRICAO= c("Puérpera", "Doença Cardiovascular","Doença Hematológica","Síndrome de Down","Doença Hepática", "Asma","Diabetes Mellitus","Doença Neurológica","Pneumopatia","Imunodeficiência","Doença Renal","Obesidade"),
  QTD=c(0))


df_fatores$QTD[df_fatores$FATOR=="PUERPERA"] <- dados %>% filter(PUERPERA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="CARDIOPATI"] <- dados %>% filter(CARDIOPATI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="HEMATOLOGI"] <- dados %>% filter(HEMATOLOGI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="SIND_DOWN"] <- dados %>% filter(SIND_DOWN=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="HEPATICA"] <- dados %>% filter(HEPATICA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="ASMA"] <- dados %>% filter(ASMA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="DIABETES"] <- dados %>% filter(DIABETES=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="NEUROLOGIC"] <- dados %>% filter(NEUROLOGIC=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="PNEUMOPATI"] <- dados %>% filter(PNEUMOPATI=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="IMUNODEPRE"] <- dados %>% filter(IMUNODEPRE=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="RENAL"] <- dados %>% filter(RENAL=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="OBESIDADE"] <- dados %>% filter(OBESIDADE=="1") %>% count()


df_fatores <- df_fatores %>% 
  arrange(desc(as.numeric(QTD))) %>% 
  mutate(perc = as.numeric(QTD)/sum(as.numeric(QTD))*100) %>%
  filter(row_number()<=3) 



df_fatores$rank <- 1:nrow(df_fatores)

df_fatores



# Grafico Idade
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)

# Totalizar fatores
df_fatores <- data.frame(FATOR= c("TOSSE","FEBRE","GARGANTA","DISPNEIA","DESC_RESP","SATURACAO","DIARREIA",
                              "VOMITO","DOR_ABD","FADIGA","PERD_OLFT","PERD_PALA"), 
                     DESCRICAO= c("TOSSE","FEBRE","DOR DE GARGANTA","DISPNEIA","DESCONFORTO RESPIRAT.","SATURACAO <95%","DIARREIA",
                              "VOMITO","DOR ABDOMINAL","FADIGA","PERDA DE OLFATO","PERDA DE PALADAR"),
                     QTD=c(0,0))
df_fatores$QTD[df_fatores$FATOR=="TOSSE"] <- df_dashboard %>% filter(TOSSE=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="FEBRE"] <- df_dashboard %>% filter(FEBRE=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="GARGANTA"] <- df_dashboard %>% filter(GARGANTA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="DISPNEIA"] <- df_dashboard %>% filter(DISPNEIA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="DESC_RESP"] <- df_dashboard %>% filter(DESC_RESP=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="SATURACAO"] <- df_dashboard %>% filter(SATURACAO=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="DIARREIA"] <- df_dashboard %>% filter(DIARREIA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="VOMITO"] <- df_dashboard %>% filter(VOMITO=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="DOR_ABD"] <- df_dashboard %>% filter(DOR_ABD=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="FADIGA"] <- df_dashboard %>% filter(FADIGA=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="PERD_OLFT"] <- df_dashboard %>% filter(PERD_OLFT=="1") %>% count()
df_fatores$QTD[df_fatores$FATOR=="PERD_PALA"] <- df_dashboard %>% filter(PERD_PALA=="1") %>% count()


df_fatores <- df_fatores %>% 
  arrange(desc(as.numeric(QTD))) %>% 
  filter(row_number()<=3)

df_fatores

values <- list(df_fatores$DESCRICAO, df_fatores$QTD) 

p <- plot_ly(
  type = 'table',
  columnorder = c(1,2),
  columnwidth = c(80,400),
  header = list(
    values = list(list('2021'),
                  list('<b>Qtd.</b>')),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12),
    height = 140
  ),
  cells = list(
    values = values,
    line = list(color = '#506784'),
    fill = list(color = c('lightgray', 'white')),
    align = c('left', 'center'),
    font = list(color = c('white', 'gray'), size = 12),
    height = 30
  ))

p



library(knitr)
library(kableExtra)

df_fatores%>%
  slice_max(QTD, n = 10)%>%
  kable()%>%
  kable_styling(bootstrap = "bordered")



chart <- df_fatores%>%
  slice_max(QTD, n = 10)%>%
  kable()%>%
  kable_styling(bootstrap = "bordered")

head( df_fatores, n = 1, colnames("", "Qtd.", "%") )

chart

p <- plot_ly(
  type = 'table',
  header = list(
    values = list(list('2021'),
                  list('<b>Qtd.</b>')),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12)
  ),
  cells = list(
    values = values,
    line = list(color = '#506784'),
    fill = list(color = c('lightgray', 'white')),
    align = c('left', 'center'),
    font = list(color = c('white', 'gray'), size = 12)
  ))
ggplotly(p)






p
values2 <- list(df_fatores$DESCRICAO, df_fatores$QTD)
values <- list(c('Salaries', 'Office', 'Merchandise', 'Legal', '<b>TOTAL<br>EXPENSES</b>'), c("Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                              "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                              "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                              "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                              "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad"))





fig











df_faixa_etaria <- df_dashboard %>%
  group_by(FAIXA_ETARIA) %>% summarize(n = length(NU_IDADE_N))



plot_ly(
  data = df_faixa_etaria,
  x = ~ FAIXA_ETARIA,
  y = ~ n,
  type = 'bar',
  colors = c('#6bbabf', '#60ab3d'),
  text = ~ paste("<br><b>Óbitos:</b>", n ),
  hovertemplate = paste('%{text}<extra></extra>')
) %>% layout(barmode = 'group', yaxis= list(showticklabels = FALSE, title = ""), xaxix = list(title="Faixa Etária"))

colors <- c("pink", "orange", "blue")

# set the marker properties, including the colors and line width
marker <- list(colors = colors)

# define the text and hover information to be displayed on the chart
textinfo <- "label+percent"
hoverinfo <- "text"



plot_ly(df_faixa_etaria, labels = ~FAIXA_ETARIA, values = ~n, type = "pie",
        hole = 0.5, marker = marker,
        text = "",
        textinfo = textinfo, hoverinfo = hoverinfo)




barplot(table(df_dashboard$FAIXA_ETARIA))



plot_ly(
  data = df_dashboard,
  x = ~NU_IDADE_N,
  y = ~FAIXA_ETARIA,
  type = "bar"
)


plot_ly(df_sexo, labels = ~label, values = ~x, type = "pie",
        hole = 0.5, marker = marker,
        text = ~paste(label, " : ", x, " Óbitos"),
        textinfo = textinfo, hoverinfo = hoverinfo)


chart2 <- plot_ly(df_faixa_etaria, labels = ~FAIXA_ETARIA, values = ~n, type = "pie",
        hole = 0.5, marker = marker,
        text = "",
        textinfo = textinfo, hoverinfo = hoverinfo)


subplot(plot_ly(df_dashboard, 
        x = ~NU_IDADE_N, 
        type = 'scatter', 
        mode = 'lines') %>% 
  layout(title = "Line chart",
         xaxis = list(title = "Age")),
  chart2)


df_idade <- df_dashboard %>%
    select(DT_EVOLUCA, CS_SEXO, NU_IDADE_N)  

df_idade
#Code 1
ggplotly(df_idade %>% pivot_longer(-DT_EVOLUCA) %>%
           mutate(Date=as.Date(DT_EVOLUCA)) %>%
           ggplot(aes(x=DT_EVOLUCA,y=value,fill=CS_SEXO))+
           geom_bar(stat = 'identity')+
           labs(fill='Var'))
#Code 2
ggplotly(df_idade %>% pivot_longer(-DT_EVOLUCA) %>%
           mutate(Date=as.Date(DT_EVOLUCA)) %>%
           ggplot(aes(x=DT_EVOLUCA,y=value,fill=name))+
           geom_bar(stat = 'identity',position = 'fill')+
           labs(fill='Var')+
           scale_y_continuous(labels = scales::percent))


















df_sexo <- aggregate(df_dashboard$CS_SEXO,
                     by=list(CS_SEXO=df_obitos$CS_SEXO), FUN=length) 

for (k in 1:nrow(df_sexo)) {
  if(df_sexo$CS_SEXO[k] == "M") df_sexo$label[k] <- "Masculino"
  if(df_sexo$CS_SEXO[k] == "F") df_sexo$label[k] <- "Feminino"
  if(df_sexo$CS_SEXO[k] == "I") df_sexo$label[k] <- "Ignorado"
}


df_sexo

# Grafico Sexo
install.packages("plotly")
library(plotly)

colors <- c("pink", "orange", "blue")

# set the marker properties, including the colors and line width
marker <- list(colors = colors)

# define the text and hover information to be displayed on the chart
textinfo <- "label+percent"
hoverinfo <- "text"


chart <- plot_ly(df_sexo, labels = ~label, values = ~x, type = "pie",
                 hole = 0.5, marker = marker,
                 text = ~paste(CS_SEXO, x, sep = ": "),
                 textinfo = textinfo, hoverinfo = hoverinfo)

# customize the layout of the chart, including the title and legend
layout(chart, 
       legend = list(x = 0, y = 1, font = list(width = 6, color = "black")))


# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=5) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

library(ggplot2)
library(gridExtra)
data <- data.frame(
  Category = c("A", "B", "C", "D"),
  Value = c(20, 30, 40, 10)
)

pie_chart <- ggplot(data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(Category, round(Value/sum(Value) * 100), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Dark2") +
  theme_void()

donut_chart <- pie_chart + 
  geom_bar(data = data, aes(x = "", y = Value), stat = "identity", 
           width = 0.5, fill = "white") +
  geom_text(data = data, aes(label = paste(Category, round(Value/sum(Value) * 100), "%")), 
            position = position_stack(vjust = 0.5), color = "black") +
  theme_void()

donut_chart

donut_chart <- pie_chart + 
  geom_bar(data = data, aes(x = "", y = Value), stat = "identity", 
           width = 0.3, fill = "white") +
  geom_text(data = data, aes(label = paste(Category, round(Value/sum(Value) * 100), "%")), 
            position = position_stack(vjust = 0.5), color = "white") + # changed color to white
  theme_void()

grid.arrange(pie_chart, donut_chart, ncol = 2, widths = c(3, 3))


install.packages("plotly")
library(plotly)

# create a data frame with category labels and corresponding values
data <- data.frame(
  category = c("A", "B", "C", "D", "E"),
  value = c(20, 25, 15, 10, 30)
)

# define the colors for each category
colors <- c("red", "orange", "yellow", "green", "blue")

# set the marker properties, including the colors and line width
marker <- list(colors = colors)

# create the pie chart with a hole in the center
plot_ly(data, labels = ~category, values = ~value, type = "pie",
        hole = 0.5, marker = marker)



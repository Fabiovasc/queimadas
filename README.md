# queimadas

require(data.table)
require(dplyr)
require(tidyr)
queimadas1 <- fread('dados rstudio/Dataset_FireWatch_Brazil_Q1_2024.csv')
queimadas2 <- fread('dados rstudio/Dataset_FireWatch_Brazil_Q2_2024.csv')
queimadas3 <- fread('dados rstudio/Dataset_FireWatch_Brazil_Q3_2024.csv')

queimadas <- rbind(queimadas1,queimadas2,queimadas3)

#Utilizando o banco de dados Queimadas, crie uma tabela com a contagem de incêndios por estado.

incendios <- queimadas %>% 
  select(estado) %>% 
  group_by(estado) %>% 
  summarise(
    incendios_por_Estado = n()
  )

#Para a região com maior número de queimadas, identifique: a. A cidade com maior número de queimadas. b.
#A data com maior número de queimadas. c. O mês com maior número de queimadas.

estados_sudeste <- c("SÃO PAULO", "RIO DE JANEIRO", "MINAS GERAIS", "ESPÍRITO SANTO")

mais_incendio <- queimadas %>% 
  filter(estado %in% estados_sudeste) %>% 
  select(estado, municipio) %>% 
  group_by(estado) %>% 
  summarise(
    incendios_por_Estado = n()
  ) %>% 
  arrange(desc(incendios_por_Estado))

#Para a região com maior número de queimadas, identifique: a. A cidade com maior número de queimadas. b.
#A data com maior número de queimadas. c. O mês com maior número de queimadas.
cidade_mais_incendios <- queimadas %>% 
  filter(estado %in% estados_sudeste) %>% 
  select(estado, municipio) %>% 
  group_by(estado, municipio) %>% 
  summarise(
    incendios_por_cidade = n()
  ) %>% 
  arrange(desc(incendios_por_cidade))

data_mais_incendios <- queimadas %>%
  filter(estado %in% estados_sudeste) %>% 
  select(estado, mes) %>% 
  group_by(estado, mes) %>% 
  summarise(
    incendios_por_mes = n()
  ) %>% 
  arrange(desc(incendios_por_mes))
library(lubridate)

queimadas$data <- ymd(queimadas$data)

mes <- month((queimadas$data))

mes_mais_incendios <- queimadas %>% 
  filter(estado %in% estados_sudeste) %>% 
  select(estado, mes) %>% 
  group_by(estado, mes) %>% 
  summarise(
    incendios_por_mes = n()
  ) %>% 
  arrange(desc(incendios_por_mes))

#mes mais ocorreu
mes_mais_incendios <- mes_mais_incendios %>% 
  filter(incendios_por_mes == max(incendios_por_mes))

#Quantas cidades tiveram risco de fogo acima de 90% no mês de julho?
risco_de_fogo <- queimadas$avg_risco_fogo

acima_90 <- queimadas %>%
  select(
    municipio,
    avg_risco_fogo
  ) %>% 







Install.packages("tidyverse") #Instalar as bibliotecas
install.packages("dplyr")

library(tidyverse)  #Carregar as bibliotecas
library(dplyr)



#Importa o banco de dados de 2016
#Retira os candidatos indígenas e amarelos
#Cria uma nova váriavel se diz se o candiato é branco ou não-branco
candidatos_2016<- read.csv2("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Banco_2016.csv", fileEncoding = "latin1")

candidatos_filtrado_2016 <- candidatos_2016 %>%
  filter(DS_COR_RACA %in% c("PARDA", "PRETA", "BRANCA")) %>%
  mutate(RACA_AGRUPADA = ifelse(DS_COR_RACA == "BRANCA", "Branco", "Não-Branco"))

table(candidatos_2016$DS_COR_RACA) #vê a quiantidade de candidatos por raça  



#A partir da tabela de candidatos filtra para os eleitos e coloca em ordem decrescente pelo número de votos
eleitos2016 <- candidatos_2016 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  arrange(desc(TOTAL_VOTOS))

#o último eleito recebeu 12464 votos



#Cria uma nova variável que diz se o candidato é competitivo ou não 
#Os competitivos tem pelo menos 2493 votos (20% do número de votos do último eleito)
candidatos_competitivos2016 <- candidatos_filtrado_2016 %>%
  mutate(COMPETITIVO = ifelse(TOTAL_VOTOS >= 2493, "Sim", "Não"))



#Cria uma nova variável que divide os competitivos em decis
candidatos_decis2016 <- candidatos_competitivos2016 %>%
  filter(COMPETITIVO == "Sim") %>%
  mutate(DECIL = ntile(TOTAL_VOTOS, 10))

candidatos_decis2016 %>%   #mostra o máximo e mínimo de cada decil
  group_by(DECIL) %>%
  summarise(
    minimo = min(TOTAL_VOTOS, na.rm = TRUE),
    maximo = max(TOTAL_VOTOS, na.rm = TRUE),
    .groups = "drop")
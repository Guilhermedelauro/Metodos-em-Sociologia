

Install.packages("tidyverse") #Instalar as bibliotecas
install.packages("dplyr")

library(tidyverse)  #Carregar as bibliotecas
library(dplyr)



#Importa o banco de dados de 2020
#Retira os candidatos indígenas e amarelos
#Cria uma nova váriavel se diz se o candiato é branco ou não-branco
candidatos_2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Banco_2020.csv", fileEncoding = "latin1")

candidatos_filtrado_2020 <- candidatos_2020 %>%
  filter(DS_COR_RACA %in% c("PARDA", "PRETA", "BRANCA")) %>%
  mutate(RACA_AGRUPADA = ifelse(DS_COR_RACA == "BRANCA", "Branco", "Não-Branco"))

table(candidatos_2020$DS_COR_RACA) #vê a quiantidade de candidatos por raça  



#A partir da tabela de candidatos filtra para os eleitos e coloca em ordem decrescente pelo número de votos
eleitos2020 <- candidatos_2020 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  arrange(desc(TOTAL_VOTOS))

#o último eleito recebeu 13673 votos



#Cria uma nova variável que diz se o candidato é competitivo ou não 
#Os competitivos tem pelo menos 2735 votos (20% do número de votos do último eleito)
candidatos_competitivos2020 <- candidatos_filtrado_2020 %>%
  mutate(COMPETITIVO = ifelse(TOTAL_VOTOS >= 2735, "Sim", "Não"))



#Cria uma nova variável que divide os competitivos em decis
candidatos_decis2020 <- candidatos_competitivos2020 %>%
  filter(COMPETITIVO == "Sim") %>%
  mutate(DECIL = ntile(TOTAL_VOTOS, 10))

candidatos_decis2020 %>%   #mostra o máximo e mínimo de cada decil
  group_by(DECIL) %>%
  summarise(
    minimo = min(TOTAL_VOTOS, na.rm = TRUE),
    maximo = max(TOTAL_VOTOS, na.rm = TRUE),
    .groups = "drop")




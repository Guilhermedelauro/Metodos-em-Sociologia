

Install.packages("tidyverse") #Instalar as bibliotecas
install.packages("dplyr")

library(tidyverse)  #Carregar as bibliotecas
library(dplyr)



#Importa o banco de dados de 2024
#Retira os candidatos indígenas e amarelos
#Cria uma nova váriavel se diz se o candiato é branco ou não-branco
candidatos_2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Banco_2024.csv", fileEncoding = "latin1")

candidatos_filtrado_2024 <- candidatos_2024 %>%
  filter(DS_COR_RACA %in% c( "PARDA", "PRETA", "BRANCA")) %>%
  mutate(RACA_AGRUPADA = ifelse(DS_COR_RACA == "BRANCA", "Branco", "Não-Branco"))

table(candidatos_2024$DS_COR_RACA) #vê a quiantidade de candidatos por raça



#A partir da tabela de candidatos filtra para os eleitos e coloca em ordem decrescente pelo número de votos
eleitos2024 <- candidatos_2024 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  arrange(desc(total_votos))

#o último eleito recebeu 22306 votos



#Cria uma nova variável que diz se o candidato é competitivo ou não 
#Os competitivos tem pelo menos 4461 votos (20% do número de votos do último eleito)
candidatos_competitivos2024 <- candidatos_filtrado_2024 %>%
  mutate(COMPETITIVO = ifelse(total_votos >= 4461, "Sim", "Não"))


#Cria uma nova variável que divide os competitivos em decis
candidatos_decis2024 <- candidatos_competitivos %>%
  filter(Competitivo == "Sim") %>%
  mutate(Decil = ntile(total_votos, 10))
  

candidatos_decis2024 %>%   #mostra o máximo e mínimo de cada decil
group_by(Decil) %>%
  summarise(
    minimo = min(total_votos, na.rm = TRUE),
    maximo = max(total_votos, na.rm = TRUE),
    .groups = "drop")

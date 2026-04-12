

Install.packages("tidyverse") #Instalar biblioteca

library(tidyverse)  #Carregar biblioteca


#Importa o banco de dados sobre os candidatos e filtra ele 

candidatos2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2024\\consulta_cand_2024_SP.csv", fileEncoding = "latin1")

banco_candidatos2024 <- candidatos2024 %>%
  filter(NM_UE %in% c("SÃO PAULO", "SAO PAULO" )) %>%
  filter(DS_CARGO == "VEREADOR") 
  filter(DS_SITUACAO_CANDIDATURA == "")  ####



#Importa o banco de dados dos bens e filtra por  
bens2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2024\\bem_candidato_2024_SP.csv", fileEncoding = "latin1")

banco_bens2024 <- bens2024 %>%
  filter(NM_UE %in% c("SÃO PAULO", "SAO PAULO")) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(bens_total = sum(VR_BEM_CANDIDATO))



#Importa o banco de dados do resultado e filtra 

resultado

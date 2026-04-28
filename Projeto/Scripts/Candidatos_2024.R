

Install.packages("tidyverse") #Instalar biblioteca

library(tidyverse)  #Carregar biblioteca



#Importa o banco de dados complementar sobre os candidatos
#Filtra para os candidatos que na situação do julgamento da urna estão deferidos ou deferidos com recurso
#Cria um novo banco com aas varíaveis desejadas
complementar2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2024\\consulta_cand_complementar_2024_SP.csv")

banco_complementar2024 <- complementar2024 %>%
  filter(DS_SITUACAO_JULGAMENTO_URNA %in% c("DEFERIDO", "DEFERIDO COM RECURSO"))

banco_complementar_filtrado2024 <- banco_complementar2024 %>%
  select(SQ_CANDIDATO, ST_REELEICAO, DS_SITUACAO_JULGAMENTO_URNA)



#Importa o banco de dados sobre os candidatos e filtra por:
#(I) candidatos do munícipio de São Paulo
#(II) vereadores
#Cria um novo banco com as varíaveis desejadas
candidatos2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2024\\consulta_cand_2024_SP.csv", fileEncoding = "latin1")

banco_candidatos2024 <- candidatos2024 %>%
  filter(NM_UE %in% c("SÃO PAULO", "SAO PAULO" )) %>%
  filter(DS_CARGO == "VEREADOR") 

banco_candidatos_filtrado2024 <- banco_candidatos2024 %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NM_SOCIAL_CANDIDATO,SG_PARTIDO,DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO, DS_SITUACAO_CANDIDATURA)



#Importa o banco de dados dos bens
#Filtra para os candidatos do munícipio de São Paulo
#Agrupa pelo SQ do candidato
#Soma o total de bens para cada candidato
bens2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2024\\bem_candidato_2024_SP.csv", fileEncoding = "latin1")

banco_bens2024 <- bens2024 %>%
  filter(NM_UE %in% c("SÃO PAULO", "SAO PAULO")) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(BENS_TOTAL = sum(VR_BEM_CANDIDATO))



#Importa o banco de dados do resultado
#Filtra por (I) candidatos do munícipio de São Paulo e (II) vereador
#Agrupa pelo SQ do candidato e pelo resultado da eleição por candidato
#Soma o total de votos para cada candidato
resultado2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2024\\votacao_candidato_munzona_2024_SP.csv", fileEncoding = "latin1")

banco_resultado2024 <- resultado2024 %>%
  filter(NM_UE %in% c("SÃO PAULO", "Sao PAULO")) %>%
  filter(DS_CARGO == "Vereador") %>%
  group_by(SQ_CANDIDATO, DS_SIT_TOT_TURNO) %>%
  summarise(TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS_VALIDOS))



#Importa o banco receita
#Filtra por (I) candidadatos do munícipio de São Paulo e (II) Vereadores
#Agrupa pelo SQ do candidato
#Soma o total de receita e a fonte da receita de cada candidato
receita2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2024\\receitas_candidatos_2024_SP.csv", fileEncoding = "latin1")

banco_receita2024 <- receita2024 %>%
  filter(NM_UE == "SÃO PAULO") %>%
  filter(DS_CARGO == "Vereador") %>%
  group_by(SQ_CANDIDATO)  %>%
  summarise(RECEITA_TOTAL = sum(VR_RECEITA),
            TIPO_RECEITA = paste(unique(DS_FONTE_RECEITA), collapse = ", "))



#Junta os bancos criados em 1 
banco_final2024 <- banco_candidatos_filtrado2024 %>%
  inner_join(banco_complementar_filtrado2024, by = "SQ_CANDIDATO") %>%
  left_join(banco_resultado2024, by = "SQ_CANDIDATO") %>%
  left_join(banco_receita2024, by = "SQ_CANDIDATO") %>%
  left_join(banco_bens2024, by = "SQ_CANDIDATO")



#Salva em CSV
write.csv2(banco_final2024, "Banco_2024.csv",row.names = FALSE,fileEncoding = "latin1" )
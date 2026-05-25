
#install.packages("tidyverse") #Instala a biblioteca

library(tidyverse)  #Carrega a biblioteca



#Importa o banco de dados dos candidatos e filtra por: 
# (I) candidatos do munícipio São Paulo
# (II) vereadores
# (III) que possuem a situação de candidatura apta
#Em seguida, cria um banco de dados com as varíaveis desejadas
candidatos2016 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\consulta_cand_2016_SP.csv", fileEncoding = "latin1")

banco_candidatos2016 <- candidatos2016 %>%
  filter(NM_UE == "SÃO PAULO") %>%
  filter(DS_CARGO == "VEREADOR") %>%
  filter(DS_SITUACAO_CANDIDATURA == "APTO")

banco_candidatos_filtrado2016 <- banco_candidatos2016 %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NM_SOCIAL_CANDIDATO,SG_PARTIDO,DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO, DS_SITUACAO_CANDIDATURA,ST_REELEICAO)



#Importa o banco de dados sobre bens
#Filtra para o munícipio de São Paulo
#Agrupa pelo SQ do candidato
#Soma o total de bens de cada candidato
bens2016 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\bem_candidato_2016_SP.csv", fileEncoding = "latin1")

banco_bens2016 <- bens2016 %>%
  filter(NM_UE == "SÃO PAULO") %>%
  group_by(SQ_CANDIDATO) %>%          
  summarise(BENS_TOTAL = sum(VR_BEM_CANDIDATO, na.rm = TRUE))



#Importa o banco de dados sobre o resultado da eleição
#Filtra por (I)candidatos do munícipio de São Paulo e (II) vereadores
#Agrupa pelo SQ do candidato e pelo resultado da eleição
#Soma o total de votos nominais válidos para cada candidato
resultado2016 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\votacao_candidato_munzona_2016_SP.csv", fileEncoding = "latin1")

banco_resultado2016 <- resultado2016 %>%
  filter(NM_UE == "SÃO PAULO") %>%
  filter(DS_CARGO == "Vereador") %>%
  group_by(SQ_CANDIDATO, DS_SIT_TOT_TURNO) %>%
  summarise(TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE))



#Importa o banco de receita
#Filtra por (I) candidatos do munícipio de São Paulo e (II) vereadores
#Agrupa pelo nome do candidato
#Soma o total de receita e o tipo da receita para cada candidato
#Muda o nome da coluna do nome do candidato
receita2016 <- read.table("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\receitas_candidatos_relatorio_financeiro_2016_SP.txt", header = TRUE, sep = ";", fileEncoding = "latin1", dec =",")


banco_receita2016 <- receita2016 %>%
  filter(Nome.da.UE == "SÃO PAULO") %>%
  filter(Cargo == "Vereador") %>%
  group_by(Nome.candidato) %>%
  summarise(RECEITA_TOTAL = sum(Valor.receita, na.rm = TRUE),
            FONTE_RECEITA = paste(unique(Fonte.recurso), collapse = ", "),
            ORIGEM_RECEITA = paste(unique(Tipo.receita), collapse = ", "))

banco_receita2016 <- banco_receita2016 %>%
  rename(NM_CANDIDATO = Nome.candidato)



# Junta os bancos criado em um a partir do banco_candidatos_filtrado2016, que possui todos os filtros
banco_final2016 <- banco_candidatos_filtrado2016 %>%
  left_join(banco_bens2016, by = "SQ_CANDIDATO") %>%        
  left_join(banco_resultado2016, by = "SQ_CANDIDATO") %>%
  left_join(banco_receita2016, by = "NM_CANDIDATO")



#Salva em csv
write.csv2(banco_final2016, "Banco_2016.csv",row.names = FALSE,fileEncoding = "latin1" )

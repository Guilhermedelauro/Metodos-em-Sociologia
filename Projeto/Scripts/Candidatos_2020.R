

install.packages("tidyverse") #Instala a biblioteca

library(tidyverse)  #Carrega a biblioteca



#Importa o banco de dados sobre os candidatos e filtra por:
# (I)candidatos de São Paulo
# (II)vereadores
# (III)situação de candidatura apto 
#Em seguida, cria um novo banco com as varíaveis desejadas
candidatos2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\consulta_cand_2020_SP.csv", fileEncoding = "latin1")

banco_candidatos2020 <- candidatos2020 %>%
  filter(NM_UE %in% c("SÃO PAULO", "SAO PAULO")) %>%
  filter(grepl("VEREADOR", DS_CARGO, ignore.case = TRUE)) %>%
  filter(DS_SITUACAO_CANDIDATURA == "APTO")

banco_filtrado2020 <- banco_candidatos2020 %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NM_SOCIAL_CANDIDATO, SG_PARTIDO, DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO,DS_SITUACAO_CANDIDATURA)



#Importa o banco de dados sobre os bens 
#Filtra para os candidatos de São Paulo
#Agrupa pelo SQ do candidato
#Soma os bens para cada candidato
bens2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\bem_candidato_2020_SP.csv", fileEncoding = "latin1" )

banco_bens2020 <- bens2020 %>%
  filter(grepl("SÃO PAULO", NM_UE, ignore.case = TRUE)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(bens_total = sum(VR_BEM_CANDIDATO))



#Importa o banco de dados sobre o resultado
#Filtra por: (I) candidatos de São Paulo e (II) vereadores
#Agrupa pelo SQ do candidato e pela resultado da eleição
#Soma a quantidade de votos para cada candidato
resultado2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\votacao_candidato_munzona_2020_SP.csv", fileEncoding = "latin1" )

banco_resultado2020 <- resultado2020 %>%
  filter(grepl("SÃO PAULO", NM_UE, ignore.case = TRUE)) %>%
  filter(grepl("VEREADOR", DS_CARGO, ignore.case = TRUE)) %>%
  group_by(SQ_CANDIDATO, DS_SIT_TOT_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS_NOMINAIS_VALIDOS)) 
 


#Importa o banco de dados sobre receita
#Filtra por: (I) candidatos de São Paulo e (II) vereadores
#Agrupa pelo SQ do candidato
#Soma a receita total, a origem da receita e a fonte da receita para cada candidato
receita2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\receitas_candidatos_2020_SP.csv", fileEncoding = "latin1" )

banco_receita2020 <- receita2020 %>%
  filter(grepl("SÃO PAULO", NM_UE, ignore.case = TRUE)) %>%
  filter(grepl("VEREADOR", DS_CARGO, ignore.case = TRUE)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(receita_total = sum(VR_RECEITA),
            origens = paste(unique(DS_ORIGEM_RECEITA), collapse = ", "),
            fontes = paste(unique(DS_FONTE_RECEITA), collapse = ", "))



#Junta os bancos criados em um, a partir do banco_filtrado2020 que possui todos os filtros
banco_final2020 <- banco_filtrado2020 %>%
left_join(banco_bens2020, by = "SQ_CANDIDATO") %>%        
  left_join(banco_resultado2020, by = "SQ_CANDIDATO") %>%
  left_join(banco_receita2020, by = "SQ_CANDIDATO")



#Salva em csv
write.csv2(banco_final2020, "Banco_2020.csv",row.names = FALSE,fileEncoding = "latin1" )
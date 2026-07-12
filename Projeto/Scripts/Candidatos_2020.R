
#install.packages("tidyverse") #Instala a biblioteca

library(tidyverse)  #Carrega a biblioteca           



#Importa o banco de dados complementar sobre os candidatos
#Seleciona as varíaveis desejadas
complementar2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\consulta_cand_complementar_2020_SP.csv" ,fileEncoding = "latin1")

banco_complementar2020 <- complementar2020 %>%
  select(SQ_CANDIDATO, ST_REELEICAO)


#Importa o banco de dados sobre os candidatos e filtra por:
# (I) candidatos de São Paulo
# (II) vereadores
#Cria um novo banco com as varíaveis desejadas
candidatos2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\consulta_cand_2020_SP.csv", fileEncoding = "latin1")

banco_candidatos2020 <- candidatos2020 %>%
  filter(toupper(NM_UE) =="SÃO PAULO", 
         toupper(DS_CARGO) == "VEREADOR")%>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NM_SOCIAL_CANDIDATO, SG_PARTIDO, DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO,DS_SITUACAO_CANDIDATURA)



#Importa o banco de dados sobre os bens 
#Filtra para os candidatos de São Paulo
#Agrupa pelo SQ do candidato
#Soma os bens para cada candidato
bens2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\bem_candidato_2020_SP.csv", fileEncoding = "latin1" )

banco_bens2020 <- bens2020 %>%
  filter(toupper(NM_UE) =="SÃO PAULO") %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(BENS_TOTAL = if (all(is.na(VR_BEM_CANDIDATO))) NA_real_ 
            else sum(VR_BEM_CANDIDATO, na.rm = TRUE),.groups = "drop")


#Importa o banco de dados sobre o resultado
#Filtra por: (I) candidatos de São Paulo e (II) vereadores
#Agrupa pelo SQ do candidato 
#Soma a quantidade de votos nominais válidos para cada candidato
resultado2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\votacao_candidato_munzona_2020_SP.csv", fileEncoding = "latin1" )

banco_resultado2020 <- resultado2020 %>%
  filter(toupper(NM_UE) =="SÃO PAULO",
         toupper(DS_CARGO) == "VEREADOR") %>%
  group_by(SQ_CANDIDATO)%>%
  summarise(TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE),
            DS_SIT_TOT_TURNO = first(DS_SIT_TOT_TURNO), 
            .groups = "drop") 
 


#Importa o banco de dados sobre receita
#Filtra por: (I) candidatos de São Paulo e (II) vereadores
#Agrupa pelo SQ do candidato
#Soma a receita total, a fonte da receita e a origem da receita para cada candidato
receita2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2020\\receitas_candidatos_2020_SP.csv", fileEncoding = "latin1" )

banco_receita2020 <- receita2020 %>%
  filter(toupper(NM_UE) == "SÃO PAULO",
        toupper(DS_CARGO) =="VEREADOR") %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(RECEITA_TOTAL = if (all(is.na(VR_RECEITA))) NA_real_ else sum(VR_RECEITA, na.rm = TRUE),
            FONTE_RECEITA = paste(unique(DS_FONTE_RECEITA), collapse = ", "),
            ORIGEM_RECEITA = paste(unique(DS_ORIGEM_RECEITA), collapse = ", "),
            .groups = "drop")



#Junta os bancos criados em um, a partir do banco_filtrado2020 que possui todos os filtros
#Filtra os casos que possuemm NA na variável número de votos 
banco_final2020 <- banco_candidatos2020 %>%
left_join(banco_bens2020, by = "SQ_CANDIDATO") %>%        
  left_join(banco_resultado2020, by = "SQ_CANDIDATO") %>%
  left_join(banco_receita2020, by = "SQ_CANDIDATO") %>%
  left_join(banco_complementar2020, by= "SQ_CANDIDATO")

banco_final2020 <- banco_final2020 %>%
  filter(!is.na(TOTAL_VOTOS))

#Salva em csv
write.csv2(banco_final2020, "Banco_2020.csv",row.names = FALSE,fileEncoding = "latin1" )

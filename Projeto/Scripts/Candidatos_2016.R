

install.packages("tidyverse") #Instala a biblioteca

library(tidyverse)  #Carrega a biblioteca



#Importa o banco de dados dos candidatos, filtra pra SP, vereador e apto e cria um novo banco com colunas selecionadas

candidatos2016 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\consulta_cand_2016_SP.csv", fileEncoding = "latin1")

banco_candidatos2016 <- candidatos2016 %>%
  filter(NM_UE == "SÃO PAULO") %>%
  filter(DS_CARGO == "VEREADOR") %>%
  filter(DS_SITUACAO_CANDIDATURA == "APTO")

banco_candidatos_filtrado2016 <- banco_candidatos %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NM_SOCIAL_CANDIDATO,SG_PARTIDO,DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO)



#Importa o banco de dados de bens e filtra por

bens2016 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\bem_candidato_2016_SP.csv", fileEncoding = "latin1")

banco_bens2016 <- bens2016 %>%
  filter(NM_UE == "SÃO PAULO") %>%
  group_by(SQ_CANDIDATO) %>%          
  summarise(bens_total = sum(VR_BEM_CANDIDATO))



#Importa o banco de dados sobre o resultado da eleição, filtra pra SP e vereador e soma o totoal de votos válidos

resultado2016 <- read.csv2("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\votacao_candidato_munzona_2016_SP.csv", fileEncoding = "latin1")

banco_resultado2016 <- resultado2016 %>%
  filter(NM_UE == "SÃO PAULO") %>%
  filter(DS_CARGO == "Vereador") %>%
  group_by(SQ_CANDIDATO, DS_SIT_TOT_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS_NOMINAIS_VALIDOS))



#Importa o banco de receita 

receita2016 <- read.table("C:\\Users\\guide\\OneDrive\\Área de Trabalho\\Candidatos_2016\\receitas_candidatos_relatorio_financeiro_2016_SP.txt", header = TRUE, sep = ";", fileEncoding = "latin1", dec =",")


banco_receita2016 <- receita2016 %>%
  filter(Nome.da.UE == "SÃO PAULO") %>%
  filter(Cargo == "Vereador") %>%
  group_by(Nome.candidato) %>%
  summarise(total_receita = sum(Valor.receita),
            tipo = paste(unique(Tipo.receita), collapse = ", "))






# 1. Bibliotecas e pacotes utilizados

#install.packages("tidyverse") #Instalar as bibliotecas apenas uma vez
#install.packages("janitor")
#install.packages("broom")
#install.packages("car")
#install.packages("marginaleffects")
#install.packages("logistf")

library(logistf)
library(tidyverse)  #Carregar as bibliotecas
library(janitor)
library(broom)
library(car)
library(marginaleffects)

#----------
# 2.Criação dos bancos de dados

#Importa o banco de dados de 2024
#Retira os candidatos indígenas e amarelos
#Cria uma nova váriavel que diz se o candiato é branco ou não branco
candidatos_2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Banco_2024.csv", fileEncoding = "latin1")

candidatos_filtrado_2024 <- candidatos_2024 %>%
  filter(DS_COR_RACA %in% c("PARDA", "PRETA", "BRANCA")) %>%
  mutate(RACA_AGRUPADA = if_else(DS_COR_RACA == "BRANCA", "Branco", "Não Branco"))

tabela_raca2024 <- table(candidatos_2024$DS_COR_RACA) #vê a quantidade de candidatos por raça


#A partir da tabela de candidatos filtra para os eleitos e coloca em ordem decrescente pelo número de votos 
#Cria uma nova variável que diz se o candidato é competitivo ou não 
#Os competitivos tem pelo menos 20% do número de votos do último eleito
eleitos2024 <- candidatos_2024 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  arrange(desc(TOTAL_VOTOS))

ultimo_eleito2024 <- min(eleitos2024$TOTAL_VOTOS)

candidatos_geral2024 <- candidatos_filtrado_2024 %>%    
  mutate(COMPETITIVO = if_else(TOTAL_VOTOS >= 0.2 * ultimo_eleito2024, "Sim", "Não"))

#------------
# 3.Tratamento dos bancos de dados

#Trata a base de dados dos candidatos em geral
#Transforma algumas variávels desejadas em factor
#Cria uma nova variável que diz se foi eleito(1) ou não eleito(0) 
#Remove os casos NA, com excesssão dos Bens
#Cria varíavies que dizem se aquele candidato possui aquele fonte de receita(fundo especial, fundo partidário ou outros recursos)(1) ou não possui (0) 
#Cria uma variável que diz se o candidato declarou bens(1) ou não(0) e uma variável que diz o valor dos bens, com 0 no lugar de NA
candidatos_semNA2024 <- candidatos_geral2024 %>% 
  mutate(RACA_AGRUPADA = as.factor(RACA_AGRUPADA),
    ST_REELEICAO = as.factor(ST_REELEICAO),
    DS_GENERO = as.factor(DS_GENERO),
    DS_GRAU_INSTRUCAO = as.factor(DS_GRAU_INSTRUCAO)) %>%
  mutate(ELEITO = if_else(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"), 1,0) ) %>%
  mutate(
    RECEITA_TOTAL = replace_na(RECEITA_TOTAL, 0),
    VALOR_BENS = replace_na(as.numeric(BENS_TOTAL), 0)
  ) %>%
  filter(
    !is.na(DS_SIT_TOT_TURNO),
    !is.na(COMPETITIVO),
    !is.na(TOTAL_VOTOS))
 
colSums(is.na(candidatos_semNA2024)) #Verifica onde tem NA

str(candidatos_semNA2024) #verifica o tipo de cada variável

#Cria uma nova base, onde competitivos estão divididos em decis a partir da receita
competitivos_semNA2024 <- candidatos_semNA2024 %>%
  filter(COMPETITIVO == "Sim")


#----------
# 4.Operações com os bancos de dados

#Faz a proporção de candidatos brancos e não-brancos
prop.table(table(candidatos_geral2024$RACA_AGRUPADA)) * 100 

num_cand2024 <- table(candidatos_geral2024$RACA_AGRUPADA) %>%
  as.data.frame()

#Faz a proporção de candidatos brancos e não-brancos competitivos
candidatos_geral2024 %>%
  filter(COMPETITIVO == "Sim") %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100) 

num_comp2024 <- candidatos_geral2024 %>%
  filter(COMPETITIVO == "Sim") %>%
  count(RACA_AGRUPADA) %>%
  as.data.frame()

#Faz a proporção de candidatos brancos e não-brancos eleitos 
candidatos_geral2024 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100) 

num_elei2024 <- candidatos_geral2024 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>%
  count(RACA_AGRUPADA) %>%
  as.data.frame()



#Coeficiente r para Receita de campanha e número de votos para os candidatos
r_cand_2024 <- candidatos_semNA2024 %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$estimate,
            p_valor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$p.value)

#Coeficiente r para Receita de campanha e número de votos para os competitivos
r_comp_2024 <- competitivos_semNA2024 %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$estimate,
            p_valor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$p.value)


#Faz o número de candidatos competitivos brancos e não-brancos por receiita
tabela_comp_receita_2024 <-competitivos_semNA2024%>%
  mutate(FAIXA_RECEITA = cut(RECEITA_TOTAL, 
                             breaks = c(-1, 0, 1000, 10000, 100000, Inf), 
                             labels = c("Sem receita", "1–1.000", "1.001–10.000", "10.001–100.000", "Mais de 100.000"),
                             include.lowest = TRUE)) %>%
  count(FAIXA_RECEITA, RACA_AGRUPADA) %>%
  group_by(FAIXA_RECEITA) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup()

print(tabela_comp_receita_2024)


#Conta o número de brancos e não brancos por partido para os candidatos
tabela_partido_candidato2024 <- candidatos_semNA2024 %>%
  tabyl(RACA_AGRUPADA,SG_PARTIDO)

#Conta o número de brancos e não brancos por partido para os competitivos
tabela_partido_competitivo2024 <- competitivos_semNA2024 %>%
  tabyl(RACA_AGRUPADA, SG_PARTIDO)


#Modelos de regressão 

#Modelo de regressão multipla para os candidatos 
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, valor dos bens 
modelo_lm2024 <- lm(log(TOTAL_VOTOS + 1 ) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS +1),
                data = candidatos_semNA2024)

summary(modelo_lm2024) #mostra o resultado do modelo

vif(modelo_lm2024) #vê se as variáveis independentes estão muito correlacionadas

plot(modelo_lm2024) #para ver os resíduos 

#Modelo de regressão logística para os candidatos(pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, reeleição, valor dos bens

modelo_logist2024 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = candidatos_semNA2024)

summary(modelo_logist2024)

exp(coef(modelo_logist2024))       # Odds Ratio
exp(confint(modelo_logist2024))   # Intervalo de Confiança dos Odds Ratio


#Modelo de regressão logística para os candidatos para ver se a raça muda o efeito da receita 
modelo_logist_interacao2024 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1)*RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = candidatos_semNA2024)


summary(modelo_logist_interacao2024)

drop1(modelo_logist_interacao2024) #para ver se a interação muda algo estatisticamente 


#Modelo de regressão multipla para os competitivos 
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, valor dos bens 
modelo_lm_com2024 <- lm(log(TOTAL_VOTOS +1) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
                data = competitivos_semNA2024)

summary(modelo_lm_com2024) #mostra o resultado do modelo

vif(modelo_lm_com2024) #vê se as variáveis independentes estão muito correlacionadas

plot(modelo_lm_com2024) #para ver os resíduos 

#Modelo de regressão logística para os competitivos (pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, reeleição, valor dos bens, 
modelo_logist_com2024 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = competitivos_semNA2024)

summary(modelo_logist_com2024)

exp(coef(modelo_logist_com2024))       # Odds Ratio
exp(confint(modelo_logist_com2024))   # Intervalo de Confiança dos Odds Ratio

#Modelo de regressão logística para os competitivos para ver se a raça muda o efeito da receita 
modelo_logist_com_interacao2024 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1)*RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = competitivos_semNA2024)

summary(modelo_logist_com_interacao2024)

drop1(modelo_logist_com_interacao2024) #para ver se a interação muda algo estatisticamente 



# 1. Bibliotecas e pacotes utilizados

Install.packages("tidyverse") #Instala as bibliotecas
install.packages("dplyr")
install.packages("janitor")
install.packages("broom")
install.packages("car")

library(tidyverse)  #Carrega as bibliotecas
library(dplyr)
library(janitor)
library(broom)
library(car)

#----------
# 2.Criação dos bancos de dados

#Importa o banco de dados de 2024
#Retira os candidatos indígenas e amarelos
#Cria uma nova váriavel que diz se o candiato é branco ou não-branco
candidatos_2024 <- read.csv2("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Banco_2024.csv", fileEncoding = "latin1")

candidatos_filtrado_2024 <- candidatos_2024 %>%
  filter(DS_COR_RACA %in% c( "PARDA", "PRETA", "BRANCA")) %>%
  mutate(RACA_AGRUPADA = ifelse(DS_COR_RACA == "BRANCA", "Branco", "Não-Branco"))

table(candidatos_2024$DS_COR_RACA) #vê a quantidade de candidatos por raça


#A partir da tabela de candidatos filtra para os eleitos e coloca em ordem decrescente pelo número de votos 
#Cria uma nova variável que diz se o candidato é competitivo ou não 
#Os competitivos tem pelo menos 4461 votos (20% do número de votos do último eleito)
eleitos2024 <- candidatos_2024 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  arrange(desc(TOTAL_VOTOS))

candidatos_competitivos2024 <- candidatos_filtrado_2024 %>%
  mutate(COMPETITIVO = ifelse(TOTAL_VOTOS >= 4461, "Sim", "Não"))


#Cria uma nova base, onde competitivos estão divididos em decis
candidatos_decis2024 <- candidatos_competitivos2024 %>%
  filter(COMPETITIVO == "Sim") %>%
  arrange(RECEITA_TOTAL) %>%
  mutate(DECIL = ntile(RECEITA_TOTAL, 10)) 
  
candidatos_decis2024 %>%  #Mostra o máximo e mínimo de cada decil
group_by(DECIL) %>%
  summarise(
    minimo = min(RECEITA_TOTAL, na.rm = TRUE),
    maximo = max(RECEITA_TOTAL, na.rm = TRUE),
    .groups = "drop")


#------------
# 3.Tratamento dos bancos de dados

#Trata a base de dados dos candidatos em geral
#Transforma algumas variávels desejadas em factor
#Cria uma nova variável que diz se foi eleito(1) ou não eleito(0) 
#Remove os casos NA, com excesssão dos Bens
candidatos_semNA <- candidatos_competitivos2024 %>% 
  mutate(
    RACA_AGRUPADA = as.factor(RACA_AGRUPADA),
    ST_REELEICAO = as.factor(ST_REELEICAO),
    DS_GENERO = as.factor(DS_GENERO),
    DS_GRAU_INSTRUCAO = as.factor(DS_GRAU_INSTRUCAO)) %>%
  mutate(ELEITO = ifelse(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"), 1,0) ) %>%
  filter(
    !is.na(DS_SIT_TOT_TURNO),
    !is.na(TIPO_RECEITA),
    !is.na(COMPETITIVO),
    !is.na(TOTAL_VOTOS),
    !is.na(RECEITA_TOTAL))

colSums(is.na(candidatos_semNA)) #Verifica onde tem NA

str(candidatos_semNA) #verifica o tipo de cada variável


#Trata da base de dados dos candidatos competitivos
#Transforma algumas variáveis desejadas em factor
#Cria uma nova variável que diz se foi eleito (1) ou não (2)
#Remove os casos NA, com exceção dos Bens
competitivos_semNA2024<- candidatos_decis2024 %>%
  mutate(
    RACA_AGRUPADA = as.factor(RACA_AGRUPADA),
    ST_REELEICAO = as.factor(ST_REELEICAO),
    DS_GENERO = as.factor(DS_GENERO),
    DS_GRAU_INSTRUCAO = as.factor(DS_GRAU_INSTRUCAO)) %>%
  mutate(ELEITO = ifelse(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"), 1,0) ) %>%
  filter(
    !is.na(DS_SIT_TOT_TURNO),
    !is.na(TIPO_RECEITA),
    !is.na(COMPETITIVO),
    !is.na(TOTAL_VOTOS),
    !is.na(RECEITA_TOTAL))

colSums(is.na(competitivos_semNA2024)) #Verifica onde tem NA

str(competitivos_semNA2024) #verifica o tipo de cada variável



#----------
# 4.Operações com os bancos de dados

#Faz a proporção de candidatos brancos e não-brancos
prop.table(table(candidatos_competitivos2024$RACA_AGRUPADA)) * 100

#Faz a proporção de candidatos brancos e não-brancos competitivos
candidatos_competitivos2024 %>%
  filter(COMPETITIVO == "Sim") %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100)

#Faz a proporção de candidatos brancos e não-brancos eleitos 
candidatos_competitivos2024 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100)


#Coeficiente r para Receita de campanha e número de votos para os candidatos
candidatos_semNA %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS, RECEITA_TOTAL)$estimate,
    p_valor = cor.test(TOTAL_VOTOS, RECEITA_TOTAL)$p.value)

#Coeficiente r para Receita de campanha e número de votos para os competitivos
competitivos_semNA2024 %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$estimate,
            p_valor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$p.value)



# Decil
# Como as variaveis estoa trelacionadas a raça (isso muda coocar raça nos modelos?)
# modelos e decil junto, tentar prever 
#verificar as variaveis dos modelos 





#Modelo de regressão multipla para os candiatos (pra ver o número de votos)
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, (bens)  (ocupação) (partido)

modelo_lm <- lm(log(TOTAL_VOTOS +1) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO,
                data = candidatos_semNA)

vif(modelo_lm) #vê se as variáveis independentes estão muito correlacionadas

tidy(modelo_lm, conf.int = TRUE) #visualiza os dados brutos

tabela_lm <- tidy(modelo_lm, conf.int = TRUE) %>% #cria uma tabela com os dados
  mutate(efeito_percentual = ifelse(grepl("log\\(", term),
      estimate * 100,                 
      (exp(estimate) - 1) * 100))       
    
  
#Modelo de regressão logística para os candidatos(pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, reeleição, (ocupação) (grau de instrução) (bens) (partido)
modelo_logist <- glm(ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO,
                  data = candidatos_semNA,
                  family = binomial(link = "logit"))

summary(modelo_logist)

vif(modelo_logist)


#Modelo de regressão multipla para os competitivos 
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, (bens)  (ocupação) (partido)
modelo_lm_com <- lm(log(TOTAL_VOTOS +1) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO,
                data = competitivos_semNA2024)

vif(modelo_lm_com) #vê se as variáveis independentes estão muito correlacionadas

tidy(modelo_lm_com, conf.int = TRUE)  #visualiza os dados brutos

tabela_lm_com <- tidy(modelo_lm_com, conf.int = TRUE) %>%  #cria uma tabela com os dados
  mutate(efeito_percentual = ifelse(grepl("log\\(", term),
                                    estimate * 100,                 
                                    (exp(estimate) - 1) * 100))       

#Modelo de regressão logística para os competitivos (pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, (bens)  (ocupação) (partido) 
modelo_logist_com <- glm(ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO,
                     data =competitivos_semNA2024,
                     family = binomial(link = "logit"))

summary(modelo_logist_com)

vif(modelo_logist_com)



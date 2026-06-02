
# 1. Bibliotecas e pacotes utilizados

#install.packages("tidyverse") #Instalar as bibliotecas apenas uma vez
#install.packages("janitor")
#install.packages("broom")
#install.packages("car")
#install.packages("marginaleffects")

library(tidyverse)  #Carregar as bibliotecas
library(janitor)
library(broom)
library(car)
library(marginaleffects)

#----------
# 2.Criação dos bancos de dados

#Importa o banco de dados de 2020
#Retira os candidatos indígenas e amarelos
#Cria uma nova váriavel que diz se o candiato é branco ou não-branco
candidatos_2020 <- read.csv2("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Banco_2020.csv", fileEncoding = "latin1")

candidatos_filtrado_2020 <- candidatos_2020 %>%
  filter(DS_COR_RACA %in% c("PARDA", "PRETA", "BRANCA")) %>%
  mutate(RACA_AGRUPADA = if_else(DS_COR_RACA == "BRANCA", "Branco", "Não-Branco"))

tabela_raca2020 <- table(candidatos_2020$DS_COR_RACA) #vê a quantidade de candidatos por raça  


#A partir da tabela de candidatos filtra para os eleitos e coloca em ordem decrescente pelo número de votos
#Cria uma nova variável que diz se o candidato é competitivo ou não 
#Os competitivos tem pelo menos 2735 votos (20% do número de votos do último eleito)
eleitos2020 <- candidatos_2020 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  arrange(desc(TOTAL_VOTOS))

ultimo_eleito2020 <- min(eleitos2020$TOTAL_VOTOS)

candidatos_geral2020 <- candidatos_filtrado_2020 %>%    ####
  mutate(COMPETITIVO = if_else(TOTAL_VOTOS >= 0.2 * ultimo_eleito2020, "Sim", "Não"))


#Cria uma nova base onde os candidatos competitivos estão divididos em decis de receita
candidatos_competitivos2020 <- candidatos_geral2020 %>%
  filter(COMPETITIVO == "Sim") %>%
  arrange(RECEITA_TOTAL) %>%
  mutate(DECIL_RECEITA = ntile(RECEITA_TOTAL, 10))

candidatos_competitivos2020 %>%   #mostra o máximo e mínimo de cada decil
  group_by(DECIL_RECEITA) %>%
  summarise(
    minimo = min(TOTAL_VOTOS, na.rm = TRUE),
    maximo = max(TOTAL_VOTOS, na.rm = TRUE),
    .groups = "drop")

#------------
# 3.Tratamento dos bancos de dados

#Trata a base de dados dos candidatos em geral
#Transforma algumas variávels desejadas em factor
#Cria uma nova variável que diz se foi eleito(1) ou não eleito(0) 
#Remove os casos NA, com excesssão dos Bens
#Cria varíavies que dizem se aquele candidato possui aquele fonte de receita(fundo especial, fundo partidário ou outros recursos)(1) ou não possui (0) 
#Cria uma variável que diz se o candidato declarou bens(1) ou não(0) e uma variável que diz o valor dos bens, com 0 no lugar de NA
candidatos_semNA2020 <- candidatos_geral2020 %>% 
  mutate(
    RACA_AGRUPADA = as.factor(RACA_AGRUPADA),
    ST_REELEICAO = as.factor(ST_REELEICAO),
    DS_GENERO = as.factor(DS_GENERO),
    DS_GRAU_INSTRUCAO = as.factor(DS_GRAU_INSTRUCAO)) %>%
  mutate(ELEITO = if_else(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"), 1,0) ) %>%
  filter(
    !is.na(DS_SIT_TOT_TURNO),
    !is.na(COMPETITIVO),
    !is.na(TOTAL_VOTOS),
    !is.na(RECEITA_TOTAL)) %>%
  mutate(DECLAROU_BENS = if_else(is.na(BENS_TOTAL), 0, 1),   
         VALOR_BENS = if_else(is.na(BENS_TOTAL),0, as.numeric(BENS_TOTAL))) 

colSums(is.na(candidatos_semNA2020)) #Verifica onde tem NA

str(candidatos_semNA2020) #verifica o tipo de cada variável


#Trata da base de dados dos candidatos competitivos
#Transforma algumas variáveis desejadas em factor
#Cria uma nova variável que diz se foi eleito (1) ou não (2)
#Remove os casos NA, com exceção dos Bens
#Cria varíavies que dizem se aquele candidato possui aquele fonte de receita(fundo especial, fundo partidário ou outros recursos)(1) ou não possui (0) 
#Cria uma variável que diz se o candidato declarou bens(1) ou não(0) e uma variável que diz o valor dos bens, com 0 no lugar de NA
competitivos_semNA2020<- candidatos_competitivos2020 %>%
  mutate(
    RACA_AGRUPADA = as.factor(RACA_AGRUPADA),
    ST_REELEICAO = as.factor(ST_REELEICAO),
    DS_GENERO = as.factor(DS_GENERO),
    DS_GRAU_INSTRUCAO = as.factor(DS_GRAU_INSTRUCAO)) %>%
  mutate(ELEITO = if_else(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"), 1,0) ) %>%
  filter(
    !is.na(DS_SIT_TOT_TURNO),
    !is.na(COMPETITIVO),
    !is.na(TOTAL_VOTOS),
    !is.na(RECEITA_TOTAL)) %>%
  mutate(DECLAROU_BENS = if_else(is.na(BENS_TOTAL), 0, 1),   
         VALOR_BENS = if_else(is.na(BENS_TOTAL),0, as.numeric(BENS_TOTAL)))

colSums(is.na(competitivos_semNA2020)) #Verifica onde tem NA

str(competitivos_semNA2020) #verifica o tipo de cada variável


#----------
# 4.Operações com os bancos de dados

#Faz a proporção de candidatos brancos e não-brancos
prop.table(table(candidatos_geral2020$RACA_AGRUPADA)) * 100

#Faz a proporção de candidatos brancos e não-brancos para os competitivos
candidatos_competitivos2020 %>%
  filter(COMPETITIVO == "Sim") %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100)

#Faz a proporção de candidatos brancos e não-brancos para os eleitos 
candidatos_geral2020 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100)


#Coeficiente r para Receita de campanha e número de votos para os candidatos
candidatos_semNA2020 %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS, RECEITA_TOTAL)$estimate,
            p_valor = cor.test(TOTAL_VOTOS, RECEITA_TOTAL)$p.value)

#Coeficiente r para Receita de campanha e número de votos para os competitivos
competitivos_semNA2020 %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$estimate,
            p_valor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$p.value)


#Faz a proporção de candidatos competitivos brancos e não-brancos a cada decil de receita
tabela_decil_receita2020 <- competitivos_semNA2020 %>%
  count(RACA_AGRUPADA, DECIL_RECEITA) %>%
  group_by(DECIL_RECEITA) %>%
  mutate(prop = n / sum(n)) 

tabela_decil_receita2020 %>%   #Para garantir q a soma de cada decil da 1 
  group_by(DECIL_RECEITA) %>%
  summarise(soma = sum(prop))


#Conta o número de brancos e não brancos por partido para os candidatos
tabela_partido_candidato2020 <- candidatos_semNA2020 %>%
  tabyl(RACA_AGRUPADA,SG_PARTIDO)

#Conta o número de brancos e não brancos por partido para os competitivos
tabela_partido_competitivo2020 <- competitivos_semNA2020 %>%
  tabyl(RACA_AGRUPADA, SG_PARTIDO)


#Modelos de regressão 

#Modelo de regressão multipla para os candidatos 
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, declarou bens, valor dos bens
modelo_lm2020 <- lm(log(TOTAL_VOTOS +1) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + DECLAROU_BENS + VALOR_BENS,
                    data = candidatos_semNA2020)

summary(modelo_lm2020) #mostra o resultado do modelo

vif(modelo_lm2020) #vê se as variáveis independentes estão muito correlacionadas

tidy(modelo_lm2020, conf.int = TRUE) #visualiza os dados brutos

plot(modelo_lm2020) #para ver os resíduos 

#Modelo de regressão logística para os candidatos(pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, reeleição, declarou bens, valor dos bens
modelo_logist2020 <- glm(ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + DECLAROU_BENS + VALOR_BENS,
                         data = candidatos_semNA2020,
                         family = binomial(link = "logit"))

summary(modelo_logist2020)


exp(cbind(
  OR = coef(modelo_logist2020),
  confint(modelo_logist2020)))

#Modelo de regressão logística para os candidatos para ver se a raça muda o efeito da receita 
modelo_logist_interacao2020 <- glm(ELEITO ~ log(RECEITA_TOTAL + 1)*RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + DECLAROU_BENS + VALOR_BENS,
                                   data = candidatos_semNA2020,
                                   family = binomial(link = "logit"))

summary(modelo_logist_interacao2020)

anova(modelo_logist2020, modelo_logist_interacao2020, test =  "Chisq") #para ver se a interação muda algo estatisticamente 


#Modelo de regressão multipla para os competitivos 
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, declarou bens, valor dos bens
modelo_lm_com2020 <- lm(log(TOTAL_VOTOS +1) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + DECLAROU_BENS + VALOR_BENS,
                        data = competitivos_semNA2020)

summary(modelo_lm_com2020) #mostra o resultado do modelo

vif(modelo_lm_com2020) #vê se as variáveis independentes estão muito correlacionadas

tidy(modelo_lm_com2020, conf.int = TRUE)  #visualiza os dados brutos

plot(modelo_lm_com2020) #para ver os resíduos 

#Modelo de regressão logística para os competitivos (pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, reeleição, declarou bens, valor dos bens
modelo_logist_com2020 <- glm(ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + DECLAROU_BENS + VALOR_BENS,
                             data =competitivos_semNA2020,
                             family = binomial(link = "logit"))

summary(modelo_logist_com2020)

exp(cbind(
  OR = coef(modelo_logist_com2020),
  confint(modelo_logist_com2020)))

#Modelo de regressão logística para os competitivos para ver se a raça muda o efeito da receita 
modelo_logist_com_interacao2020 <- glm(ELEITO ~ log(RECEITA_TOTAL + 1)*RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + DECLAROU_BENS + VALOR_BENS,
                                       data = competitivos_semNA2020,
                                       family = binomial(link = "logit"))

summary(modelo_logist_com_interacao2020)

anova(modelo_logist_com2020, modelo_logist_com_interacao2020, test =  "Chisq") #para ver se a interação muda algo estatisticamente 

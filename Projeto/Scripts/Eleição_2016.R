
# 1. Bibliotecas e pacotes utilizados

#install.packages("tidyverse") #Instalar as bibliotecas apenas uma vez
#install.packages("janitor")
#install.packages("broom")
#install.packages("car")
#install.packages("marginaleffects")

library(logistf)
library(tidyverse)  #Carregar as bibliotecas
library(janitor)
library(broom)
library(car)
library(marginaleffects)

#----------
# 2.Criação dos bancos de dados

#Importa o banco de dados de 2016
#Retira os candidatos indígenas e amarelos
#Cria uma nova váriavel que diz se o candiato é branco ou não-branco
candidatos_2016 <- read.csv2("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Banco_2016.csv", fileEncoding = "latin1")

candidatos_filtrado_2016 <- candidatos_2016 %>%
  filter(DS_COR_RACA %in% c("PARDA", "PRETA", "BRANCA")) %>%
  mutate(RACA_AGRUPADA = if_else(DS_COR_RACA == "BRANCA", "Branco", "Não Branco"))

tabela_raca2016 <- table(candidatos_2016$DS_COR_RACA) #vê a quantidade de candidatos por raça  


#A partir da tabela de candidatos filtra para os eleitos e coloca em ordem decrescente pelo número de votos
#Cria uma nova variável que diz se o candidato é competitivo ou não 
#Os competitivos tem pelo menos 20% do número de votos do último eleito
eleitos2016 <- candidatos_2016 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  arrange(desc(TOTAL_VOTOS))

ultimo_eleito2016 <- min((eleitos2016$TOTAL_VOTOS))

candidatos_geral2016 <- candidatos_filtrado_2016 %>%    
  mutate(COMPETITIVO = if_else(TOTAL_VOTOS >= 0.2 * ultimo_eleito2016, "Sim", "Não"))


#------------
# 3.Tratamento dos bancos de dados

#Trata a base de dados dos candidatos em geral
#Transforma algumas variávels desejadas em factor
#Cria uma nova variável que diz se foi eleito(1) ou não eleito(0) 
#Remove os casos NA, com excesssão dos Bens
#Cria varíavies que dizem se aquele candidato possui aquele fonte de receita(fundo especial, fundo partidário ou outros recursos)(1) ou não possui (0) 
#Cria uma variável que diz se o candidato declarou bens(1) ou não(0) e uma variável que diz o valor dos bens, com 0 no lugar de NA
candidatos_semNA2016 <- candidatos_geral2016 %>% 
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
    

colSums(is.na(candidatos_semNA2016)) #Verifica onde tem NA

str(candidatos_semNA2016) #verifica o tipo de cada variável


#Cria uma nova base onde os competitivos estão divididos em decis a partir da receita 
competitivos_semNA2016 <- candidatos_semNA2016%>%
  filter(COMPETITIVO == "Sim") 

#----------
# 4.Operações com os bancos de dados

#Faz a proporção de candidatos brancos e não-brancos
prop.table(table(candidatos_geral2016$RACA_AGRUPADA)) * 100 

num_cand2016 <- table(candidatos_geral2016$RACA_AGRUPADA) %>%
  as.data.frame()

#Faz a proporção de candidatos brancos e não-brancos para os competitivos
candidatos_geral2016 %>%
  filter(COMPETITIVO == "Sim") %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100) 
  
num_comp2016 <-candidatos_geral2016 %>%
  filter(COMPETITIVO == "Sim") %>%
  count(RACA_AGRUPADA) %>%
  as.data.frame()
  
#Faz a proporção de candidatos brancos e não-brancos para os eleitos 
candidatos_geral2016 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>%
  with(prop.table(table(RACA_AGRUPADA)) * 100) 

num_elei2016 <- candidatos_geral2016 %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>%
  count(RACA_AGRUPADA) %>%
  as.data.frame()


#Coeficiente r para Receita de campanha e número de votos para os candidatos
r_cand_2016 <- candidatos_semNA2016 %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS, RECEITA_TOTAL)$estimate,
            p_valor = cor.test(TOTAL_VOTOS, RECEITA_TOTAL)$p.value)

#Coeficiente r para Receita de campanha e número de votos para os competitivos
r_comp_2016 <- competitivos_semNA2016 %>%
  group_by(RACA_AGRUPADA) %>%
  summarise(cor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$estimate,
            p_valor = cor.test(TOTAL_VOTOS,RECEITA_TOTAL)$p.value)


#Faz o número de candidatos competitivos brancos e não-brancos por receiita
tabela_comp_receita_2016 <-competitivos_semNA2016%>%
  mutate(FAIXA_RECEITA = cut(RECEITA_TOTAL, 
                             breaks = c(-1, 0, 1000, 10000, 100000, Inf), 
                             labels = c("Sem receita", "1–1.000", "1.001–10.000", "10.001–100.000", "Mais de 100.000"),
                             include.lowest = TRUE)) %>%
    count(FAIXA_RECEITA, RACA_AGRUPADA) %>%
    group_by(FAIXA_RECEITA) %>%
    mutate(prop = n / sum(n) * 100) %>%
    ungroup()
  
print(tabela_comp_receita_2016)

#Conta o número de brancos e não brancos por partido para os candidatos
tabela_partido_candidato2016 <- candidatos_semNA2016 %>%
  tabyl(RACA_AGRUPADA,SG_PARTIDO)

#Conta o número de brancos e não brancos por partido para os competitivos
tabela_partido_competitivo2016 <- competitivos_semNA2016 %>%
  tabyl(RACA_AGRUPADA, SG_PARTIDO)


#Modelos de regressão 

#Modelo de regressão multipla para os candidatos 
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição,  valor dos bens
modelo_lm2016 <- lm(log(TOTAL_VOTOS +1) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS +1),
                    data = candidatos_semNA2016)

summary(modelo_lm2016) #mostra o resultado do modelo

vif(modelo_lm2016) #vê se as variáveis independentes estão muito correlacionadas


plot(modelo_lm2016) #para ver os resíduos 

#Modelo de regressão logística para os candidatos(pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, reeleição, valor dos bens
modelo_logist2016 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = candidatos_semNA2016)

summary(modelo_logist2016)


exp(coef(modelo_logist2016))       # Odds Ratio
exp(confint(modelo_logist2016))   # Intervalo de Confiança dos Odds Ratio

#Modelo de regressão logística para os candidatos para ver se a raça muda o efeito da receita 

modelo_logist_interacao2016 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1)*RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = candidatos_semNA2016,
  control = logistf.control(maxit = 1000))

summary(modelo_logist_interacao2016)

drop1(modelo_logist_interacao2016)

#Modelo de regressão multipla para os competitivos 
#Variável dependente: número de votos
#Variáveis independentes: receita, gênero, branco ou não-branco, grau de instrução, reeleição, valor dos bens 
modelo_lm_com2016 <- lm(log(TOTAL_VOTOS +1) ~ log(RECEITA_TOTAL +1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS +1),
                        data = competitivos_semNA2016)

summary(modelo_lm_com2016) #mostra o resultado do modelo

vif(modelo_lm_com2016) #vê se as variáveis independentes estão muito correlacionadas

plot(modelo_lm_com2016) #para ver os resíduos 

#Modelo de regressão logística para os competitivos (pra ver a chance de ser eleito)
#Variável dependente: eleito ou não
#Variáveis independentes: receita, gênero, branco ou não-branco, reeleição, valor dos bens
modelo_logist_com2016 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1) + RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = competitivos_semNA2016)

summary(modelo_logist_com2016)

exp(coef(modelo_logist_com2016))       # Odds Ratio
exp(confint(modelo_logist_com2016))    # Intervalo de Confiança dos Odds Ratio

#Modelo de regressão logística para os competitivos para ver se a raça muda o efeito da receita 
modelo_logist_com_interacao2016 <- logistf(
  ELEITO ~ log(RECEITA_TOTAL + 1)*RACA_AGRUPADA + ST_REELEICAO + DS_GENERO + DS_GRAU_INSTRUCAO + log(VALOR_BENS + 1),
  data = competitivos_semNA2016,
  control = logistf.control(maxit = 1000))

summary(modelo_logist_com_interacao2016)

drop1(modelo_logist_com_interacao2016)


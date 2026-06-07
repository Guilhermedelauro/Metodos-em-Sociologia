
# 1. Bibliotecas
install.packages("ggplot2")

install.packages("pandoc")
install.packages("modelsummary")


library(ggplot2)
library(tidyverse)
library (flextable)
library(scales)
library(modelsummary)
library(pandoc)
# 2. Bancos de dados 

source("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Projeto\\Scripts\\Eleição_2016.R")
source("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Projeto\\Scripts\\Eleição_2020.R")
source("C:\\Users\\guide\\OneDrive\\R pasta\\Metodos-em-Sociologia\\Projeto\\Scripts\\Eleição_2024.R")

#3. Gráficos e Tabelas 

#gráfico de raça por ano
tabela_raca2016 <- as.data.frame(tabela_raca2016)
tabela_raca2016$Ano <- 2016

tabela_raca2020 <- as.data.frame(tabela_raca2020)
tabela_raca2020$Ano <- 2020

tabela_raca2024 <- as.data.frame(tabela_raca2024)
tabela_raca2024$Ano <- 2024

tabela_raca_junto <- rbind(tabela_raca2016, tabela_raca2020, tabela_raca2024)

tabela_raca_junto$Var1 <- factor(tabela_raca_junto$Var1,
  levels = c("BRANCA","PARDA","PRETA","AMARELA","INDÍGENA","NÃO DIVULGÁVEL" ))

barra_raca <- ggplot(tabela_raca_junto, aes(x=Freq, y=Var1, fill= factor(Ano))) + 
  geom_col(position = position_dodge2(width = 0.6), width = 0.9) + 
  theme_minimal(base_family = "Times", base_size = 12) + 
  labs(x= "Quantidade de candidatos", y= "Raça", fill = "Ano") +
  theme(legend.position = "bottom",panel.grid.minor = element_blank(),panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(colour = "grey85",linewidth = 0.3)) +
  scale_x_continuous(limits = c(0,1200), breaks = c(0,200,400,600,800,1000,1200)) +
  geom_text(aes(label = Freq),position = position_dodge(width = 1), hjust = -0.2, vjust = 0.35, size = 3) +
  scale_fill_brewer(palette = "Set2") 
  
  
print(barra_raca)


#tabela raça candidatos

num_cand2016$ANO <- 2016
num_cand2020$ANO <- 2020
num_cand2024$ANO <- 2024


num_junto_cand <- rbind(num_cand2016, num_cand2020, num_cand2024)
 
junto_cand <- num_junto_cand %>%
  group_by(ANO) %>%
  mutate(prop = Freq/ sum(Freq)*100) %>%
  ungroup()

tabela_candidato <- junto_cand %>%
  pivot_wider(names_from = ANO,values_from = c(Freq, prop)) %>%
  mutate(var_total = (Freq_2024 - Freq_2016)/ Freq_2016 * 100) %>% 
  rename(Raça = "Var1", "2016(N)*"= Freq_2016, 
         "2020(N)"= Freq_2020, 
         "2024(N)"= Freq_2024, 
         "2016(P)**"= prop_2016, 
         "2020(P)"= prop_2020, 
         "2024(P)"= prop_2024, 
         "Variação total***" = var_total)

tabela_candidato <- flextable(tabela_candidato) %>% 
  theme_booktabs() %>% 
  autofit() %>% 
  align(align = "center", part = "all") %>%
  colformat_double(j = c("2016(P)**", "2020(P)", "2024(P)", "Variação total***"),
                   digits = 2,
                   suffix = "%", 
                   decimal.mark= ",", 
                   big.mark = ".") %>%
  colformat_num(j = c("2016(N)*", "2020(N)","2024(N)"), big.mark = "") %>%
  add_footer_lines(c(
    "* Número de candidatos",
    "** Porcentagem de candidatos",
    "*** Variação do número de candidatos de 2016 para 2024")) %>%
  font(fontname = "Times New Roman",part = "all") 



save_as_image(tabela_candidato, path="tabela_candidato.png")

#tabela raça competitivos 


num_comp2016$Ano <- 2016
num_comp2020$Ano <- 2020
num_comp2024$Ano <- 2024

num_junto_comp <- rbind(num_comp2016, num_comp2020, num_comp2024)

junto_comp <- num_junto_comp %>%
  group_by(Ano) %>%
  mutate(prop = n/ sum(n)*100) %>%
  ungroup()

tabela_competitivo <- junto_comp %>%
  pivot_wider(names_from = Ano,values_from = c(n, prop)) %>%
  mutate(var_total = (n_2024 - n_2016)/ n_2016 * 100) %>% 
  rename(Raça = "RACA_AGRUPADA", "2016(N)*"= n_2016, 
         "2020(N)"= n_2020, 
         "2024(N)"= n_2024, 
         "2016(P)**"= prop_2016, 
         "2020(P)"= prop_2020, 
         "2024(P)"= prop_2024, 
         "Variação total***" = var_total)

tabela_competitivo <- flextable(tabela_competitivo) %>% 
  theme_booktabs() %>% 
  autofit() %>% 
  align(align = "center", part = "all") %>%
  colformat_double(j = c("2016(P)**", "2020(P)", "2024(P)", "Variação total***"),
                   digits = 2,
                   suffix = "%", 
                   decimal.mark= ",", 
                   big.mark = ".") %>%
  colformat_num(j = c("2016(N)*", "2020(N)","2024(N)"), big.mark = "") %>%
  add_footer_lines(c(
    "* Número de candidatos",
    "** Porcentagem de candidatos",
    "*** Variação do número de candidatos de 2016 para 2024")) %>%
  font(fontname = "Times New Roman",part = "all") 


save_as_image(tabela_competitivo, path="tabela_competitivo.png")


#tabela raça eleitos

num_elei2016$Ano <- 2016
num_elei2020$Ano <- 2020
num_elei2024$Ano <- 2024

num_junto_elei <- rbind(num_elei2016, num_elei2020, num_elei2024)

junto_elei <- num_junto_elei %>%
  group_by(Ano) %>%
  mutate(prop = n/ sum(n)*100) %>%
  ungroup()

tabela_eleito <- junto_elei %>%
  pivot_wider(names_from = Ano,values_from = c(n, prop)) %>%
  mutate(var_total = (n_2024 - n_2016)/ n_2016 * 100) %>% 
  rename(Raça = "RACA_AGRUPADA", "2016(N)*"= n_2016, 
         "2020(N)"= n_2020, 
         "2024(N)"= n_2024, 
         "2016(P)**"= prop_2016, 
         "2020(P)"= prop_2020, 
         "2024(P)"= prop_2024, 
         "Variação total***" = var_total)

tabela_eleito <- flextable(tabela_eleito) %>% 
  theme_booktabs() %>% 
  autofit() %>% 
  align(align = "center", part = "all") %>%
  colformat_double(j = c("2016(P)**", "2020(P)", "2024(P)", "Variação total***"),
                   digits = 2,
                   suffix = "%", 
                   decimal.mark= ",", 
                   big.mark = ".") %>%
  colformat_num(j = c("2016(N)*", "2020(N)","2024(N)"), big.mark = "") %>%
  add_footer_lines(c(
    "* Número de candidatos",
    "** Porcentagem de candidatos",
    "*** Variação do número de candidatos de 2016 para 2024")) %>%
  font(fontname = "Times New Roman",part = "all") 


save_as_image(tabela_eleito, path="tabela_eleito.png")


# Coeficiente r receita e número de votos candidatos 

r_cand_2016$Ano <- 2016
r_cand_2020$Ano <- 2020
r_cand_2024$Ano <- 2024

r_junto_cand <- rbind(r_cand_2016, r_cand_2020, r_cand_2024)


r_junto_cand <- r_junto_cand %>%
  mutate(
    p_valor = as.numeric(gsub(",", ".", p_valor)),
    p_valor = formatC(p_valor, format = "e", digits = 2, decimal.mark = ","),
    Ano = as.character(Ano))


tabela_r_cand <- r_junto_cand %>%
  pivot_wider(names_from = RACA_AGRUPADA,values_from = c(cor, p_valor)) %>% 
  rename("r Branco" = cor_Branco,
         "r Não Branco" = 'cor_Não Branco', 
         "p-valor Branco" = p_valor_Branco,
         "p-valor Não Branco" = 'p_valor_Não Branco')

tabela_r_cand<- flextable(tabela_r_cand) %>% 
  theme_booktabs() %>% 
  autofit() %>% 
  align(align = "center", part = "all") %>%
  colformat_double(j = c("r Branco", "r Não Branco"),
                   digits = 2,
                   decimal.mark= ",", 
                   big.mark = "") 

print(tabela_r_cand)

save_as_image(tabela_r_cand, path="tabela_r_cand.png")


#Coeficiente r receita e númerod e votos competitivos 

r_comp_2016$Ano <- 2016
r_comp_2020$Ano <- 2020
r_comp_2024$Ano <- 2024

r_junto_comp<- rbind(r_comp_2016, r_comp_2020, r_comp_2024)


r_junto_comp <- r_junto_comp %>%
  mutate(
    p_valor = as.numeric(gsub(",", ".", p_valor)),
    p_valor = formatC(p_valor, format = "e", digits = 2, decimal.mark = ","),
    Ano = as.character(Ano))

tabela_r_comp <- r_junto_comp %>%
  pivot_wider(names_from = RACA_AGRUPADA,values_from = c(cor, p_valor)) %>% 
  rename("r Branco" = cor_Branco,
         "r Não Branco" = 'cor_Não Branco', 
         "p-valor Branco" = p_valor_Branco,
         "p-valor Não Branco" = 'p_valor_Não Branco')

tabela_r_comp<- flextable(tabela_r_comp) %>% 
  theme_booktabs() %>% 
  autofit() %>% 
  align(align = "center", part = "all") %>%
  colformat_double(j = c("r Branco", "r Não Branco"),
                   digits = 2,
                   decimal.mark= ",", 
                   big.mark = "") 

print(tabela_r_comp)
save_as_image(tabela_r_comp, path="tabela_r_comp.png")



#divisão receita 2016
windowsFonts(
  Times = windowsFont("Times New Roman")
)

ggplot(tabela_comp_receita_2016, aes(x = FAIXA_RECEITA, y = prop / 100, fill = RACA_AGRUPADA)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent,  limits = c(0, 1)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_family = "Times", base_size = 12) +   
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotaciona os rótulos longos
  ) +
  labs(x = "Faixa de Receita", y = "Proporção", fill = "Raça") 


candidatos_semNA2016$FAIXA_RECEITA <- cut(
  candidatos_semNA2016$RECEITA_TOTAL, 
  breaks = c(-1, 0, 1000, 10000, 100000, Inf), 
  labels = c(
    "Sem receita", 
    "1–1.000", 
    "1.001–10.000", 
    "10.001–100.000", 
    "Mais de 100.000"),
  include.lowest = TRUE)



ggplot(candidatos_semNA2016, aes(x = FAIXA_RECEITA, fill = RACA_AGRUPADA)) +
  geom_bar(position = "dodge") +
  labs(x = "Receita",
       y = "Quantidade de candidatos",
       fill = "Raça") +
  theme_minimal(base_family = "Times", base_size = 12) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(colour = "grey85",linewidth = 0.3)) 


  

#divisão receita 2020

ggplot(tabela_comp_receita_2020, aes(x = FAIXA_RECEITA, y = prop / 100, fill = RACA_AGRUPADA)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent,  limits = c(0, 1)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_family = "Times", base_size = 12) +   
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotaciona os rótulos longos
  ) +
  labs(x = "Faixa de Receita", y = "Proporção", fill = "Raça")


candidatos_semNA2020$FAIXA_RECEITA <- cut(
  candidatos_semNA2020$RECEITA_TOTAL, 
  breaks = c(-1, 0, 1000, 10000, 100000, Inf), 
  labels = c(
    "Sem receita", 
    "1–1.000", 
    "1.001–10.000", 
    "10.001–100.000", 
    "Mais de 100.000"),
  include.lowest = TRUE)

ggplot(candidatos_semNA2020, aes(x = FAIXA_RECEITA, fill = RACA_AGRUPADA)) +
  geom_bar(position = "dodge") +
  labs(x = "Receita",
       y = "Quantidade de candidatos",
       fill = "Raça") +
  theme_minimal(base_family = "Times", base_size = 12) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(colour = "grey85",linewidth = 0.3)) 




#divisão receita 2024

ggplot(tabela_comp_receita_2024, aes(x = FAIXA_RECEITA, y = prop / 100, fill = RACA_AGRUPADA)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_family = "Times", base_size = 12) +   
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotaciona os rótulos longos
  ) +
  labs(x = "Faixa de Receita", y = "Proporção", fill = "Raça")



candidatos_semNA2024$FAIXA_RECEITA <- cut(
  candidatos_semNA2024$RECEITA_TOTAL, 
  breaks = c(-1, 0, 1000, 10000, 100000, Inf), # -1 garante que o 0 vire "Sem renda"
  labels = c(
    "Sem renda", 
    "1–1.000", 
    "1.001–10.000", 
    "10.001–100.000", 
    "Mais de 100.000"),
  include.lowest = TRUE)


ggplot(candidatos_semNA2024, aes(x = FAIXA_RECEITA, fill = RACA_AGRUPADA)) +
  geom_bar(position = "dodge") +
  labs(x = "Receita",
       y = "Quantidade de candidatos",
       fill = "Raça") +
  theme_minimal(base_family = "Times", base_size = 12) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(colour = "grey85",linewidth = 0.3)) 



#Modelos  de regressão


#regressão multipla cand 
regressao_mul_cand <- modelsummary(
  list(
    "2016" = modelo_lm2016,
    "2020" = modelo_lm2020,
    "2024" = modelo_lm2024),
  statistic =  "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  coef_map = c(
    "(Intercept)" = "(Constante)",
    "log(RECEITA_TOTAL + 1)" = "Receita (log)",
    "log(VALOR_BENS + 1)" = "Patrimônio (log)",
    "RACA_AGRUPADANão Branco" = "Não branco",
    "DS_GENEROMASCULINO" = "Masculino",
    "ST_REELEICAOS" = "Reeleição",
    "DS_GRAU_INSTRUCAOLÊ E ESCREVE" = "Lê e escreve",
    "DS_GRAU_INSTRUCAOENSINO FUNDAMENTAL INCOMPLETO" = "Fundamental incompleto",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO COMPLETO" = "Médio completo",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO INCOMPLETO" = "Médio incompleto",
    "DS_GRAU_INSTRUCAOSUPERIOR COMPLETO" = "Superior completo",
    "DS_GRAU_INSTRUCAOSUPERIOR INCOMPLETO" = "Superior incompleto"),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"),
  notes = c(
    "Erros-padrão entre parênteses.",
    "Categoria de referência para escolaridade: Ensino fundamental completo."
  ), output = "flextable")

save_as_docx(regressao_mul_cand, path = "regressao_mul_cand.docx")

#Regressao multipla comp
regressao_mul_comp <- modelsummary(
  list(
    "2016" = modelo_lm_com2016,
    "2020" = modelo_lm_com2020,
    "2024" = modelo_lm_com2024),
  statistic =  "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  coef_map = c(
    "(Intercept)" = "(Constante)",
    "log(RECEITA_TOTAL + 1)" = "Receita (log)",
    "log(VALOR_BENS + 1)" = "Patrimônio (log)",
    "RACA_AGRUPADANão Branco" = "Não branco",
    "DS_GENEROMASCULINO" = "Masculino",
    "ST_REELEICAOS" = "Reeleição",
    "DS_GRAU_INSTRUCAOLÊ E ESCREVE" = "Lê e escreve",
    "DS_GRAU_INSTRUCAOENSINO FUNDAMENTAL INCOMPLETO" = "Fundamental incompleto",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO COMPLETO" = "Médio completo",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO INCOMPLETO" = "Médio incompleto",
    "DS_GRAU_INSTRUCAOSUPERIOR COMPLETO" = "Superior completo",
    "DS_GRAU_INSTRUCAOSUPERIOR INCOMPLETO" = "Superior incompleto"),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"),
  notes = c(
    "Erros-padrão entre parênteses.",
    "Categoria de referência para escolaridade: Ensino fundamental completo."
  ), output = "flextable")

save_as_docx(regressao_mul_comp, path = "regressao_mul_comp.docx")



#regressao logostica cand

regressao_logist_cand <- modelsummary(
  list(
    "2016" = modelo_logist2016,
    "2020" = modelo_logist2020,
    "2024" = modelo_logist2024),
  exponentiate = TRUE,
  statistic = "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  coef_map = c(
    "(Intercept)" = "Constante",
    "log(RECEITA_TOTAL + 1)" = "Receita (log)",
    "log(VALOR_BENS + 1)" = "Patrimônio (log)",
    "RACA_AGRUPADANão Branco" = "Não branco",
    "DS_GENEROMASCULINO" = "Masculino",
    "ST_REELEICAOS" = "Reeleição",
    "DS_GRAU_INSTRUCAOLÊ E ESCREVE" = "Lê e escreve",
    "DS_GRAU_INSTRUCAOENSINO FUNDAMENTAL INCOMPLETO" = "Fundamental incompleto",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO COMPLETO" = "Médio completo",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO INCOMPLETO" = "Médio incompleto",
    "DS_GRAU_INSTRUCAOSUPERIOR COMPLETO" = "Superior completo",
    "DS_GRAU_INSTRUCAOSUPERIOR INCOMPLETO" = "Superior incompleto"),
  gof_map = c("nobs", "aic", "bic"),
  notes = c(
    "Odds ratios reportados; erros-padrão entre parênteses.",
    "Categoria de referência para escolaridade: Ensino Fundamental Completo."), output = "flextable")


save_as_docx(regressao_logist_cand, path = "regressao_logist_cand.docx")



#regressão logistica comp

regressao_logist_comp <- modelsummary(
  list(
    "2016" = modelo_logist_com2016,
    "2020" = modelo_logist_com2020,
    "2024" = modelo_logist_com2024),
  exponentiate = TRUE,
  statistic = "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  coef_map = c(
    "(Intercept)" = "Constante",
    "log(RECEITA_TOTAL + 1)" = "Receita (log)",
    "log(VALOR_BENS + 1)" = "Patrimônio (log)",
    "RACA_AGRUPADANão Branco" = "Não branco",
    "DS_GENEROMASCULINO" = "Masculino",
    "ST_REELEICAOS" = "Reeleição",
    "DS_GRAU_INSTRUCAOLÊ E ESCREVE" = "Lê e escreve",
    "DS_GRAU_INSTRUCAOENSINO FUNDAMENTAL INCOMPLETO" = "Fundamental incompleto",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO COMPLETO" = "Médio completo",
    "DS_GRAU_INSTRUCAOENSINO MÉDIO INCOMPLETO" = "Médio incompleto",
    "DS_GRAU_INSTRUCAOSUPERIOR COMPLETO" = "Superior completo",
    "DS_GRAU_INSTRUCAOSUPERIOR INCOMPLETO" = "Superior incompleto"),
  gof_map = c("nobs", "aic", "bic"),
  notes = c(
    "Odds ratios reportados; erros-padrão entre parênteses.",
    "Categoria de referência para escolaridade: Ensino Fundamental Completo."), output = "flextable")


save_as_docx(regressao_logist_comp, path = "regressao_logist_comp.docx")




#modelo com interação cand

drop1_cand2024 <- as.data.frame(drop1(modelo_logist_interacao2024, test = "Chisq"))
drop1_cand2020 <- as.data.frame(drop1(modelo_logist_interacao2020, test = "Chisq"))
drop1_cand2016 <- as.data.frame(drop1(modelo_logist_interacao2016, test = "Chisq"))

# Transformar o nome das variáveis em coluna
drop1_cand2024 <- tibble::rownames_to_column(drop1_cand2024, "Variavel")
drop1_cand2020 <- tibble::rownames_to_column(drop1_cand2020, "Variavel")
drop1_cand2016 <- tibble::rownames_to_column(drop1_cand2016, "Variavel")

# Adicionar ano
drop1_cand2024$Ano <- 2024
drop1_cand2020$Ano <- 2020
drop1_cand2016$Ano <- 2016

# Juntar tudo
drop1_junto_cand <- bind_rows(drop1_cand2024,drop1_cand2020,drop1_cand2016)

drop1_junto_cand <- drop1_junto_cand%>%
  rename("Qui quadrado" = ChiSq,
         "P-valor" = 'P-value',
         "GL" = df,
         "Variáveis" = Variavel) %>%
  mutate(
    `Qui quadrado` = round(`Qui quadrado`, 3),
    `P-valor` = ifelse(
      `P-valor` < 0.001,
      format(`P-valor`, scientific = TRUE, digits = 2),
      sprintf("%.4f", `P-valor`))) %>%
  mutate(
    Variáveis = case_when(
      Variáveis == "log(RECEITA_TOTAL + 1)" ~ "Log(Receita +1)",
      Variáveis == "log(VALOR_BENS + 1)" ~ "Log(Patrimônio +1)",
      Variáveis == "RACA_AGRUPADA" ~ "Raça",
      Variáveis == "DS_GENERO" ~ "Gênero",
      Variáveis == "ST_REELEICAO" ~ "Reeleição",
      Variáveis == "DS_GRAU_INSTRUCAO" ~ "Escolaridade",
      Variáveis == "log(RECEITA_TOTAL + 1):RACA_AGRUPADA" ~ "Interação Log(Receita +1) e Raça",
      TRUE ~ Variáveis
    )
  )

drop1_junto_cand <- drop1_junto_cand %>%
  pivot_wider(
    names_from = Ano,
    values_from = c(`Qui quadrado`, `P-valor`))


# Criar tabela
tabela_drop1_cand <- flextable(drop1_junto_cand) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(
    `Qui quadrado_2016` = "χ² (2016)",
    `Qui quadrado_2020` = "χ² (2020)",
    `Qui quadrado_2024` = "χ² (2024)",
    `P-valor_2016` = "P-valor (2016)",
    `P-valor_2020` = "P-valor (2020)",
    `P-valor_2024` = "P-valor (2024)"
  ) %>%
  add_footer_lines(c(
    "GL = Graus de Liberdade",
    "$\chi^2$ = Estatística Qui-Quadrado do teste de razão de verossimilhança" )) %>%
  font(fontname = "Times New Roman",part = "all") 


save_as_image(tabela_drop1_cand, path = "tabela_drop1_cand.png")




#modelo com interação comp

drop1_comp2024 <- as.data.frame(drop1(modelo_logist_com_interacao2024, test = "Chisq"))
drop1_comp2020 <- as.data.frame(drop1(modelo_logist_com_interacao2020, test = "Chisq"))
drop1_comp2016 <- as.data.frame(drop1(modelo_logist_com_interacao2016, test = "Chisq"))

# Transformar o nome das variáveis em coluna
drop1_comp2024 <- tibble::rownames_to_column(drop1_comp2024, "Variavel")
drop1_comp2020 <- tibble::rownames_to_column(drop1_comp2020, "Variavel")
drop1_comp2016 <- tibble::rownames_to_column(drop1_comp2016, "Variavel")

# Adicionar ano
drop1_comp2024$Ano <- 2024
drop1_comp2020$Ano <- 2020
drop1_comp2016$Ano <- 2016

# Juntar tudo
drop1_junto_comp <- bind_rows(drop1_comp2024,drop1_comp2020,drop1_comp2016)

drop1_junto_comp <- drop1_junto_comp%>%
  rename("Qui quadrado" = ChiSq,
         "P-valor" = 'P-value',
         "GL" = df,
         "Variáveis" = Variavel) %>%
  mutate(
    `Qui quadrado` = round(`Qui quadrado`, 3),
    `P-valor` = ifelse(
      `P-valor` < 0.001,
      format(`P-valor`, scientific = TRUE, digits = 2),
      sprintf("%.4f", `P-valor`))) %>%
  mutate(
    Variáveis = case_when(
      Variáveis == "log(RECEITA_TOTAL + 1)" ~ "Log(Receita +1)",
      Variáveis == "log(VALOR_BENS + 1)" ~ "Log(Patrimônio +1)",
      Variáveis == "RACA_AGRUPADA" ~ "Raça",
      Variáveis == "DS_GENERO" ~ "Gênero",
      Variáveis == "ST_REELEICAO" ~ "Reeleição",
      Variáveis == "DS_GRAU_INSTRUCAO" ~ "Escolaridade",
      Variáveis == "log(RECEITA_TOTAL + 1):RACA_AGRUPADA" ~ "Interação Log(Receita +1) e Raça",
      TRUE ~ Variáveis
    )
  )

drop1_junto_comp <- drop1_junto_comp %>%
  pivot_wider(
    names_from = Ano,
    values_from = c(`Qui quadrado`, `P-valor`))


# Criar tabela
tabela_drop1_comp <- flextable(drop1_junto_comp) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(
    `Qui quadrado_2016` = "χ² (2016)",
    `Qui quadrado_2020` = "χ² (2020)",
    `Qui quadrado_2024` = "χ² (2024)",
    `P-valor_2016` = "P-valor (2016)",
    `P-valor_2020` = "P-valor (2020)",
    `P-valor_2024` = "P-valor (2024)"
  ) %>%
  add_footer_lines(c(
    "GL = Graus de Liberdade",
    "$\chi^2$ = Estatística Qui-Quadrado do teste de razão de verossimilhança" )) %>%
  font(fontname = "Times New Roman",part = "all") 

save_as_image(tabela_drop1_comp, path = "tabela_drop1_comp.png")



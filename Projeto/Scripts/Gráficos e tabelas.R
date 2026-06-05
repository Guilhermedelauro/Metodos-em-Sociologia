# 1. Bibliotecas
install.packages("ggplot2")

library(ggplot2)
library(tidyverse)
library (flextable)

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

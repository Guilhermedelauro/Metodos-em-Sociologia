# 1. Bibliotecas
install.packages("ggplot2")

library(ggplot2)

# 2. Bancos de dados 

source("C:\\Users\\Lauro\\Documents\\Metodos-em-Sociologia\\Projeto\\Scripts\\Eleição_2016.R")
source("C:\\Users\\Lauro\\Documents\\Metodos-em-Sociologia\\Projeto\\Scripts\\Eleição_2020.R")
source("C:\\Users\\Lauro\\Documents\\Metodos-em-Sociologia\\Projeto\\Scripts\\Eleição_2024.R")

#3. Gráficos e Tabelas 

#gráfico de raça por ano
tabela_raca2016 <- as.data.frame(tabela_raca2016)
tabela_raca2016$Ano <- 2016

tabela_raca2020 <- as.data.frame(tabela_raca2020)
tabela_raca2020$Ano <- 2020

tabela_raca2024 <- as.data.frame(tabela_raca2024)
tabela_raca2024$Ano <- 2024

tabela_raca_junto <- rbind(tabela_raca2016, tabela_raca2020, tabela_raca2024)
tabela_raca_junto <- factor(tabela_raca_junto$Ano)

barra_raca <- ggplot(tabela_raca_junto, aes(x=Freq, y=Var1, fill= Ano)) + 
  geom_col(position ="dodge") + 
  theme_minimal() + 
  labs(x= "Quantidade de candidatos", y= "Raça") 
  

print(barra_raca)

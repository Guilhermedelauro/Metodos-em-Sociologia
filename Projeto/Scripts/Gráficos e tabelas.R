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


windowsFonts(Times = windowsFont("Times New Roman"))

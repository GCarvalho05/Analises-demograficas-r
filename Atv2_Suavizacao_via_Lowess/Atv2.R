setwd("C:/Users/gcarv/Documents/Atv2_Topicos_em_Demografia")

library(readxl) #ler arquivo do excel

dados <- read_excel("TEA_Idade_POP013.xlsx")

library(dplyr)
library(tidyr)
library(ggplot2)
#Bibliotecas para fazer gráficos

grafico_sem_suavizacao <- ggplot(dados, aes(x = Idade, y = Taxa)) +
  # Linha levemente mais espessa e com uma cor mais profissional
  geom_line(color = "red", linewidth = 1) +                
  
  # Textos
  labs(title = "Taxa sem Suavização",
       x = "Idade (anos)",
       y = "Taxa") +
  
  # Exatamente o mesmo tema aplicado no gráfico da direita
  theme_minimal(base_size = 14) +                              
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    panel.grid.minor = element_blank()
  )

# Visualizar o Gráfico
print(grafico_sem_suavizacao)             

#Com Suavização:

grafico_com_suavizacao <- ggplot(dados, aes(x = Idade, y = Taxa)) +
  
  # Nível 3: Alta suavização (Plotada primeiro para ficar ao fundo)
  geom_smooth(method = "loess", span = 0.8, se = FALSE, linewidth = 1.2, alpha = 0.8, aes(color = "Alta (0.8)")) +
  
  # Nível 2: Suavização média
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.2, alpha = 0.8, aes(color = "Média (0.3)")) +
  
  # Nível 1: Pouca suavização (Plotada por último pois tem mais detalhes)
  geom_smooth(method = "loess", span = 0.1, se = FALSE, linewidth = 1.2, alpha = 0.8, aes(color = "Baixa (0.1)")) +
  
  # Paleta de cores moderna e amigável
  scale_color_manual(name = "Nível de Suavização",
                     values = c("Baixa (0.1)" = "#2A9D8F",   
                                "Média (0.3)" = "#264653",   
                                "Alta (0.8)"  = "#E76F51")) + 
  
  # Textos mais descritivos
  labs(title = "Taxa com Suavização ",
       x = "Idade (anos)",
       y = "Taxa") +
  
  # Limpeza geral do tema
  theme_minimal(base_size = 14) + # Aumenta a fonte base para melhor leitura
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centraliza e destaca o título
    legend.position = "bottom", # Move a legenda para baixo
    panel.grid.minor = element_blank() # Remove linhas de grade menores (poluição visual)
  )

# Visualizar o Gráfico
print(grafico_com_suavizacao)             

#Comparando:

library(patchwork) #Junta os dois Gráficos de forma SIMPLES

comparacao <- (grafico_sem_suavizacao + grafico_com_suavizacao) +
  plot_annotation(
    title = "Comparação: Taxas Brutas vs. Taxas Suavizadas",
    subtitle = "Análise da distribuição por idade",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  )

print(comparacao)

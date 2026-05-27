# 1. Carregar os pacotes
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)

# 2. Definir o diretório de trabalho
setwd("C:/Users/gcarv/Documents/Atv1_Topicos_em_Demografia")

# 3. Importar a tabela
dados <- read_excel("tabela_interpolada.xlsx")

# 4. Tratamento dos dados (apenas transformar para o formato longo)
dados_limpos <- dados %>%
  pivot_longer(
    cols = c("Pop. estimada Karup-King", "Pop. estimada Beers Modified"),
    names_to = "Metodo",
    values_to = "Populacao"
  )

# 5. Criar o Gráfico
ggplot(dados_limpos, aes(x = Idade, y = Populacao, color = Metodo, group = Metodo)) +
  geom_line(size =1) + # Mantemos apenas as linhas
  
  # Formatar o eixo Y para números com separador de milhar
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  
  # EIXO X: Criar quebras de 10 em 10 anos
  scale_x_continuous(breaks = seq(0, max(dados_limpos$Idade, na.rm = TRUE), by = 10)) +
  
  # PERSONALIZAÇÃO DAS CORES:
  scale_color_manual(
    values = c(
      "Pop. estimada Beers Modified" = "green", 
      "Pop. estimada Karup-King" = "purple"
    )
  ) +
  
  labs(
    title = "Comparação de Métodos de Interpolação: Karup-King vs Beers",
    subtitle = "População Residente por Idade Simples - Brasil 2022",
    x = "Idade (Anos)",
    y = "População Estimada",
    color = "Método"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 6. Calcular a diferença entre os métodos (Karup-King menos Beers)
dados_diferenca <- dados %>%
  mutate(Diferenca = `Pop. estimada Karup-King` - `Pop. estimada Beers Modified`)

# 7. Criar o Gráfico de Diferenças
ggplot(dados_diferenca, aes(x = Idade, y = Diferenca)) +
  
  # geom_col cria um gráfico de barras. 
  # O preenchimento (fill) muda de cor se a diferença for maior que zero
  geom_col(aes(fill = Diferenca > 0), show.legend = FALSE) + 
  
  # Adiciona uma linha horizontal marcando o zero (onde os métodos são idênticos)
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  
  # Formatação do eixo Y para separar milhares
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  
  # Eixo X mantendo o pulo de 10 em 10 anos
  scale_x_continuous(breaks = seq(0, max(dados_diferenca$Idade, na.rm = TRUE), by = 10)) +
  
  # Aplicando as suas cores:
  # TRUE (Karup-King > Beers) = purple
  # FALSE (Beers > Karup-King) = green
  scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "green")) +
  
  labs(
    title = "Diferença Absoluta: Karup-King vs Beers Modified",
    subtitle = "Barras roxas: Idade que Karup-King estimou mais. Barras verdes: Idade que Beers estimou mais.",
    x = "Idade (Anos)",
    y = "Diferença (Número de Pessoas)"
  ) +
  theme_minimal()

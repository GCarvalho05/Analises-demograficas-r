### Bibliotecas ###

# Carregando os pacotes necessários para a análise
library(censobr)  # Para baixar os microdados do IBGE
library(sidrar)   # Para baixar os dados do Sidra (caso use para 2022)
library(dplyr)    # Para manipulação, limpeza e agrupamento dos dados
library(ggplot2)  # Para a criação dos gráficos (pirâmides)
library(tidyr)    # Para organização e estruturação de dados
library(patchwork) # Para juntar e organizar vários gráficos lado a lado ou em uma grade

#####################

#=====================
#   Censo de 1991
#=====================

### Dicionario de 1991 ###
# Esta linha serve para você consultar os códigos das variáveis. 
# Pode deixar comentada no dia a dia para não abrir a janela toda vez que rodar o script.
# dicionario_1991 <- censobr::data_dictionary(year = 1991, dataset = "population")
# View(dicionario_1991)

### Lendo os Dados ###
# Baixando a base da amostra de 1991. 
# Não usamos o 'add_labels' aqui porque o pacote não suporta essa função para 1991.
pop_1991 <- read_population(
  year = 1991, 
  columns = c('V7301', 'V0301', 'V3072') # V7301 = Peso, V0301 = Sexo, V3072 = Idade
)

pop_1991 <- pop_1991 |>
  mutate(
    # 1. Transformando a coluna numérica de sexo em texto (categórica).
    # Isso é fundamental para o ggplot entender que são duas categorias de cores diferentes.
    V0301 = dplyr::case_when(
      V0301 == 1 ~ "Masculino",
      V0301 == 2 ~ "Feminino"
    ),
    
    # 2. Criando as faixas etárias quinquenais.
    # O código lê a idade exata e a classifica dentro da string correspondente.
    age_group = dplyr::case_when(
      V3072 <= 04              ~ "00-05",
      V3072 >= 05 & V3072 < 10 ~ "05-10",
      V3072 >= 10 & V3072 < 15 ~ "10-15",
      V3072 >= 15 & V3072 < 20 ~ "15-20",
      V3072 >= 20 & V3072 < 25 ~ "20-25",
      V3072 >= 25 & V3072 < 30 ~ "25-30",
      V3072 >= 30 & V3072 < 35 ~ "30-35",
      V3072 >= 35 & V3072 < 40 ~ "35-40",
      V3072 >= 40 & V3072 < 45 ~ "40-45",
      V3072 >= 45 & V3072 < 50 ~ "45-50",
      V3072 >= 50 & V3072 < 55 ~ "50-55",
      V3072 >= 55 & V3072 < 60 ~ "55-60",
      V3072 >= 60 & V3072 < 65 ~ "60-65",
      V3072 >= 65 & V3072 < 70 ~ "65-70",
      V3072 >= 70              ~ "70+"
    )
  )

# Calcula a tabela de contagem de pessoas por idade
piramide_df_1991 <- pop_1991 |>
  group_by(V0301, age_group) |>
  # Usamos sum() no peso amostral (V7301) para obter a população real, 
  # e não apenas contar o número de pessoas que responderam ao questionário.
  summarise(pop_count = sum(V7301)) |>
  collect()

# Verifica as primeiras linhas do dataframe resultante para conferência
head(piramide_df_1991)

# Remove qualquer grupo em que a idade não foi informada (valores NA)
piramide_df_1991 <- filter(piramide_df_1991, !is.na(age_group))

# Transforma a contagem de homens para valores negativos.
# O if_else inverte o eixo matemático para criar aquele formato espelhado da pirâmide.
piramide_df_1991 <- piramide_df_1991 |>
  mutate(pop_count = if_else(V0301 == "Masculino", pop_count, -pop_count))

# Gerando a figura da Pirâmide de 1991
piramide_1991 <- ggplot(data = piramide_df_1991,
                        aes(x = pop_count / 1000, # Divide por mil para o eixo não ficar com números muito extensos
                            y = age_group,
                            fill = V0301)) +
  geom_col() + # Função que cria as barras horizontais
  labs(title = 'Censo de 1991')+
  scale_fill_discrete(name="", type=c("#ffcb69","#437297")) + # Define as cores exatas do seu projeto
  scale_x_continuous(labels = function(x){scales::comma(abs(x))}, # A função abs() esconde o sinal negativo do eixo X no gráfico final
                     breaks = c(-8000, -4000,0,4000, 8000),
                     name = "População (em milhares)") +
  theme_classic() + # Aplica um tema de fundo limpo, sem fundo cinza
  theme(
    legend.position = "top", # Move a legenda das cores para a parte superior
    axis.title.y=element_blank(), # Remove o texto indicativo do eixo Y (limpa o layout)
    panel.grid.major.x = element_line(color = "grey90"), # Adiciona linhas verticais sutis para facilitar a leitura dos valores
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Imprime o gráfico de 1991 no painel de visualização
print(piramide_df_1991)

#=====================
#   Censo de 2010
#=====================

# Dicionário de 2010 (também deixado comentado para não atrapalhar o fluxo)
# dicionario_2010 <- censobr::data_dictionary(year = 2010, dataset = "population")
# View(dicionario_2010)

# Lendo os dados de 2010. 
# Como para 2010 o 'add_labels = pt' funciona, a variável de sexo (V0601) já vem traduzida.
pop_2010 <- read_population(
  year = 2010,
  columns = c('V0010', 'V0601', 'V6036'), # V0010 = Peso, V0601 = Sexo, V6036 = Idade
  add_labels = 'pt'
)

# Criando as faixas etárias usando a variável de idade de 2010 (V6036)
pop_2010 <- pop_2010 |>
  mutate(
    age_group = dplyr::case_when(
      V6036 <= 04              ~ "00-05",
      V6036 >= 05 & V6036 < 10 ~ "05-10",
      V6036 >= 10 & V6036 < 15 ~ "10-15",
      V6036 >= 15 & V6036 < 20 ~ "15-20",
      V6036 >= 20 & V6036 < 25 ~ "20-25",
      V6036 >= 25 & V6036 < 30 ~ "25-30",
      V6036 >= 30 & V6036 < 35 ~ "30-35",
      V6036 >= 35 & V6036 < 40 ~ "35-40",
      V6036 >= 40 & V6036 < 45 ~ "40-45",
      V6036 >= 45 & V6036 < 50 ~ "45-50",
      V6036 >= 50 & V6036 < 55 ~ "50-55",
      V6036 >= 55 & V6036 < 60 ~ "55-60",
      V6036 >= 60 & V6036 < 65 ~ "60-65",
      V6036 >= 65 & V6036 < 70 ~ "65-70",
      V6036 >= 70              ~ "70+"
    )
  )

# Calcula a população de 2010 somando o peso amostral (V0010)
piramide_df_2010 <- pop_2010 |>
  group_by(V0601, age_group) |>
  summarise(pop_count = sum(V0010)) |>
  collect()

# Remove grupo com idade missing `NA`
piramide_df_2010 <- filter(piramide_df_2010, !is.na(age_group))

# Espelha o gráfico passando os valores masculinos para negativo
piramide_df_2010 <- piramide_df_2010 |>
  mutate(pop_count = if_else(V0601 == "Masculino", pop_count, -pop_count))

# Gerando a figura da Pirâmide de 2010
piramide_2010 <- ggplot(data = piramide_df_2010,
                        aes(x = pop_count / 1000,
                            y = age_group,
                            fill = V0601)) +
  geom_col() +
  labs(title = "Censo de 2010") + 
  scale_fill_discrete(name="", type=c("red","darkblue")) + # Usa um padrão de cores diferente do de 1991
  scale_x_continuous(labels = function(x){scales::comma(abs(x))},
                     breaks = c(-8000, -4000,0,4000, 8000),
                     name = "População (em milhares)") +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y=element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Imprime o gráfico de 2010 no painel de visualização
print(piramide_2010)

#=====================
#   Censo de 2022
#=====================

# Importa os dados da Tabela 9514 diretamente da API do SIDRA/IBGE
pop_2022 <- get_sidra(api = "/t/9514/n1/all/v/all/p/all/c2/4,5/c287/all")

# Trata e limpa o banco de dados para a estrutura da pirâmide
pop_2022 <- pop_2022 |>
  # Mantém apenas as linhas com as faixas etárias quinquenais ou o grupo aberto final
  filter(grepl(" a | ou mais", Idade)) |>
  
  # Cria e padroniza as variáveis necessárias para o gráfico
  mutate(
    # Converte os rótulos de sexo para o padrão de texto dos gráficos anteriores
    V022_SEXO = case_when(
      Sexo == "Homens" ~ "Masculino",
      Sexo == "Mulheres" ~ "Feminino",
      TRUE ~ as.character(Sexo)
    ),
    
    # Classifica as idades textuais do SIDRA nos grupos quinquenais adotados
    age_group = case_when(
      Idade == "0 a 4 anos"   ~ "00-05",
      Idade == "5 a 9 anos"   ~ "05-10",
      Idade == "10 a 14 anos" ~ "10-15",
      Idade == "15 a 19 anos" ~ "15-20",
      Idade == "20 a 24 anos" ~ "20-25",
      Idade == "25 a 29 anos" ~ "25-30",
      Idade == "30 a 34 anos" ~ "30-35",
      Idade == "35 a 39 anos" ~ "35-40",
      Idade == "40 a 44 anos" ~ "40-45",
      Idade == "45 a 49 anos" ~ "45-50",
      Idade == "50 a 54 anos" ~ "50-55",
      Idade == "55 a 59 anos" ~ "55-60",
      Idade == "60 a 64 anos" ~ "60-65",
      Idade == "65 a 69 anos" ~ "65-70",
      TRUE                    ~ "70+"  # Agrupa todas as faixas de idade superiores a 70 anos
    )
  )

# Agrega a população somando os valores por sexo e faixa etária
piramide_df_2022 <- pop_2022 |>
  group_by(V022_SEXO, age_group) |>
  summarise(pop_count = sum(Valor, na.rm = TRUE), .groups = "drop")

# Filtra o banco para garantir que não existam registros sem idade definida (NAs)
piramide_df_2022 <- filter(piramide_df_2022, !is.na(age_group))

# Inverte os valores da população masculina para criar o espelhamento horizontal do gráfico
piramide_df_2022 <- piramide_df_2022 |>
  mutate(pop_count = if_else(V022_SEXO == "Masculino", pop_count, -pop_count))

# Monta a estrutura visual da pirâmide usando o ggplot2
piramide_2022 <- ggplot(data = piramide_df_2022,
                        aes(x = pop_count / 1000, # Reduz a escala populacional para milhares
                            y = age_group,        # Posiciona as faixas de idade no eixo vertical
                            fill = V022_SEXO)) +  # Define as cores de preenchimento por sexo
  geom_col() + # Plota as colunas horizontais da pirâmide
  labs(title = "Censo de 2022") +
  scale_fill_discrete(name="", type=c("#2ca02c","#9467bd")) + # Escolhe a paleta de cores customizada
  scale_x_continuous(labels = function(x){scales::comma(abs(x))}, # Remove o sinal negativo na exibição do eixo X
                     breaks = c(-8000, -4000, 0, 4000, 8000),     # Define as marcações de escala do eixo X
                     name = "População (em milhares)") +
  theme_classic() + # Aplica um layout minimalista com fundo branco
  theme(
    legend.position = "top", # Posiciona a legenda dos sexos no topo da figura
    axis.title.y=element_blank(), # Suprime o título do eixo Y para limpar o visual
    panel.grid.major.x = element_line(color = "grey90"),# Insere linhas verticais sutis de grade
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Exibe o gráfico gerado na tela do RStudio
print(piramide_2022)

#=====================
#   Resultado
#=====================

piramide_1991 + piramide_2010 + piramide_2022

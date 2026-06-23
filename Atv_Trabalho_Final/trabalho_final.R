# =====================================================================
# Título: Análise Comparativa de Crises Financeiras (1929 vs 2008)
# Descrição: Estudo focado em extração de métricas de risco (VaR, CVaR), 
#            testes estatísticos de distribuição e visualizações usando S&P 500.
# =====================================================================

# ---------------------------------------------------------------------
# 1. SETUP E CARREGAMENTO DE PACOTES
# ---------------------------------------------------------------------
# Descomente a linha abaixo para instalar os pacotes, caso necessário:
# install.packages(c("quantmod", "PerformanceAnalytics", "tseries", "car", "ggplot2"))

library(quantmod)             # Importação de dados do Yahoo Finance
library(PerformanceAnalytics) # Cálculos de métricas de risco (VaR, Drawdown)
library(tseries)              # Testes para séries temporais (Jarque-Bera)
library(car)                  # Testes de homogeneidade de variância (Levene)
library(ggplot2)              # Criação de gráficos avançados

# Definição de um tema global (DRY) para padronizar todos os gráficos ggplot2
tema_profissional <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#1B365D", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#555555", size = 14),
    axis.title = element_text(face = "bold", color = "#333333"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0", linetype = "dashed"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )

# ---------------------------------------------------------------------
# 2. COLETA E PREPARAÇÃO DOS DADOS
# ---------------------------------------------------------------------
# Extração do índice S&P 500 cobrindo ambas as crises
getSymbols("^GSPC", from = "1927-12-30", to = "2012-12-31")

# Cálculo dos log-retornos diários com base no preço ajustado (remoção do primeiro NA)
retornos <- Return.calculate(Ad(GSPC), method = "log")[-1]

# Segmentação das janelas temporais de cada crise
crise_29 <- retornos["1928-01-01/1933-12-31"]
crise_08 <- retornos["2006-01-01/2011-12-31"]

# Consolidando dados em um data.frame estruturado para ggplot2 e testes unificados
dados_crises <- data.frame(
  retorno = c(coredata(crise_29), coredata(crise_08)),
  crise = factor(c(rep("1929", length(crise_29)), rep("2008", length(crise_08))))
)

# ---------------------------------------------------------------------
# 3. MÉTRICAS DE RISCO E TESTES ESTATÍSTICOS
# ---------------------------------------------------------------------

# Value at Risk (VaR) e Expected Shortfall (CVaR) a 95% de confiança histórica
var_29  <- VaR(crise_29, p = 0.95, method = "historical")
var_08  <- VaR(crise_08, p = 0.95, method = "historical")
cvar_29 <- ES(crise_29, p = 0.95, method = "historical")
cvar_08 <- ES(crise_08, p = 0.95, method = "historical")

# Teste de Normalidade de Jarque-Bera (Avalia assimetria e curtose/caudas pesadas)
jarque.bera.test(coredata(crise_29))
jarque.bera.test(coredata(crise_08))

# Teste não-paramétrico de Wilcoxon-Mann-Whitney (Comparação de tendências centrais)
wilcox.test(coredata(crise_29), coredata(crise_08))

# Teste de Kolmogorov-Smirnov (Comparação das distribuições cumulativas)
ks.test(coredata(crise_29), coredata(crise_08))

# Teste de Levene (Avaliação de homogeneidade da variância/volatilidade)
leveneTest(retorno ~ crise, data = dados_crises)

# ---------------------------------------------------------------------
# 4. VISUALIZAÇÕES
# ---------------------------------------------------------------------

# 4.1 Gráficos de Drawdown (PerformanceAnalytics)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) # Divide o painel para facilitar comparação

chart.Drawdown(crise_29, 
               main = "Drawdown: Grande Depressão (1929)", 
               ylab = "Drawdown",
               colorset = "#C5A059", lwd = 2)

chart.Drawdown(crise_08, 
               main = "Drawdown: Crise do Subprime (2008)", 
               ylab = "Drawdown",
               colorset = "#1B365D", lwd = 2)
par(mfrow = c(1, 1)) # Retorna o painel ao padrão

# 4.2 Teste de Wilcoxon: Comparação de Medianas via Boxplot
grafico_wilcoxon <- ggplot(dados_crises, aes(x = crise, y = retorno, fill = crise)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#C5A059", outlier.size = 2.5, outlier.alpha = 0.7) +
  scale_fill_manual(values = c("1929" = "#C5A059", "2008" = "#1B365D")) +
  labs(title = "Comparação de Medianas (Wilcoxon)",
       subtitle = "Avaliação do impacto diário mediano entre as crises",
       x = "Período da Crise", y = "Log-Retornos Diários") +
  tema_profissional +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")

print(grafico_wilcoxon)

# 4.3 Teste Kolmogorov-Smirnov: Função de Distribuição Empírica Acumulada (ECDF)
grafico_ks <- ggplot(dados_crises, aes(x = retorno, color = crise)) +
  stat_ecdf(geom = "step", linewidth = 1.5) +
  scale_color_manual(values = c("1929" = "#C5A059", "2008" = "#1B365D")) +
  labs(title = "Função de Distribuição Acumulada (KS)",
       subtitle = "Diferenciação estrutural no acúmulo de risco",
       x = "Log-Retornos Diários", y = "Probabilidade Acumulada", color = "Crise:") +
  tema_profissional

print(grafico_ks)

# 4.4 Teste Jarque-Bera: Retornos de 2008 vs. Distribuição Normal
df_08 <- subset(dados_crises, crise == "2008")
media_08 <- mean(df_08$retorno)
desvio_08 <- sd(df_08$retorno)

grafico_jb <- ggplot(df_08, aes(x = retorno)) +
  geom_density(fill = "#1B365D", alpha = 0.7, color = NA) +
  stat_function(fun = dnorm, args = list(mean = media_08, sd = desvio_08),
                color = "#C5A059", linetype = "dashed", linewidth = 1.5) +
  labs(title = "Risco Empírico vs Distribuição Normal Teórica (2008)",
       subtitle = "Evidência visual de caudas pesadas e alta curtose",
       x = "Log-Retornos Diários", y = "Densidade") +
  annotate("text", x = 0.06, y = 18, label = "Risco de Cauda", 
           color = "#C5A059", fontface = "bold", size = 5) +
  tema_profissional

print(grafico_jb)

# 4.5 Teste de Levene: Sobreposição de Densidades para Comparar Volatilidade
grafico_levene <- ggplot(dados_crises, aes(x = retorno, fill = crise)) +
  geom_density(alpha = 0.6, color = NA) +
  scale_fill_manual(values = c("1929" = "#C5A059", "2008" = "#1B365D")) +
  labs(title = "Perfil de Volatilidade: 1929 vs 2008",
       subtitle = "Curvas mais achatadas representam maior dispersão (variância)",
       x = "Log-Retornos Diários", y = "Densidade", fill = "Crise:") +
  tema_profissional

print(grafico_levene)
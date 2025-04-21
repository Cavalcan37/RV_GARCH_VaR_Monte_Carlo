#######################
# 1ª PARTE: GARCH/VaR #
#######################
# ETF GLOBAL DE MERCADOS EMERGENTES (VWO)
#DIAGNÓSTICO DE VOLATILIDADE

# Passo 1: Instalação e carregamento dos pacotes 
install.packages("quantmod")
install.packages("rugarch")
install.packages("FinTS")
library(quantmod) # Funções 'getSymbols' e 'ROC'
library(rugarch) # Funções 'ugarchspec', 'ugarchfit' e 'sigma'
library(FinTS) # Função 'ArchTest'

# Ensaio Prévio - VWO
getSymbols("VWO", from = "2025-03-01", to = Sys.Date())
chartSeries(VWO, theme = "white", TA = "addVo(); addBBands()")

# Passo 2: Obtenção dos dados do VWO
vwo <- getSymbols("VWO", src = "yahoo", from = "2021-01-01", to = Sys.Date(), auto.assign = FALSE)
vwo <- na.locf(vwo)  # Valores faltantes preenchidos com o último valor válido
# Muinto comum em finanças                           
vwo

# Passo 3: Calcule os retornos do VWO
retornos_vwo <- na.omit(ROC(Cl(vwo), type = "discrete"))
retornos_vwo
View(retornos_vwo)

################################################
# Salva no Excel para Calculo do Retorno       #
################################################
library(openxlsx)                              #
write.xlsx(retornos_vwo, "retornos_vwo.xlsx")  #
################################################

# Renomeia a coluna
colnames(retornos_vwo) <- "Retornos_VWO"
retornos_vwo

################################################################################
# Passo 4: Testar Estacionariedade
# Carregar pacotes necessários
install.packages("tseries")
library(tseries)

# Teste ADF
adf_test <- adf.test(retornos_vwo)       # H0: Série Não Estacionária
print(adf_test)                          # H1: Série Estacionária

# Teste KPSS
kpss_test <- kpss.test(retornos_vwo)     # H0: Série Estacionária
print(kpss_test)                         # H0: Série Não Estacionária
################################################################################

# Passo 5: Análise exploratória
par(mfrow = c(2, 1))
plot(retornos_vwo, main = "Retornos Diários VWO", col = "blue4", lwd = 1)
hist(retornos_vwo, breaks = 25, main = "Distribuição dos Retornos VWO", 
     xlab = "Retornos", col = "lightgreen",probability = TRUE)

densidade <- density(retornos_vwo)  # Calcula a densidade kernel
lines(densidade, col = "red", lwd = 2)  # Adiciona a curva de densidade ao gráfico
par(mfrow = c(1, 1))

ArchTest(retornos_vwo, lags = 12)

# Passo 6: Especificação do modelo GARCH(1,1) para VWO
especificacao_vwo <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"  # Distribuição t de Student
)

# Passo 7: Estimação do modelo GARCH(1,1) para VWO
modelo_garch_vwo <- ugarchfit(spec = especificacao_vwo, data = retornos_vwo)
print(modelo_garch_vwo)

# Passo 8: Obtenha a volatilidade estimada para VWO
volatilidade_estimada_vwo <- sigma(modelo_garch_vwo)
volatilidade_estimada_vwo

# Passo 9: Plotagem dos retornos e volatilidades estimadas

# Bloco do VWO
par(mfrow = c(2, 1))  # Divide a área de plotagem em 2 linhas e 1 coluna

# Gráfico de cima: Retornos do VWO
plot(retornos_vwo, main = "Retornos Diários VWO", col = "blue4", lwd = 1)
legend("topright", 
       legend = c("Retornos VWO"), 
       col = c("blue4"), 
       lwd = 1, 
       cex = 0.8,  # Tamanho da legenda
       bty = "n")  # Remove a caixa ao redor da legenda

# Gráfico de baixo: Volatilidade estimada do VWO
plot(volatilidade_estimada_vwo, main = "Volatilidade Estimada VWO (GARCH(1,1))", col = "red3", lwd = 2)
legend("topright", 
       legend = c("Volatilidade VWO"), 
       col = c("red3"), 
       lwd = 2, 
       cex = 0.8,  # Tamanho da legenda
       bty = "n")  # Remove a caixa ao redor da legenda

par(mfrow = c(1, 1))

# Passo 10: Previsão de volatilidade para os próximos 30* dias
previsao_volatilidade <- ugarchforecast(modelo_garch_vwo, n.ahead = 30)
print(previsao_volatilidade)

# Passo 11: Plotagem da previsão de volatilidade
plot(previsao_volatilidade, which = 3, main = "Previsão de Volatilidade para os Próximos 30 Dias")

# VALUE-AT-RISK:
# Passo 12: Calcular o Value-at-Risk (VaR) para 1 dia

# Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Calcular o VaR para os retornos históricos
# Usando a volatilidade estimada pelo GARCH(1,1) e a distribuição t de Student
# O quantil da distribuição t de Student é obtido com qt()
df <- modelo_garch_vwo@fit$coef["shape"]  # Graus de liberdade da distribuição t
VaR_historico <- volatilidade_estimada_vwo * qt(alpha, df)

# Plotar o VaR histórico sobre os retornos
plot(retornos_vwo, main = "Retornos VWO com VaR (95%)", col = "blue4", lwd = 1)
lines(VaR_historico, col = "red", lwd = 2)
legend("topright", 
       legend = c("Retornos VWO", "VaR 95%"), 
       col = c("blue4", "red"), 
       lwd = c(1, 2), 
       cex = 0.8, 
       bty = "n")

# Passo 13: Calcular o Value-at-Risk (VaR) para 10 dias

# Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Calcular o VaR para 10 dias
# A volatilidade para 10 dias é a volatilidade diária multiplicada pela raiz quadrada de 10
volatilidade_10_dias <- volatilidade_estimada_vwo * sqrt(10)

# Calcular o VaR para 10 dias usando a distribuição t de Student
df <- modelo_garch_vwo@fit$coef["shape"]  # Graus de liberdade da distribuição t
VaR_10_dias <- volatilidade_10_dias * qt(alpha, df)

# Plotar o VaR para 10 dias sobre os retornos
plot(retornos_vwo, main = "Retornos do VWO com VaR (95%) para 10 Dias", col = "blue4", lwd = 1)
lines(VaR_10_dias, col = "red", lwd = 2)
legend("topright", 
       legend = c("Retornos VWO", "VaR 95% (10 Dias)"), 
       col = c("blue4", "red"), 
       lwd = c(1, 2), 
       cex = 0.8, 
       bty = "n")

# Value-at-Risk (VaR):
# Passo 14: Extrair a volatilidade estimada pelo modelo GARCH(1,1)
volatilidade_diaria <- as.numeric(tail(volatilidade_estimada_vwo, 1))  # Último valor da volatilidade diária

# Passo 15: Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Passo 16: Calcular o quantil da distribuição t de Student
df <- modelo_garch_vwo@fit$coef["shape"]  # Graus de liberdade da distribuição t
quantil <- qt(alpha, df)  # Quantil para o nível de confiança alpha

# Passo 17: Calcular o VaR para 1 dia
VaR_1_dia <- volatilidade_diaria * quantil

# Passo 18: Calcular o VaR para 10 dias
volatilidade_10_dias <- volatilidade_diaria * sqrt(10)
VaR_10_dias <- volatilidade_10_dias * quantil

# Passo 19: Exibir os resultados
cat("VaR para 1 dia (95% de confiança):", VaR_1_dia, "\n")
cat("VaR para 10 dias (95% de confiança):", VaR_10_dias, "\n")

# Interpretação:
# Para 1 dia: VaR = -0.08. Significa que há 95% de confiança de que a perda
# máxima para um 1 não exceda 8% (ou seja, temos um risco de 5% da perda 
# máxima em 1 dia ultrapassar os 8%).

# Para 10 dia: VaR = -0.25. Significa que há 95% de confiança de que a perda
# máxima para um 1 não exceda 25% (ou seja, temos um risco de 5% da perda 
# máxima em 10 dia ultrapassar os 25%).

# LINKEDIN######################################################################
# Plotando Retornos e VaR + GARCH(1,1) abaixo (p/Linkedin)
# Ensaio Prévio - VWO
getSymbols("VWO", from = "2025-03-01", to = Sys.Date())
chartSeries(VWO, theme = "white", TA = "addVo(); addBBands()")

par(mfrow = c(1, 1))
plot(retornos_vwo, main = "Retornos VWO com VaR (95%)", col = "blue4", lwd = 1)
graf <- lines(VaR_historico, col = "red", lwd = 2)

# Gráfico Retornos + VaR 
par(mfrow = c(2, 1))
graf

# Gráfico GARCH(1,1)
plot(volatilidade_estimada_vwo, main = "Volatilidade VWO (GARCH(1,1))", col = "red3", lwd = 2)

par(mfrow = c(1, 1)) 

####### Cálculo do Retorno Médio Geométrico #######
#Carregar pacotes necessários
library(quantmod)
library(PerformanceAnalytics)  # Para funções financeiras avançadas

# Baixar dados do VWO
getSymbols("VWO", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Calcular retornos diários (usando preços ajustados)
retornos_diarios <- dailyReturn(Ad(VWO), type = "log")  # Retornos logarítmicos

# Remover valores NA (se houver)
retornos_diarios <- na.omit(retornos_diarios)

# Calcular retorno geométrico anualizado
retorno_geometrico_anual <- Return.annualized(retornos_diarios, geometric = TRUE)
retorno_geometrico_anual
################################################################################
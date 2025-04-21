##########################
# 2ª PARTE: ARIMA/SARIMA #
##########################

# MODELO ARIMA/SARIMA
# Pacotes
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)
library(seastests)

# Passo 1: Baixar dados
getSymbols("VWO", src = "yahoo", from = "2023-01-01", to = Sys.Date())
serie <- Ad(VWO)
serie <- na.omit(serie)
serie

# Passo 2: Converter para série temporal e sua visualização
par(mfrow = c(1, 1))
serie_ts <- ts(serie, frequency = 5)
serie_ts
plot(serie_ts, main = "ETF Global VWO", xlab = "Dias", ylab = "Cotação (US$)")

# Passo 3: Detecção de Sazonalidade
# 1. Análise gráfica 
seasonplot(serie_ts, col = rainbow(12), year.labels = T, type = "o", pch = 16)
decomp.serie_ts<-decompose(serie_ts)
plot(decomp.serie_ts)

# 2. Teste QS de sazonalidade
qs_test <- qs(serie_ts, freq = 5) # H0: Não há Sazonalidade
qs_test                           # H1: Há Sazonalidade    

# 3. Função de Autocorrelação (ACF)
acf_plot <- acf(serie_ts, lag.max = 20, main = "ACF do ETF VWO", plot = TRUE)

# Passo 4: Modelagem
modelo <- auto.arima(serie_ts, seasonal = TRUE, stepwise = FALSE)

# Diagnóstico do Modelo
summary(modelo)
checkresiduals(modelo)

# Passo 5: Previsão
previsao <- forecast(modelo, h = 10)
autoplot(previsao, include = 30) +
  ggtitle(paste("Previsão VWO")) +
  xlab("Data") + ylab("Preço (R$)") +
  theme_gray()

# Visualização Alternativa
# Passo 6: Previsão para 10 dias com histórico reduzido
previsao <- forecast(modelo, h = 10)
previsao

# Definir o número de dias históricos a mostrar
n_dias_historicos <- min(30, length(serie_ts))

# Configurações gráficas avançadas
par(
  bg = "#f5f5f5",           # Cor de fundo cinza claro
  fg = "#333333",           # Cor dos eixos e texto
  col.main = "#2c3e50",     # Cor do título (azul escuro)
  col.lab = "#2c3e50",      # Cor dos rótulos
  font.main = 2,            # Título em negrito
  font.lab = 1,             # Rótulos em normal
  las = 1,                  # Texto dos eixos na horizontal
  mar = c(5, 4, 4, 2) + 0.1 # Margens do gráfico
)

# Plot principal
plot(previsao, 
     main = paste("Previsão VWO"),
     xlab = "Data", 
     ylab = "Preço (US$)", 
     flty = 1,               # Linha contínua para previsão
     lwd = 2,                # Espessura da linha
     fcol = "#e74c3c",       # Vermelho moderno para previsão
     col = "#3498db",        # Azul para intervalo
     shaded = TRUE,
     include = n_dias_historicos,
     axes = FALSE)           # Desativar eixos padrão

# Adicionar eixos personalizados
axis(1, col = "#bdc3c7", col.axis = "#2c3e50", cex.axis = 0.9)  # Eixo X
axis(2, col = "#bdc3c7", col.axis = "#2c3e50", cex.axis = 0.9)  # Eixo Y

# Adicionar histórico recente
lines(serie_recente, col = "#2c3e50", lwd = 2)

# Grid sutil
grid(col = "#d5dbdb", lty = 1, lwd = 0.5)

# Adicionar legenda
legend("topright", 
       legend = c("Histórico", "Previsão", "Intervalo 80%"),
       col = c("#2c3e50", "#e74c3c", "#3498db"),
       lwd = c(2, 2, 8),
       lty = c(1, 1, 1),
       bg = "white",
       cex = 0.65)

# Adicionar borda
box(col = "#bdc3c7")
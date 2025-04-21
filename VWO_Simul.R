###################################
# 3ª PARTE: SIMULAÇÃO MONTE CARLO #
###################################

# VALUATION VIA FCD (SOMENTE GANHOS / QUEDAS) #

# Valuation Técnico de Curto Prazo
# Swing Trade
# Avaliação de Arbitragem 
# Sinal de Entrada/Saída
# Filtro de Ativos
# Estudos de Eficiência de Mercado

# Pacotes
library(forecast)
library(ggplot2)
library(tseries)
library(seastests)
library(quantmod) # Funções 'getSymbols' e 'ROC'
library(rugarch) # Funções 'ugarchspec', 'ugarchfit' e 'sigma'
library(FinTS) # Função 'ArchTest'

# Preço atual da ação (último valor real da série)
preco_atual <- as.numeric(tail(serie, 1))
preco_atual

# Preços previstos extraídos do objeto de forecast
precos_previstos <- as.numeric(previsao$mean)
precos_previstos

# Taxa de desconto anual (exemplo: 10%)
taxa_anual <- 0.10

# Conversão da taxa anual para taxa diária (252 dias úteis por ano)
taxa_diaria <- (1 + taxa_anual)^(1 / 252) - 1
taxa_diaria

# Dias correspondentes às previsões
dias <- 1:length(precos_previstos)
dias

# Cálculo dos acréscimos (positivos ou negativos) em relação ao preço atual
ganhos_ou_perdas <- precos_previstos - preco_atual
ganhos_ou_perdas

# Valor presente dos ganhos/perdas esperados
vp_ganhos_ou_perdas <- ganhos_ou_perdas / ((1 + taxa_diaria) ^ dias)
vp_ganhos_ou_perdas

# Valor justo refinado: preço atual + valor presente líquido dos ganhos/perdas
valor_justo_refinado <- preco_atual + sum(vp_ganhos_ou_perdas)
valor_justo_refinado

# Diferença entre valor justo e preço atual
delta_valor <- valor_justo_refinado - preco_atual
delta_valor

# Resultados
cat("Preço atual da ação: US$", round(preco_atual, 2), "\n")
cat("Valor justo estimado (baseado na valorização esperada): US$", round(valor_justo_refinado, 2), "\n")
cat("Diferença: US$", round(delta_valor, 2), "\n")

if (delta_valor > 0) {
  cat("→ A ação pode estar SUBVALORIZADA no curto prazo.\n")
} else if (delta_valor < 0) {
  cat("→ A ação pode estar SOBREVALORIZADA no curto prazo.\n")
} else {
  cat("→ A ação está corretamente precificada de acordo com as expectativas.\n")
}

#####################################
# SIMULAÇÃO ESTATÍSTICA DE CENÁRIOS # versão 1
#####################################
set.seed(123)  # Para reprodutibilidade

# Preço atual
preco_atual <- as.numeric(tail(serie, 1))

# Previsões base
precos_previstos <- as.numeric(previsao$mean)
dias <- 1:length(precos_previstos)

# Parâmetros da distribuição normal
media <- mean(precos_previstos)
desvio <- sd(precos_previstos)
desvio <- 1.5

# Simulação: gerar 5000 vetores de previsões (um para cada cenário)
n_simulacoes <- 5000
simulacoes <- matrix(rnorm(n_simulacoes * length(precos_previstos), mean = media, sd = desvio),
                     nrow = n_simulacoes, ncol = length(precos_previstos))

# Taxa de desconto
taxa_anual <- 0.10
taxa_diaria <- (1 + taxa_anual)^(1 / 252) - 1

# Inicializar vetores para armazenar resultados
valores_justos <- numeric(n_simulacoes)
deltas_valor <- numeric(n_simulacoes)

# Loop pelas simulações
for (i in 1:n_simulacoes) {
  ganhos_ou_perdas <- simulacoes[i, ] - preco_atual
  vp_ganhos <- ganhos_ou_perdas / ((1 + taxa_diaria) ^ dias)
  valores_justos[i] <- preco_atual + sum(vp_ganhos)
  deltas_valor[i] <- valores_justos[i] - preco_atual
}

# Calcular as médias e desvios
media_vj <- mean(valores_justos)
dp_vj <- sd(valores_justos)

media_delta <- mean(deltas_valor)
dp_delta <- sd(deltas_valor)

# Probabilidades desejadas
prob_vj_4330 <- pnorm(43.30, mean = media_vj, sd = dp_vj)
prob_delta_0 <- pnorm(0, mean = media_delta, sd = dp_delta)

# Intervalos para densidade
x_vj <- seq(min(valores_justos), max(valores_justos), length.out = 1000)
dens_vj <- dnorm(x_vj, mean = media_vj, sd = dp_vj)

x_delta <- seq(min(deltas_valor), max(deltas_valor), length.out = 1000)
dens_delta <- dnorm(x_delta, mean = media_delta, sd = dp_delta)

# Layout com dois gráficos verticais
par(mfrow = c(2, 1), mar = c(4, 4, 2.5, 1))

# 1. Histograma do valor justo refinado ########################################
hist_vj <- hist(valores_justos, breaks = 20, col = "skyblue", border = "black",
                main = "Valor VWO",
                xlab = "VWO (US$)", freq = FALSE)
lines(x_vj, dens_vj, col = "red", lwd = 2)
abline(v = media_vj, col = "black", lty = 2, lwd = 2)

# Área sombreada para valores < 43.30
x_sombra <- x_vj[x_vj <= 43.30]
y_sombra <- dnorm(x_sombra, mean = media_vj, sd = dp_vj)
polygon(c(x_sombra, rev(x_sombra)), c(y_sombra, rep(0, length(y_sombra))),
        col = rgb(1, 0, 0, 0.5), border = NA)

# Texto da probabilidade no gráfico
text(x = 40, y = 0.04,
     labels = paste0("Valor VWO < 43,30: ",
                     round(prob_vj_4330 * 100, 2), "%"),
     col = "blue4", cex = 0.9, font = 2)

# Texto da média
text(x = media_vj + 1.5, y = max(dens_vj)*0.9,
     labels = paste0("Média = ", round(media_vj, 2)),
     col = "black", cex = 0.9, font = 2)

# 2. Histograma do delta valor #################################################
hist_delta <- hist(deltas_valor, breaks = 20, col = "moccasin", border = "black",
                   main = "Delta VWO",
                   xlab = "Delta VWO (US$)", freq = FALSE)
lines(x_delta, dens_delta, col = "red", lwd = 2)
abline(v = media_delta, col = "black", lty = 2, lwd = 2)

# Área sombreada para valores < 0
x_sombra2 <- x_delta[x_delta <= 0]
y_sombra2 <- dnorm(x_sombra2, mean = media_delta, sd = dp_delta)
polygon(c(x_sombra2, rev(x_sombra2)), c(y_sombra2, rep(0, length(y_sombra2))),
        col = rgb(1, 0, 0, 0.5), border = NA)

# Texto da probabilidade no gráfico
text(x = -5, y = 0.04,
     labels = paste0("Delta VWO < 0: ",
                     round(prob_delta_0 * 100, 2), "%"),
     col = "blue4", cex = 0.9, font = 2)

# Texto da média
text(x = media_delta + 1.5, y = max(dens_delta)*0.9,
     labels = paste0("Média = ", round(media_delta, 2)),
     col = "black", cex = 0.9, font = 2)



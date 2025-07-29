#-----------------------------------------------------------------------
# Análise de Série Temporal - Consumo de Energia no Ceará
#-----------------------------------------------------------------------

# 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES
#-----------------------------------------------------------------------

pacman::p_load(forecast, tseries, ggplot2, gridExtra, urca, lubridate, FinTS, zoo,tibble,readxl,datawirzad,tidyverse)


# 2. CARREGAMENTO E PREPARAÇÃO DOS DADOS
#-----------------------------------------------------------------------
# Carregar o arquivo CSV
file_path <- "C:/Users/thali/Desktop/Área de Trabalho/Serie temporal/Análise de Energia eletrica industrial do Ceará/CMEIC.xlsx"
dados_completos <- read_excel(file_path)

# Renomear colunas para facilitar o manuseio (assumindo a ordem das colunas)
colnames(dados_completos) <- c("data", "consumo_gwh")

# Filtrar dados para o estado do Ceará (CE)
ceara_data <- dados_completos %>%
  mutate(data = as.Date(data)) %>%
  arrange(data)

# 3. ANÁLISE DESCRITIVA
#-----------------------------------------------------------------------
# Resumo estatístico
summary(ceara_data$consumo_gwh)

# Gráfico da série temporal completa
ggplot(ceara_data, aes(x = data, y = consumo_gwh)) +
  geom_line(color = "#0072B2", size = 1) +
  labs(title = "Consumo Mensal de Energia Elétrica no Ceará (2004-2023)",
       subtitle = "Fonte: Empresa de Pesquisa Energética (EPE)",
       x = "Ano",
       y = "Consumo (GWh)") +
  theme_minimal()

# 4. CRIAÇÃO E ANÁLISE DO OBJETO DE SÉRIE TEMPORAL
#-----------------------------------------------------------------------
# Criar o objeto de série temporal (ts) com frequência 12 (mensal)
# A série começa em Janeiro de 2004
ts_ceara <- ts(ceara_data$consumo_gwh, start = c(2004, 1), frequency = 12)

# Decomposição da série temporal usando STL (Seasonal and Trend decomposition using Loess)
decomposicao <- stl(ts_ceara, s.window = "periodic")
plot(decomposicao, main = "Decomposição da Série Temporal de Consumo de Energia no CE")

# 5. VERIFICAÇÃO DE ESTACIONARIEDADE E DIFERENCIAÇÃO
#-----------------------------------------------------------------------
# Teste de Estacionariedade (Augmented Dickey-Fuller Test) na série original
# Hipótese nula (H0): a série não é estacionária
print("ADF Test na série original:")
adf.test(ts_ceara) # Espera-se p-valor > 0.05 (não estacionária)

# Aplicar diferenciação para tornar a série estacionária
# d=1 (regular) e D=1 (sazonal)
ts_ceara_diff <- diff(diff(ts_ceara, 12), 1)

# Testar a estacionariedade da série diferenciada
print("ADF Test na série diferenciada:")
adf.test(ts_ceara_diff) # Espera-se p-valor < 0.05 (estacionária)

# Plotar as funções de Autocorrelação (ACF) e Autocorrelação Parcial (PACF)
# para ajudar a identificar os parâmetros do modelo
ggtsdisplay(ts_ceara_diff, main = "Série Diferenciada (d=1, D=1)")

# 6. AJUSTE DO MODELO SARIMA
#-----------------------------------------------------------------------
# Usar auto.arima para encontrar o melhor modelo SARIMA automaticamente
# Ele testará diferentes parâmetros (p,d,q)(P,D,Q) e selecionará o melhor com base no AIC
modelo_sarima <- auto.arima(ts_ceara,
                            stepwise = TRUE,    # Busca mais rápida
                            trace = TRUE,       # Mostra os modelos testados
                            approximation = FALSE) # Busca mais precisa

# Exibir o resumo do melhor modelo encontrado
summary(modelo_sarima)

# 7. DIAGNÓSTICO DO MODELO
#-----------------------------------------------------------------------
# Verificar se os resíduos do modelo se comportam como ruído branco
# (sem padrões, média zero, variância constante, não correlacionados)
checkresiduals(modelo_sarima)

# 8. PREVISÃO
#-----------------------------------------------------------------------
# Fazer a previsão para os próximos 24 meses (2 anos)
previsao <- forecast(modelo_sarima, h = 24)

# Plotar a previsão
autoplot(previsao) +
  labs(title = "Previsão do Consumo de Energia no Ceará (Próximos 24 meses)",
       subtitle = "Modelo SARIMA(1,1,1)(2,1,0)[12]",
       x = "Ano",
       y = "Consumo (GWh)") +
  theme_minimal()

# Exibir os valores previstos em uma tabela
print("Valores Previstos (GWh):")
kable(previsao, caption = "Previsão de Consumo de Energia para os Próximos 24 Meses")
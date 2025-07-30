
pacman::p_load(forecast, tseries, ggplot2, patchwork,gridExtra, urca, lubridate, FinTS, zoo,tibble,readxl,datawirzad,tidyverse,timetk,tseries,TSA,tsoutliers,)



# 2. CARREGAMENTO E PREPARAÇÃO DOS DADOS
#-----------------------------------------------------------------------
# Carregar o arquivo CSV
file_path <- "C:/Users/thali/Desktop/Área de Trabalho/Serie temporal/Análise de Energia eletrica industrial do Ceará/Slide/Analise-da-Serie-EPE-CE/Bases/CMEIC.xlsx"
dados_completos <- read_excel(file_path)

# Renomear colunas para facilitar o manuseio (assumindo a ordem das colunas)
colnames(dados_completos) <- c("data", "consumo_gwh")

# Filtrar dados para o estado do Ceará (CE)
ceara_data <- dados_completos %>%
  mutate(data = as.Date(data)) %>%
  arrange(data)

# Gráfico da série temporal completa
ggplot(ceara_data, aes(x = data, y = consumo_gwh)) +
  geom_line(color = "#000000", size = 1) +
  labs(title = "Consumo Mensal de Energia Elétrica no Ceará (Sistema Simples) (2004-2025)",
       subtitle = "Fonte: Empresa de Pesquisa Energética (EPE)",
       x = "Ano",
       y = "Consumo (MWh)") +
  theme_minimal()


teste = ceara_data %>%  rename( date = data , value = consumo_gwh)


teste %>%  plot_seasonal_diagnostics(date, value, .interactive = FALSE) 


# A série começa em Janeiro de 2004
ts_ceara <- ts(ceara_data$consumo_gwh, start = c(2004, 1), frequency = 12)
# 2. Criar o conjunto de TREINO usando a função window()
treino <- window(ts_ceara, end = c(2022, 12))
# 3. Criar o conjunto de TESTE usando a função window()
teste <- window(ts_ceara, start = c(2023, 1), end = c(2025, 3))

# --- Função Aprimorada ---

#' Gera um painel de diagnóstico para uma série temporal.
#'
#' @param ts_object Um objeto de série temporal (classe ts).
#' @param titulo Um título geral para o painel de gráficos.
#' @return Um objeto de gráfico patchwork contendo os diagnósticos.

gerar_diagnostico_ts <- function(ts_object, titulo = "Painel de Diagnóstico da Série Temporal") {
  
  # 2. Gráfico da Função de Autocorrelação (ACF)
  p1 <- ggAcf(ts_object, lag.max = 36) +
    labs(title = "Autocorrelação (ACF)") +
    theme_light()
  
  # 3. Gráfico da Função de Autocorrelação Parcial (PACF)
  p2 <- ggPacf(ts_object, lag.max = 36) +
    labs(title = "Autocorrelação Parcial (PACF)") +
    theme_light()
  
  
  # 5. Organizar os gráficos usando patchwork
  layout <- (p1 | p2) 
  
  # Adicionar um título geral ao painel
  plot_final <- layout + plot_annotation(title = titulo,
                                         theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
  
  return(plot_final)
}



gerar_diagnostico2_ts <- function(ts_object, titulo = "Painel de Diagnóstico da Série Temporal") {
  
  # 2. Gráfico da Função de Autocorrelação (ACF)
  p1 <- ggAcf(ts_object, lag.max = 36) +
    labs(title = "Autocorrelação (ACF)") +
    theme_light()
  
  # 3. Gráfico da Função de Autocorrelação Parcial (PACF)
  p2 <- ggPacf(ts_object, lag.max = 36) +
    labs(title = "Autocorrelação Parcial (PACF)") +
    theme_light()
  
  p3 <- autoplot(ts_object) +   geom_line(color = "#000000", size = 1) +  labs(title = "Gráfico da Série",
                                                                               x = "Ano",
                                                                               y = "Consumo (MWh)") +
    theme_light()
  
  # 5. Organizar os gráficos usando patchwork
  layout <- p3 / (p1 | p2)
  
  # Adicionar um título geral ao painel
  plot_final <- layout + plot_annotation(title = titulo,
                                         theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
  
  return(plot_final)
}



modelo_ets <- ets(treino)
#summary(modelo_ets) 
#checkresiduals(modelo_ets)


gerar_diagnostico2_ts(modelo_ets$residuals) 

# Realizar a previsão para os próximos 27 meses 
previsao_ets <- forecast(modelo_ets, h = 27)

# Gráfico da previsão vs. dados de teste 
autoplot(previsao_ets) +
  autolayer(teste, series="Dados Reais") +
  labs(title='Gráfico 2: Previsão com ETS(M,Ad,M) vs. Dados de Teste', 
       x='Ano', y='Índice',
       caption = 'Linha azul: Previsão ETS. Sombreado: Intervalos de Confiança.') +
  theme_minimal()


# Obter as medidas de ajuste no conjunto de teste 
acuracia_ets <- accuracy(previsao_ets, teste)
print(acuracia_ets)


# A série começa em Janeiro de 2004
ts_ceara <- ts(ceara_data$consumo_gwh, start = c(2004, 1), frequency = 12)
# 2. Criar o conjunto de TREINO usando a função window()
treino <- window(ts_ceara, end = c(2022, 12))
# 3. Criar o conjunto de TESTE usando a função window()
teste <- window(ts_ceara, start = c(2023, 1), end = c(2025, 3))


# Decomposição da série temporal usando STL (Seasonal and Trend decomposition using Loess)
decomposicao <- stl(ts_ceara, s.window = "periodic")

autoplot(decomposicao, colour = "#0072B2") +
  
  # 1. Adicionar títulos, subtítulos e legendas mais descritivas
  labs(
    title = "Decomposição STL da Série Temporal",
    subtitle = "Componentes: Dados, Tendência, Sazonalidade e Resíduo",
    x = "Ano",
    y = ""  # Deixamos em branco para que os nomes dos painéis sirvam como rótulos
  ) +
  
  # 2. Personalizar os elementos do tema do gráfico
  theme_minimal() +
  theme(
    # Formatação do título principal
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "gray20"),
    
    # Formatação do subtítulo
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
    
    # Formatação do título do eixo X
    axis.title.x = element_text(size = 12, face = "italic"),
    
    # Formatação do texto dos painéis (Data, Trend, etc.)
    strip.text = element_text(size = 11, face = "bold", color = "gray10"),
    
    # Ajustar as linhas de grade para um visual mais limpo
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_blank(), # Remover linhas de grade menores
    
    # Adicionar um espaço entre os painéis para melhor separação
    panel.spacing = unit(1.5, "lines") 
  )

adf.test(treino) 
kpss.test(treino) 


gerar_diagnostico_ts(ts_ceara)


ts_diff= diff(ts_ceara)
gerar_diagnostico2_ts(ts_diff)
#adf.test(ts_diff)
#kpss.test(ts_diff)


Modelo1 <- Arima(treino, order = c(1, 1, 1), seasonal = c(1,0,0))

summary(Modelo1)

gerar_diagnostico2_ts(Modelo1$residuals) 

ts_diff_12 = diff(diff(ts_ceara),12)

gerar_diagnostico2_ts(ts_diff_12 )
#adf.test(ts_diff_12)
#kpss.test(ts_diff_12)

# Ajuste o modelo ARIMA

# Faça a previsão para os próximos 24 períodos (por exemplo, 2 anos)
previsao_modelo2 <- forecast(Modelo2,27)
# Gráfico da previsão vs. dados de teste 
autoplot(previsao_modelo2) +
  autolayer(teste, series="Dados Reais") +
  labs(title='Gráfico 2: Previsão com ETS(M,Ad,M) vs. Dados de Teste', 
       x='Ano', y='Índice',
       caption = 'Linha azul: Previsão ETS. Sombreado: Intervalos de Confiança.') +
  theme_minimal()

Modelo3 <- Arima(treino, order = c(1, 1, 1), seasonal = c(0,1,1))

summary(Modelo2)

gerar_diagnostico2_ts(Modelo3$residuals)


# Ajuste o modelo ARIMA

# Faça a previsão para os próximos 24 períodos (por exemplo, 2 anos)
previsao_modelo2 <- forecast(Modelo3,27)
# Gráfico da previsão vs. dados de teste 
autoplot(previsao_modelo2) +
  autolayer(teste, series="Dados Reais") +
  labs(title='Gráfico 2: Previsão com ETS(M,Ad,M) vs. Dados de Teste', 
       x='Ano', y='Índice',
       caption = 'Linha azul: Previsão ETS. Sombreado: Intervalos de Confiança.') +
  theme_minimal()

detectAO(Modelo3)
detectIO(Modelo3)

# Primeiro, pegamos o tamanho do seu conjunto de treino
n_treino <- length(treino)

# Criamos um vetor de zeros com o mesmo tamanho do treino
# Este será nosso regressor para o outlier
outlier_ao_196 <- rep(0, n_treino)

# Agora, colocamos o valor 1 na posição 196, que é o índice do outlier
outlier_ao_196[196] <- 1

Modelo3_com_outlier <- Arima(treino, 
                             order = c(1, 1, 1), 
                             seasonal = list(order = c(0, 1, 1), period = 12),
                             xreg = outlier_ao_196) 

# Veja o resumo do novo modelo
summary(Modelo3_com_outlier)


# Ajuste o modelo ARIMA
# Defina o horizonte de previsão (ex: 24 meses)
h_previsao <- 27

# Crie o xreg para o período futuro. Como o outlier não vai se repetir,
# ele será um vetor de zeros.
xreg_futuro <- rep(0, h_previsao)
# Faça a previsão para os próximos 24 períodos (por exemplo, 2 anos)
previsao_modelo3 <- forecast(Modelo3_com_outlier,27,xreg = xreg_futuro)
# Gráfico da previsão vs. dados de teste 
autoplot(previsao_modelo2) +
  autolayer(teste, series="Dados Reais") +
  labs(title='Gráfico 2: Previsão com ETS(M,Ad,M) vs. Dados de Teste', 
       x='Ano', y='Índice',
       caption = 'Linha azul: Previsão ETS. Sombreado: Intervalos de Confiança.') +
  theme_minimal()

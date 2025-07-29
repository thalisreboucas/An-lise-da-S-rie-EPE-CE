
# Parâmetros das distribuições
size <- 6  # Parâmetro "size" da distribuição binomial negativa
prob <- 0.5  # Parâmetro "prob" da distribuição binomial negativa
lambda <- 5  # Parâmetro "lambda" da distribuição Poisson

# Valores possíveis para o eixo x
x_values <- 0:30

# Função para calcular as probabilidades da distribuição binomial negativa
negative_binomial_probs <- function(x, size, prob) {
  choose(x + size - 1, x) * prob^x * (1 - prob)^size
}

# Cálculo das probabilidades das distribuições binomial negativa e Poisson
negative_binomial_probs <- negative_binomial_probs(x_values, size, prob)
poisson_probs <- dpois(x_values, lambda)

# Criação do data frame para o gráfico
data <- data.frame(x = rep(x_values, 2),
                   y = c(negative_binomial_probs, poisson_probs),
                   distribuicao = rep(c("Binomial Negativa", "Poisson"), each = length(x_values)))

# Plot do gráfico
ggplot(data, aes(x = as.factor(x), y = y, fill = distribuicao)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Valores", y = "Probabilidades", fill = "Distribuição") +
  scale_fill_manual(values = c("lightblue4", "gray10")) + 
  ggtitle("Comparação da Distribuição Binomial Negativa e Poisson") +
  theme_minimal()

#####################################################

# Função para calcular as probabilidades da distribuição binomial negativa
negative_binomial_probs <- function(x, size, prob) {
  choose(x + size - 1, x) * prob^x * (1 - prob)^size
}

# Parâmetros das distribuições
size <- 10  # Parâmetro "size" da distribuição binomial negativa
prob <- 0.5  # Parâmetro "prob" da distribuição binomial negativa
lambda <- 5  # Parâmetro "lambda" da distribuição Poisson
psi <- 0.2  # Parâmetro "psi" da distribuição Zero-Inflated Poisson

# Valores possíveis para o eixo x
x_values <- 0:30

# Cálculo das probabilidades das distribuições binomial negativa, Poisson e Zero-Inflated Poisson
negative_binomial_probs <- negative_binomial_probs(x_values, size, prob)
poisson_probs <- dpois(x_values, lambda)
zip_probs <- ifelse(x_values == 0, psi + (1 - psi) * dpois(x_values, lambda), (1 - psi) * dpois(x_values, lambda))

# Criação do data frame para o gráfico
data <- data.frame(x = rep(x_values, 3),
                   y = c(negative_binomial_probs, poisson_probs, zip_probs),
                   distribuicao = rep(c("Binomial Negativa", "Poisson", "Zero-Inflated Poisson"), each = length(x_values)))

# Plot do gráfico

ggplot(data, aes(x = as.factor(x), y = y, fill = distribuicao)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Valores", y = "Probabilidades", fill = "Distribuição") +
  ggtitle("Comparação de Distribuições Binomial Negativa, Poisson e Zero-Inflated Poisson") +
  theme_minimal()



# Parâmetros da distribuição Poisson
lambdas <- c(2, 5, 10, 15)  # Valores diferentes de lambda

# Valores possíveis para o eixo x
x_values <- 0:30

# Cálculo das probabilidades para cada valor de lambda
poisson_probs <- sapply(lambdas, function(lambda) dpois(x_values, lambda))

# Criação do data frame para o gráfico
data <- data.frame(x = rep(x_values, length(lambdas)),
                   y = c(poisson_probs),
                   lambda = rep(lambdas, each = length(x_values)))

# Plot do gráfico
cores <- c("black", "lightblue", "gray10", "purple4")

ggplot(data, aes(x = as.factor(x), y = y, fill = as.factor(lambda))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Valores", y = "Probabilidades", fill = "Lambda") +
  scale_fill_manual(values = cores) + 
  ggtitle("Distribuição Poisson com Diferentes Valores de Lambda") +
  theme_minimal()



# Gerar dados simulados
x <- seq(1, 10, length.out = 100)
lambda <- exp(0.5 + 0.2 * x)
y <- rpois(length(x), lambda)

# Ajustar modelo de regressão Poisson
model <- glm(y ~ x, family = poisson())

# Prever valores ajustados
predicted <- predict(model, type = "response", se.fit = TRUE)

# Criar data frame com os valores
data <- data.frame(x = x, y = predicted$fit, ymin = predicted$fit - 1.96 * predicted$se.fit, 
                   ymax = predicted$fit + 1.96 * predicted$se.fit)

# Obter os coeficientes estimados
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Função para formatar a fórmula estimada da reta
format_formula <- function(intercept, slope) {
  paste0("y = exp(", round(intercept, 2), " + ", round(slope, 2), "x)")
}

# Plot do gráfico
ggplotly(ggplot() +
  geom_ribbon(data = data, aes(x = x, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.5) +
  geom_line(data = data, aes(x = x, y = y), color = "lightblue3", size = 1) +
  geom_point(data = data.frame(x, y), aes(x = x, y = y), color = "gray10", size = 2) +
  geom_text(x = 1, y = max(predicted$fit), label = format_formula(intercept, slope), hjust = 0, vjust = 1) +
  labs(x = "Variável X", y = "Contagem Y", title = "Regressão Poisson") +
  theme_minimal() )


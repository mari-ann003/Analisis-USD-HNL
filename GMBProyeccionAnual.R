# Cargar librerías necesarias
library(readr)
library(ggplot2)
library(dplyr)

# 1. Cargar y preparar los datos
datos <- read_csv("Seminario/lps_dolar.csv")
df <- as.data.frame(datos)

# 2. Funciones auxiliares
calcular_log_rendimientos <- function(precios, escala = "diaria") {
  if (escala == "diaria") {
    return(diff(log(precios)))
  } else if (escala == "semanal") {
    indices <- seq(1, length(precios), by = 5)
    return(diff(log(precios[indices])))
  } else if (escala == "mensual") {
    indices <- seq(1, length(precios), by = 21)
    return(diff(log(precios[indices])))
  }
}

calcular_mu_sigma <- function(diferencias, dias_por_año) {
  xbar <- mean(diferencias)
  volatilidad <- sd(diferencias)
  drift <- xbar + 0.5*volatilidad^2
  
  mu_anual <- drift * dias_por_año
  sigma_anual <- volatilidad * sqrt(dias_por_año)
  
  return(list(mu = mu_anual, sigma = sigma_anual))
}

GBM <- function(S0, t, N, mu, sigma) {
  dt <- t / N
  dW <- rnorm(N, mean = 0, sd = sqrt(dt))
  St <- numeric(N+1)
  St[1] <- S0
  for (i in 2:(N+1)) {
    St[i] <- St[i-1] * exp((mu - 0.5*sigma^2) * dt + sigma * dW[i-1])
  }
  return(St)
}

# 3. Calcular parámetros para diferentes escalas
dif_diaria <- calcular_log_rendimientos(df$Venta, "diaria")
dif_semanal <- calcular_log_rendimientos(df$Venta, "semanal")
dif_mensual <- calcular_log_rendimientos(df$Venta, "mensual")

params_diaria <- calcular_mu_sigma(dif_diaria, 250)
params_semanal <- calcular_mu_sigma(dif_semanal, 52)
params_mensual <- calcular_mu_sigma(dif_mensual, 12)

# 4. Configuración para predicciones
ultimo_precio <- tail(df$Venta, 1)
horizonte <- 1  # 1 año de predicción
pasos_diarios <- 250
pasos_semanales <- 52
pasos_mensuales <- 12
num_simulaciones <- 1000

# 5. Realizar múltiples simulaciones para cada escala
sim_diarias <- matrix(NA, nrow = pasos_diarios+1, ncol = num_simulaciones)
sim_semanales <- matrix(NA, nrow = pasos_semanales+1, ncol = num_simulaciones)
sim_mensuales <- matrix(NA, nrow = pasos_mensuales+1, ncol = num_simulaciones)

for(i in 1:num_simulaciones) {
  sim_diarias[,i] <- GBM(S0 = ultimo_precio, t = horizonte, N = pasos_diarios, 
                         mu = params_diaria$mu, sigma = params_diaria$sigma)
  sim_semanales[,i] <- GBM(S0 = ultimo_precio, t = horizonte, N = pasos_semanales, 
                           mu = params_semanal$mu, sigma = params_semanal$sigma)
  sim_mensuales[,i] <- GBM(S0 = ultimo_precio, t = horizonte, N = pasos_mensuales, 
                           mu = params_mensual$mu, sigma = params_mensual$sigma)
}

# 6. Calcular intervalos de confianza
intervalo_diario <- apply(sim_diarias, 1, quantile, probs = c(0.025, 0.5, 0.975))
intervalo_semanal <- apply(sim_semanales, 1, quantile, probs = c(0.025, 0.5, 0.975))
intervalo_mensual <- apply(sim_mensuales, 1, quantile, probs = c(0.025, 0.5, 0.975))

# 7. Visualización de resultados
# Crear data frames para ggplot
df_diario <- data.frame(
  Dia = 0:pasos_diarios,
  Mediana = intervalo_diario[2,],
  Inferior = intervalo_diario[1,],
  Superior = intervalo_diario[3,],
  Escala = "Diaria"
)

df_semanal <- data.frame(
  Dia = seq(0, horizonte*250, length.out = pasos_semanales+1),  # 0 a 250 días en 53 pasos
  Mediana = intervalo_semanal[2,],
  Inferior = intervalo_semanal[1,],
  Superior = intervalo_semanal[3,],
  Escala = "Semanal"
)

df_mensual <- data.frame(
  Dia = seq(0, horizonte*250, length.out = pasos_mensuales+1),  # 0 a 250 días en 13 pasos
  Mediana = intervalo_mensual[2,],
  Inferior = intervalo_mensual[1,],
  Superior = intervalo_mensual[3,],
  Escala = "Mensual"
)
# Combinar todos los datos
df_plot <- bind_rows(df_diario, df_semanal, df_mensual)

# Gráfico principal
ggplot(df_plot, aes(x = Dia, y = Mediana, color = Escala)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Inferior, ymax = Superior, fill = Escala), 
              alpha = 0.2, color = NA) +
  geom_hline(yintercept = ultimo_precio, linetype = "dashed", color = "red") +
  labs(title = "Predicción del Precio del Dólar a 1 Año",
       subtitle = paste("Último precio observado:", round(ultimo_precio, 2)),
       x = "Días futuros", y = "Precio proyectado",
       color = "Escala", fill = "Escala") +
  theme_minimal() +
  scale_color_manual(values = c("Diaria" = "blue", "Semanal" = "darkgreen", "Mensual" = "purple")) +
  scale_fill_manual(values = c("Diaria" = "blue", "Semanal" = "darkgreen", "Mensual" = "purple"))

# 8. Resultados numéricos
cat("=== RESUMEN DE PREDICCIONES A 1 AÑO ===\n")
cat("\nEscala Diaria:\n")
cat("  - Mediana:", round(intervalo_diario[2, pasos_diarios+1], 2), "\n")
cat("  - Intervalo 95%:", round(intervalo_diario[1, pasos_diarios+1], 2), "-", 
    round(intervalo_diario[3, pasos_diarios+1], 2), "\n")

cat("\nEscala Semanal:\n")
cat("  - Mediana:", round(intervalo_semanal[2, pasos_semanales+1], 2), "\n")
cat("  - Intervalo 95%:", round(intervalo_semanal[1, pasos_semanales+1], 2), "-", 
    round(intervalo_semanal[3, pasos_semanales+1], 2), "\n")

cat("\nEscala Mensual:\n")
cat("  - Mediana:", round(intervalo_mensual[2, pasos_mensuales+1], 2), "\n")
cat("  - Intervalo 95%:", round(intervalo_mensual[1, pasos_mensuales+1], 2), "-", 
    round(intervalo_mensual[3, pasos_mensuales+1], 2), "\n")

cat("\nParámetros estimados:\n")
cat("  - Diaria - Drift:", round(params_diaria$mu, 6), "Volatilidad:", round(params_diaria$sigma, 6), "\n")
cat("  - Semanal - Drift:", round(params_semanal$mu, 6), "Volatilidad:", round(params_semanal$sigma, 6), "\n")
cat("  - Mensual - Drift:", round(params_mensual$mu, 6), "Volatilidad:", round(params_mensual$sigma, 6), "\n")


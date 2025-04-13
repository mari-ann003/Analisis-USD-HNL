library(readr)
datos <- read_csv("Seminario/lps_dolar.csv")
df <- as.data.frame(datos)

# Función para calcular log-retornos con diferentes escalas de tiempo
calcular_log_rendimientos <- function(precios, escala = "diaria") {
  if (escala == "diaria") {
    return(diff(log(precios)))
  } else if (escala == "semanal") {
    # Tomar cada 5 días 
    indices <- seq(1, length(precios), by = 5)
    return(diff(log(precios[indices])))
  } else if (escala == "mensual") {
    # Tomar cada 20 días 
    indices <- seq(1, length(precios), by = 21)
    return(diff(log(precios[indices])))
  }
}

# Calcular log-retornos para diferentes escalas
dif_diaria <- calcular_log_rendimientos(df$Venta, "diaria")
dif_semanal <- calcular_log_rendimientos(df$Venta, "semanal")
dif_mensual <- calcular_log_rendimientos(df$Venta, "mensual")

# Calcular estadísticas para cada escala
calcular_mu_sigma <- function(diferencias, dias_por_año) {
  xbar <- mean(diferencias)
  volatilidad <- sd(diferencias)
  drift <- xbar + 0.5*volatilidad^2
  
  # Anualizar los parámetros
  mu_anual <- drift * dias_por_año
  sigma_anual <- volatilidad * sqrt(dias_por_año)
  
  return(list(mu = mu_anual, sigma = sigma_anual))
}

# Obtener parámetros para cada escala
params_diaria <- calcular_mu_sigma(dif_diaria, 250)
params_semanal <- calcular_mu_sigma(dif_semanal, 52)
params_mensual <- calcular_mu_sigma(dif_mensual, 12)

# Función GBM mejorada
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

# Simulaciones para diferentes escalas
GBM_diaria <- GBM(S0 = df$Venta[1], t = 25.16, N = 6340, 
                  mu = params_diaria$mu, sigma = params_diaria$sigma)

GBM_semanal <- GBM(S0 = df$Venta[1], t = 25.16, N = 6340/5, 
                   mu = params_semanal$mu, sigma = params_semanal$sigma)

GBM_mensual <- GBM(S0 = df$Venta[1], t = 25.16, N = 6340/21, 
                   mu = params_mensual$mu, sigma = params_mensual$sigma)

# Crear un solo gráfico con todas las series
plot(df$Venta, type = "l", col = "black", lwd = 2,
     main = "Comparación de GBM (Diaria, Semanal, Mensual) vs Datos reales",
     xlab = "Días", ylab = "Precio",
     ylim = range(c(GBM_diaria, GBM_semanal, GBM_mensual, df$Venta)))

# Añadir las simulaciones al mismo gráfico
lines(GBM_diaria, col = "blue", lty = 2, lwd = 1.5)      # Diaria (azul)
lines(seq(1, length(GBM_semanal)*5, by = 5), GBM_semanal, col = "green", lty = 2, lwd = 1.5)  # Semanal (verde)
lines(seq(1, length(GBM_mensual)*21, by = 21), GBM_mensual, col = "red", lty = 2, lwd = 1.5)  # Mensual (rojo)

# Leyenda
legend("topleft", 
       legend = c("Datos reales", "GBM Diaria", "GBM Semanal", "GBM Mensual"),
       col = c("black", "blue", "green", "red"),
       lty = c(1, 2, 2, 2), lwd = c(2, 1.5, 1.5, 1.5),
       bty = "n")


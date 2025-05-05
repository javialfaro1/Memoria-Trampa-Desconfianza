# Creacion Grafico Normal

library(readxl)
library(dplyr)
library(ggplot2)
install.packages("patchwork")  # Solo si no lo tienes instalado
library(patchwork)

library(readxl)

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"

# Leer el Excel
datos_wvs <- read_excel(paste0(TreatedDataPath, "WV567_final.xlsx"))

# USAR datos_wvs AQUÍ
tabla_promedios <- datos_wvs %>%
  group_by(countryISO3c) %>%
  summarise(
    Trust_mean_promedio = mean(Trust.mean, na.rm = TRUE),
    Satisfaction_mean_promedio = mean(Satisfaction.mean, na.rm = TRUE)
  )

# Calcular la media y desviación estándar de tus datos de confianza
media_trust <- mean(tabla_promedios$Trust_mean_promedio, na.rm = TRUE)
desviacion_trust <- sd(tabla_promedios$Trust_mean_promedio, na.rm = TRUE)

# Crear la curva normal teórica
x <- seq(media_trust - 4 * desviacion_trust, media_trust + 4 * desviacion_trust, length.out = 100)
y <- dnorm(x, mean = media_trust, sd = desviacion_trust)

# Transformar datos reales
datos_reales <- data.frame(Trust_mean_promedio = tabla_promedios$Trust_mean_promedio)

# Graficar
ggplot() +
  geom_line(aes(x = x, y = y), color = "blue", size = 1.5) +  
  geom_vline(xintercept = media_trust, color = "red", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = media_trust + desviacion_trust, color = "darkgreen", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = media_trust - desviacion_trust, color = "darkgreen", linetype = "dashed", size = 1) +  
  geom_point(data = datos_reales, aes(x = Trust_mean_promedio, y = 0), 
             color = "skyblue", size = 3, alpha = 0.8) +  
  labs(title = "Trust Mean Promedio de países vs Distribución Normal Ideal",
       x = "Trust Mean Promedio", y = "Densidad") +
  theme_minimal()

# TAMBIÉN debes corregir esto: usar datos_wvs
media_5 <- datos_wvs %>%
  filter(wave %in% c(5))


# 1. Calcular promedio y desviación estándar de Trust.mean para ola 5
media_trust_5 <- mean(media_5$Trust.mean, na.rm = TRUE)
desviacion_trust_5 <- sd(media_5$Trust.mean, na.rm = TRUE)

# 2. Crear la secuencia de x, asegurando que incluya la media exacta
x <- c(seq(media_trust_5 - 4 * desviacion_trust_5, media_trust_5 + 4 * desviacion_trust_5, length.out = 100), media_trust_5)
x <- sort(unique(x))  # Ordenar y eliminar duplicados

# 3. Calcular la función de densidad normal
y <- dnorm(x, mean = media_trust_5, sd = desviacion_trust_5)

# 4. Preparar tus datos reales
datos_reales_5 <- data.frame(Trust.mean = media_5$Trust.mean)

# 5. Graficar
ggplot() +
  geom_line(aes(x = x, y = y), color = "blue", size = 1.5) +  # Curva azul
  geom_vline(xintercept = media_trust_5, color = "red", linetype = "dashed", size = 1) +  # Línea en la media
  geom_vline(xintercept = media_trust_5 + desviacion_trust_5, color = "darkgreen", linetype = "dashed", size = 1) +  # +1 sd
  geom_vline(xintercept = media_trust_5 - desviacion_trust_5, color = "darkgreen", linetype = "dashed", size = 1) +  # -1 sd
  geom_point(data = datos_reales_5, aes(x = Trust.mean, y = 0), 
             color = "skyblue", size = 3, alpha = 0.8) +  # Puntitos celestes
  labs(title = "Distribución Normal de Trust Mean - Ola 5",
       x = "Trust Mean (Ola 5)", y = "Densidad") +
  annotate("text", x = media_trust_5, y = max(y), label = "Media", vjust = -1, color = "red", size = 4) +  # Etiqueta "Media"
  theme_minimal()


media_6 <- datos_wvs %>%
  filter(wave %in% c(6))

# 1. Calcular promedio y desviación estándar de Trust.mean para ola 6
media_trust_6 <- mean(media_6$Trust.mean, na.rm = TRUE)
desviacion_trust_6 <- sd(media_6$Trust.mean, na.rm = TRUE)

# 2. Crear la secuencia de x, asegurando que incluya la media exacta
x6 <- c(seq(media_trust_6 - 4 * desviacion_trust_6, media_trust_6 + 4 * desviacion_trust_6, length.out = 100), media_trust_6)
x6 <- sort(unique(x6))  # Ordenar y eliminar duplicados

# 3. Calcular la función de densidad normal
y6 <- dnorm(x6, mean = media_trust_6, sd = desviacion_trust_6)

# 4. Preparar los datos reales
datos_reales_6 <- data.frame(Trust.mean = media_6$Trust.mean)

# 5. Graficar
ggplot() +
  geom_line(aes(x = x6, y = y6), color = "blue", size = 1.5) +  # Curva azul
  geom_vline(xintercept = media_trust_6, color = "red", linetype = "dashed", size = 1) +  # Línea en la media
  geom_vline(xintercept = media_trust_6 + desviacion_trust_6, color = "darkgreen", linetype = "dashed", size = 1) +  # +1 sd
  geom_vline(xintercept = media_trust_6 - desviacion_trust_6, color = "darkgreen", linetype = "dashed", size = 1) +  # -1 sd
  geom_point(data = datos_reales_6, aes(x = Trust.mean, y = 0), 
             color = "skyblue", size = 3, alpha = 0.8) +  # Puntitos claros
  labs(title = "Distribución Normal de Trust Mean - Ola 6",
       x = "Trust Mean (Ola 6)", y = "Densidad") +
  annotate("text", x = media_trust_6, y = max(y6), label = "Media", vjust = -1, color = "red", size = 4) +  # Etiqueta "Media"
  theme_minimal()



media_7 <- datos_wvs %>%
  filter(wave %in% c(7))


# 1. Calcular promedio y desviación estándar de Trust.mean para ola 7
media_trust_7 <- mean(media_7$Trust.mean, na.rm = TRUE)
desviacion_trust_7 <- sd(media_7$Trust.mean, na.rm = TRUE)

# 2. Crear la secuencia de x, asegurando que incluya la media exacta
x7 <- c(seq(media_trust_7 - 4 * desviacion_trust_7, media_trust_7 + 4 * desviacion_trust_7, length.out = 100), media_trust_7)
x7 <- sort(unique(x7))  # Ordenar y eliminar duplicados

# 3. Calcular la función de densidad normal
y7 <- dnorm(x7, mean = media_trust_7, sd = desviacion_trust_7)

# 4. Preparar los datos reales
datos_reales_7 <- data.frame(Trust.mean = media_7$Trust.mean)

# 5. Graficar
ggplot() +
  geom_line(aes(x = x7, y = y7), color = "blue", size = 1.5) +  # Curva azul
  geom_vline(xintercept = media_trust_7, color = "red", linetype = "dashed", size = 1) +  # Línea media
  geom_vline(xintercept = media_trust_7 + desviacion_trust_7, color = "darkgreen", linetype = "dashed", size = 1) +  # +1 sd
  geom_vline(xintercept = media_trust_7 - desviacion_trust_7, color = "darkgreen", linetype = "dashed", size = 1) +  # -1 sd
  geom_point(data = datos_reales_7, aes(x = Trust.mean, y = 0), 
             color = "skyblue", size = 3, alpha = 0.8) +  # Puntitos celestes
  labs(title = "Distribución Normal de Trust Mean - Ola 7",
       x = "Trust Mean (Ola 7)", y = "Densidad") +
  annotate("text", x = media_trust_7, y = max(y7), label = "Media", vjust = -1, color = "red", size = 4) +  # Etiqueta Media
  theme_minimal()




library(ggplot2)
library(patchwork)

# 1. Calcular para todos los datos combinados
media_trust_total <- mean(datos_wvs$Trust.mean, na.rm = TRUE)
desviacion_trust_total <- sd(datos_wvs$Trust.mean, na.rm = TRUE)
x_total <- c(seq(media_trust_total - 4 * desviacion_trust_total, media_trust_total + 4 * desviacion_trust_total, length.out = 100), media_trust_total)
x_total <- sort(unique(x_total))
y_total <- dnorm(x_total, mean = media_trust_total, sd = desviacion_trust_total)

grafico_total <- ggplot() +
  geom_line(aes(x = x_total, y = y_total), color = "blue", size = 1.5) +
  geom_vline(xintercept = media_trust_total, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_total + desviacion_trust_total, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_total - desviacion_trust_total, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_point(data = datos_wvs, aes(x = Trust.mean, y = 0), 
             color = "skyblue", size = 2, alpha = 0.7) +
  labs(title = "Distribución Normal - Todas las Olas (5, 6, 7)",
       x = "Trust Mean", y = "Densidad") +
  annotate("text", x = media_trust_total, y = max(y_total), label = "Media", vjust = -1, color = "red", size = 4) +
  theme_minimal()

# 2. Calcular y graficar ola 5
grafico_5 <- ggplot() +
  geom_line(aes(x = x, y = y), color = "blue", size = 1.5) +
  geom_vline(xintercept = media_trust_5, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_5 + desviacion_trust_5, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_5 - desviacion_trust_5, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_point(data = datos_reales_5, aes(x = Trust.mean, y = 0),
             color = "skyblue", size = 2, alpha = 0.7) +
  labs(title = "Distribución Normal - Ola 5",
       x = "Trust Mean (Ola 5)", y = "Densidad") +
  annotate("text", x = media_trust_5, y = max(y), label = "Media", vjust = -1, color = "red", size = 4) +
  theme_minimal()

# 3. Calcular y graficar ola 6
grafico_6 <- ggplot() +
  geom_line(aes(x = x6, y = y6), color = "blue", size = 1.5) +
  geom_vline(xintercept = media_trust_6, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_6 + desviacion_trust_6, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_6 - desviacion_trust_6, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_point(data = datos_reales_6, aes(x = Trust.mean, y = 0),
             color = "skyblue", size = 2, alpha = 0.7) +
  labs(title = "Distribución Normal - Ola 6",
       x = "Trust Mean (Ola 6)", y = "Densidad") +
  annotate("text", x = media_trust_6, y = max(y6), label = "Media", vjust = -1, color = "red", size = 4) +
  theme_minimal()

# 4. Calcular y graficar ola 7
grafico_7 <- ggplot() +
  geom_line(aes(x = x7, y = y7), color = "blue", size = 1.5) +
  geom_vline(xintercept = media_trust_7, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_7 + desviacion_trust_7, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media_trust_7 - desviacion_trust_7, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_point(data = datos_reales_7, aes(x = Trust.mean, y = 0),
             color = "skyblue", size = 2, alpha = 0.7) +
  labs(title = "Distribución Normal - Ola 7",
       x = "Trust Mean (Ola 7)", y = "Densidad") +
  annotate("text", x = media_trust_7, y = max(y7), label = "Media", vjust = -1, color = "red", size = 4) +
  theme_minimal()


(grafico_total) / (grafico_5 | grafico_6 | grafico_7)




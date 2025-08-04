
TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "consolidado_puntos_sig.RData"))   # carga objeto WV567

library(dplyr)
library(scales)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(writexl)
library(segmented)   # ajuste con quiebre

consolidado$Trust.mean <- consolidado$Trust.mean * 100
consolidado$T_promedio <- consolidado$T_promedio * 100

dir.create("Significancia_paises", showWarnings = FALSE)

paises_validos <- consolidado %>%
  group_by(countryISO3c) %>%
  summarise(tipo_max = max(tipo, na.rm = TRUE)) %>%
  filter(tipo_max != 0) %>%
  pull(countryISO3c)

y_min <- floor(min(consolidado$Satisfaction.mean, na.rm = TRUE))
y_max <- ceiling(max(consolidado$Satisfaction.mean, na.rm = TRUE))

# Crear carpeta si no existe
dir.create("Significancia_paises", showWarnings = FALSE)

# Agrupar por país y graficar TODOS los tipos
consolidado %>%
  group_split(countryISO3c) %>%
  walk(function(df) {
    pais <- unique(df$countryISO3c)
    tipo <- unique(df$tipo)
    
    df <- df %>%
      mutate(wave = paste0("WV", wave))  # Etiqueta tipo WV
    
    if (tipo == 0) {
      g <- ggplot(df, aes(x = Trust.mean, y = Satisfaction.mean)) +
        geom_point(size = 3, color = "gray50") +
        geom_path(linewidth = 1.5, color = "gray50") +
        geom_text(aes(label = wave), vjust = -1, size = 3.5) +
        labs(title = paste("País:", pais, "| Tipo:", tipo),
             x = "Confianza media", y = "Satisfacción media") +
        scale_x_continuous(labels = label_number(accuracy = 0.01)) +
        ylim(y_min, y_max) +
        theme_minimal(base_size = 12)
      
    } else if (tipo == 1) {
      df_plot <- df %>%
        mutate(S_plot = first(S_promedio))
      
      g <- ggplot(df_plot, aes(x = Trust.mean, y = S_plot)) +
        geom_point(size = 3, color = "red") +
        geom_path(linewidth = 1.5, color = "red") +
        geom_text(aes(label = wave), vjust = -1, size = 3.5) +
        labs(title = paste("País:", pais, "| Tipo:", tipo),
             x = "Confianza media", y = "Satisfacción media") +
        scale_x_continuous(labels = label_number(accuracy = 0.01)) +
        ylim(y_min, y_max) +
        theme_minimal(base_size = 12)
      
    } else if (tipo == 2) {
      df_plot <- df %>%
        mutate(T_plot = first(T_promedio))
      
      g <- ggplot(df_plot, aes(x = T_plot, y = Satisfaction.mean)) +
        geom_point(size = 3, color = "blue") +
        geom_path(linewidth = 1.5, color = "blue") +
        geom_text(aes(label = wave), vjust = -1, size = 3.5) +
        labs(title = paste("País:", pais, "| Tipo:", tipo),
             x = "Confianza media", y = "Satisfacción media") +
        scale_x_continuous(labels = label_number(accuracy = 0.01)) +
        ylim(y_min, y_max) +
        theme_minimal(base_size = 12)
      
    } else if (tipo == 3) {
      g <- ggplot(df, aes(x = Trust.mean, y = Satisfaction.mean)) +
        geom_point(size = 3, color = "green4") +
        geom_path(linewidth = 1.5, color = "green4") +
        geom_text(aes(label = wave), vjust = -1, size = 3.5) +
        labs(title = paste("País:", pais, "| Tipo:", tipo),
             x = "Confianza media", y = "Satisfacción media") +
        scale_x_continuous(labels = label_number(accuracy = 0.01)) +
        ylim(y_min, y_max) +
        theme_minimal(base_size = 12)
    }
    
    ggsave(file.path("Significancia_paises", paste0(pais, ".png")),
           g, width = 5, height = 4, dpi = 300, bg = "white")
  })


#########################################################################
#### Normalizacion por el Min ratio Significativo

df_model <- consolidado %>% 
  mutate(
    Trust.mean  = case_when(
      tipo == 2 ~ T_promedio,      # tipo 2: sustituir Trust
      TRUE      ~ Trust.mean
    ),
    Satisfaction.mean = case_when(
      tipo == 1 ~ S_promedio,      # tipo 1: sustituir Satisfaction
      TRUE      ~ Satisfaction.mean
    )
  ) %>% 
  select(countryISO3c, wave, Trust.mean, Satisfaction.mean, tipo)

## NORMALIZACION DATOS 
df_model_normalizado <- df_model %>% 
  group_by(countryISO3c) %>% 
  mutate(
    Trust_min  = min(Trust.mean, na.rm = TRUE),
    Trust_max  = max(Trust.mean, na.rm = TRUE),
    Satisf_min = min(Satisfaction.mean, na.rm = TRUE),
    Satisf_max = max(Satisfaction.mean, na.rm = TRUE),
    
    Trust_norm  = Trust.mean/ Trust_min,
    Satisf_norm = Satisfaction.mean  / Satisf_min,
    Trust_norm  = Trust_norm -1 ,                    # Δ respecto al mínimo
    Satisf_norm = Satisf_norm -1
  ) %>% 
  ungroup() %>% 
  select(countryISO3c, wave, Trust_norm, Satisf_norm, tipo)


df_log <- df_model %>%
  mutate(
    Trust_norm = log(Trust.mean),
    Satisf_norm = log(Satisfaction.mean)
  ) %>% 
  ungroup() %>% 
  select(countryISO3c, wave, Trust_norm, Satisf_norm)


ggplot(df_log, aes(Trust_norm, Satisf_norm)) +
  geom_point(alpha = 0.4, colour = "purple") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "Distribución normalización por mínimos en confianza y satisfacción",
       x = "Confianza", y = " Satisfacción") +
  theme_minimal(base_size = 13)




ggplot(consolidado, aes(x = Trust.mean, y = Satisfaction.mean, color = as.factor(wave))) +
  geom_point(size = 3) +
  geom_text(aes(label = countryISO3c), vjust = -0.8, size = 3) +
  scale_color_manual(
    name = "Ola",
    values = c(
      "5" = "#F8766D",  # Coral suave
      "6" = "#00BFC4",  # Azul cielo
      "7" = "#7CAE00"   # Verde menta
    )
  ) +
  labs(
    title = "Confianza vs. Satisfacción por ola",
    x = "Promedio Confianza",
    y = "Promedio Satisfacción"
  ) +
  theme_minimal(base_size = 13)


#####################################################################################



stats_trust_modelo <- df_model %>% 
  summarise(
    # TRUST
    trust_min    = min(Trust.mean, na.rm = TRUE),
    trust_max    = max(Trust.mean, na.rm = TRUE),
    trust_mean   = mean(Trust.mean, na.rm = TRUE),
    trust_median = median(Trust.mean, na.rm = TRUE),
    trust_sd     = sd(Trust.mean, na.rm = TRUE),
    
    # SATISFACCIÓN
    sat_min    = min(Satisfaction.mean, na.rm = TRUE),
    sat_max    = max(Satisfaction.mean, na.rm = TRUE),
    sat_mean   = mean(Satisfaction.mean, na.rm = TRUE),
    sat_median = median(Satisfaction.mean, na.rm = TRUE),
    sat_sd     = sd(Satisfaction.mean, na.rm = TRUE)
  )


stats_trust_min <- df_model_normalizado %>% 
  summarise(
    # TRUST
    trust_min    = min(Trust_norm, na.rm = TRUE),
    trust_max    = max(Trust_norm, na.rm = TRUE),
    trust_mean   = mean(Trust_norm, na.rm = TRUE),
    trust_median = median(Trust_norm, na.rm = TRUE),
    trust_sd     = sd(Trust_norm, na.rm = TRUE),
    trust_p80     = quantile(Trust_norm, 0.80, na.rm = TRUE),
    trust_p90     = quantile(Trust_norm, 0.90, na.rm = TRUE),
    
    # SATISFACCIÓN
    sat_min    = min(Satisf_norm, na.rm = TRUE),
    sat_max    = max(Satisf_norm, na.rm = TRUE),
    sat_mean   = mean(Satisf_norm, na.rm = TRUE),
    sat_median = median(Satisf_norm, na.rm = TRUE),
    sat_sd     = sd(Satisf_norm, na.rm = TRUE)
  )


stats_trust_log <- df_log %>% 
  summarise(
    # TRUST
    trust_min    = min(Trust_norm, na.rm = TRUE),
    trust_max    = max(Trust_norm, na.rm = TRUE),
    trust_mean   = mean(Trust_norm, na.rm = TRUE),
    trust_median = median(Trust_norm, na.rm = TRUE),
    trust_sd     = sd(Trust_norm, na.rm = TRUE),
    trust_p80     = quantile(Trust_norm, 0.80, na.rm = TRUE),
    trust_p90     = quantile(Trust_norm, 0.90, na.rm = TRUE),
    
    # SATISFACCIÓN
    sat_min    = min(Satisf_norm, na.rm = TRUE),
    sat_max    = max(Satisf_norm, na.rm = TRUE),
    sat_mean   = mean(Satisf_norm, na.rm = TRUE),
    sat_median = median(Satisf_norm, na.rm = TRUE),
    sat_sd     = sd(Satisf_norm, na.rm = TRUE)
  )


###########################################################################


#OBTENCIÓN BASE IQR (sin outlayers)
q1 <- quantile(df_model_normalizado$Trust_norm, 0.25, na.rm = TRUE)
q3 <- quantile(df_model_normalizado$Trust_norm, 0.75, na.rm = TRUE)
iqr <- q3 - q1

# Límites inferior y superior para identificar outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Ver los valores calculados
cat("Q1:", q1, "\nQ3:", q3, "\nIQR:", iqr, "\n")
cat("Límite inferior:", lower_bound, "\nLímite superior:", upper_bound, "\n")

# Filtrar los datos sin outliers
df_normalizado_IQR <- df_model_normalizado[
                                      df_model_normalizado$Trust_norm <= upper_bound, ]

################################################################################

####test de chow### 

library(dplyr)
library(broom)     # tidy()
library(purrr)     # map()


taus <- seq(from = 0, by = 0.05)

# adapta al rango real de Trust
chow_test <- function(data, cut){
  d <- data %>% mutate(grupo = if_else(Trust_norm > cut, "post", "pre"))
  d_pre  <- filter(d, grupo == "pre")
  d_post <- filter(d, grupo == "post")
  
  if (nrow(d_pre) < 5 | nrow(d_post) < 5) {
    return(tibble(
      tau = cut,
      n_pre = nrow(d_pre),
      n_post = nrow(d_post),
      slope_pre = NA_real_,
      slope_post = NA_real_,
      F_chow = NA_real_,
      p_value = NA_real_
    ))
  }
  
  m_pre  <- lm(Satisf_norm ~ Trust_norm, data = d_pre)
  m_post <- lm(Satisf_norm ~ Trust_norm, data = d_post)
  m_pool <- lm(Satisf_norm ~ Trust_norm, data = d)
  ft <- anova(m_pool, lm(Satisf_norm ~ Trust_norm * grupo, data = d))
  
  tibble(
    tau = cut,
    n_pre = nrow(d_pre),
    n_post = nrow(d_post),
    slope_pre = coef(m_pre)["Trust_norm"],
    slope_post = coef(m_post)["Trust_norm"],
    F_chow = ft$`F`[2],
    p_value = ft$`Pr(>F)`[2]
  )
}

resultados_chow1 <- map_dfr(taus, ~ chow_test(df_model_normalizado, .x))
resultados_chow3 <- map_dfr(taus, ~ chow_test(df_normalizado_IQR, .x))
resultados_chow4 <- map_dfr(taus, ~ chow_test(df_log, .x))

# Ordena por p-valor ascendente
resultados_chow1 <- resultados_chow1 %>% arrange(p_value)
resultados_chow3 <- resultados_chow3 %>% arrange(p_value)
resultados_chow4 <- resultados_chow4 %>% arrange(p_value)



plot_chow <- function(resultados, titulo) {
  ggplot(resultados, aes(x = tau, y = p_value)) +
    geom_line(color = "gray30") +
    geom_point(aes(color = p_value < 0.05), size = 2) +
    scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                       labels = c("TRUE" = "Significativo", "FALSE" = "No significativo"),
                       name = "p-value < 0.05") +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "green", linewidth = 0.6) +
    annotate("text", x = 0.05, y = 0.06, label = "P_value = 0.05", 
             color = "black", size = 3, hjust = 0) +
    scale_x_continuous(
      limits = c(min(resultados$tau, na.rm = TRUE), max(resultados$tau, na.rm = TRUE)),
      breaks = pretty(resultados$tau, n = 5)
    ) +
    scale_y_continuous(limits = c(0, 1.05)) +
    labs(title = titulo,
         x = "Tau (Corte en confianza)",
         y = "p-value del test de Chow") +
    theme_minimal(base_size = 12)
}


plot_chow(resultados_chow1, "Test de Chow sobre confianza normalizada por mínimos")
plot_chow(resultados_chow3, "Test de Chow sobre confianza normalizada por mínimos (IQR)")
plot_chow(resultados_chow_quad, "Test de Chow sobre confianza normalizada por mínimos en regresión cuadrática (IQR)")
plot_chow(resultados_chow4, "Test de Chow normalizado por logaritmos")


# Visualiza los mejores candidatos
print(resultados_chow4, n = Inf)

mejor_tau <- resultados_chow4 %>%
  filter(!is.na(p_value)) %>%
  slice_min(p_value, n = 1)

tau_optimo <- mejor_tau$tau[1]  # O usa pull(tau)
print(tau_optimo)

tau_optimo <- 1.75

# Separar los datos correctamente
df_pre  <- df_log %>% filter(Trust_norm <= tau_optimo)
df_post <- df_log %>% filter(Trust_norm > tau_optimo)

# Ajustar modelos
modelo_pre  <- lm(Satisf_norm ~ Trust_norm, data = df_pre)
modelo_post <- lm(Satisf_norm ~ Trust_norm, data = df_post)

summary(modelo_pre)
summary(modelo_post)


# Mostrar ecuaciones
coef_pre <- coef(modelo_pre)
coef_post <- coef(modelo_post)

cat("Recta pre-quiebre:   Satisf_norm =", round(coef_pre[1], 3), "+", round(coef_pre[2], 3), "* Trust_norm\n")
cat("Recta post-quiebre:  Satisf_norm =", round(coef_post[1], 3), "+", round(coef_post[2], 3), "* Trust_norm\n")

# Segmentación explícita
df_log_segmentado <- df_log %>%
  mutate(segmento = ifelse(Trust_norm <= tau_optimo, "pre", "post"))


###############################################################################################
# Gráfico sin cruces de regresión
ggplot(df_log_segmentado, aes(x = Trust_norm, y = Satisf_norm)) +
  geom_point(aes(color = segmento), alpha = 0.7) +
  geom_vline(xintercept = tau_optimo, linetype = "dashed", color = "black") +
  
  # Línea solo para pre-quiebre
  geom_smooth(data = df_pre, method = "lm", se = FALSE, color = "turquoise3") +
  
  # Línea solo para post-quiebre
  geom_smooth(data = df_post, method = "lm", se = FALSE, color = "indianred") +
  
  scale_color_manual(values = c("pre" = "turquoise3", "post" = "indianred")) +
  
  labs(
    title = paste0("Segmentación por test de Chow (τ = ", round(tau_optimo, 2), " )"),
    x = "Trust normalizado",
    y = "Satisfacción normalizada",
    color = "Segmento"
  ) +
  theme_minimal(base_size = 12)


##########################################################################################################

##### Test CHOW cuadratico

library(dplyr)
library(broom)
library(purrr)

taus <- seq(0, by = 0.05)

# Función modificada para el modelo cuadrático


chow_test_quad <- function(data, cut) {
  d <- data %>% mutate(grupo = if_else(Trust_norm > cut, "post", "pre"))
  d_pre  <- filter(d, grupo == "pre")
  d_post <- filter(d, grupo == "post")
  
  if (nrow(d_pre) < 5 | nrow(d_post) < 5) {
    return(tibble(
      tau = cut,
      n_pre = nrow(d_pre),
      n_post = nrow(d_post),
      beta1_pre = NA_real_,
      beta2_pre = NA_real_,
      beta1_post = NA_real_,
      beta2_post = NA_real_,
      F_chow = NA_real_,
      p_value = NA_real_
    ))
  }
  
  m_pre  <- lm(Satisf_norm ~ Trust_norm + I(Trust_norm^2), data = d_pre)
  m_post <- lm(Satisf_norm ~ Trust_norm + I(Trust_norm^2), data = d_post)
  m_pool <- lm(Satisf_norm ~ Trust_norm + I(Trust_norm^2), data = d)
  ft     <- anova(m_pool, lm(Satisf_norm ~ (Trust_norm + I(Trust_norm^2)) * grupo, data = d))
  
  tibble(
    tau = cut,
    n_pre = nrow(d_pre),
    n_post = nrow(d_post),
    beta1_pre = coef(m_pre)["Trust_norm"],
    beta2_pre = coef(m_pre)["I(Trust_norm^2)"],
    beta1_post = coef(m_post)["Trust_norm"],
    beta2_post = coef(m_post)["I(Trust_norm^2)"],
    F_chow = ft$`F`[2],
    p_value = ft$`Pr(>F)`[2]
  )
}


modelo_cuad <- lm(Satisf_norm ~ Trust_norm + I(Trust_norm^2), data = df_normalizado_IQR)
summary(modelo_cuad)

# Aplica la función
resultados_chow_quad <- map_dfr(taus, ~ chow_test_quad(df_model_normalizado, .x)) %>%
  arrange(p_value)

# Identifica el mejor punto de corte
mejor_tau_quad <- resultados_chow_quad %>%
  filter(!is.na(p_value)) %>%
  slice_min(p_value, n = 1)



tau_optimo_quad <- mejor_tau_quad$tau[1]
print(tau_optimo_quad)

tau_optimo_quad <- 1

# Separar datos y ajustar modelos finales
df_pre  <- df_model_normalizado %>% filter(Trust_norm <= tau_optimo_quad)
df_post <- df_model_normalizado %>% filter(Trust_norm > tau_optimo_quad)

modelo_pre  <- lm(Satisf_norm ~ Trust_norm + I(Trust_norm^2), data = df_pre)
modelo_post <- lm(Satisf_norm ~ Trust_norm + I(Trust_norm^2), data = df_post)

summary(modelo_pre)
summary(modelo_post)

0# Mostrar ecuaciones simplificadas
coef_pre <- coef(modelo_pre)
coef_post <- coef(modelo_post)

cat("Modelo pre-quiebre:\nSatisf_norm =",
    round(coef_pre[1], 3), "+", round(coef_pre[2], 3), "* Trust_norm +",
    round(coef_pre[3], 3), "* Trust_norm^2\n")

cat("Modelo post-quiebre:\nSatisf_norm =",
    round(coef_post[1], 3), "+", round(coef_post[2], 3), "* Trust_norm +",
    round(coef_post[3], 3), "* Trust_norm^2\n")

###############################


# Generar secuencias de confianza para cada tramo
trust_pre <- seq(min(df_model_normalizado$Trust_norm), tau_optimo_quad, length.out = 100)
trust_post <- seq(tau_optimo_quad, max(df_model_normalizado$Trust_norm), length.out = 100)

# Coeficientes pre-quiebre
a1 <- coef(modelo_pre)[1]
b1 <- coef(modelo_pre)[2]
c1 <- coef(modelo_pre)[3]

# Coeficientes post-quiebre
a2 <- coef(modelo_post)[1]
b2 <- coef(modelo_post)[2]
c2 <- coef(modelo_post)[3]

# Crear dataframes con las curvas ajustadas
curva_pre <- data.frame(
  Trust_norm = trust_pre,
  Satisf_norm = a1 + b1 * trust_pre + c1 * trust_pre^2,
  tramo = "Pre-quiebre"
)

curva_post <- data.frame(
  Trust_norm = trust_post,
  Satisf_norm = a2 + b2 * trust_post + c2 * trust_post^2,
  tramo = "Post-quiebre"
)

# Unir las curvas
curvas_chow_quad <- rbind(curva_pre, curva_post)

# Graficar
ggplot(df_model_normalizado, aes(x = Trust_norm, y = Satisf_norm)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_line(data = curvas_chow_quad, aes(color = tramo), size = 1) +
  labs(title = "Curvas cuadráticas pre y post-quiebre (Test de Chow)",
       x = "Confianza normalizada",
       y = "Satisfacción normalizada") +
  theme_minimal() +
  scale_color_manual(values = c("Pre-quiebre" = "#0072B2", "Post-quiebre" = "#D55E00"))


#################################################################################



df_log_chow <- df_log %>%
  mutate(grupo = if_else(Trust_norm <= tau_optimo, "pre", "post"))


# Graficar dispersión con líneas de regresión por grupo
ggplot(df_log_chow, aes(x = Trust_norm, y = Satisf_norm, color = grupo)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  # rectas pre y post
  geom_vline(xintercept = tau_optimo, linetype = "dashed", color = "black") +  # línea vertical en τ
  labs(
    title = paste("Segmentación por test de Chow (τ =", round(tau_optimo, 2), ")"),
    x = "Log Trust",
    y = "Log Satisfaction",
    color = "Segmento"
  ) +
  theme_minimal()

##################################################################################

modelo_normalizado <- lm(Satisf_norm ~ Trust_norm, data = df_model_normalizado)
modelo_iqr <- lm(Satisf_norm ~ Trust_norm, data = df_normalizado_IQR)

summary(modelo_normalizado)
summary(modelo_iqr)

stargazer(modelo_cuad,
          type = "latex",
          digits = 3,               # reduce a 3 decimales
          star.cutoffs = c(0.05, 0.01, 0.001), 
          star.char = c("*", "**", "***"),
          omit.stat = c("f", "ser"),
          title = "Resultados del modelo de regresión lineal",
          label = "tab:regresion_trust_satisf",
          dep.var.labels = "Satisfacción normalizada",
          covariate.labels = c("Confianza normalizada"),
          out = "modelo_regresion.tex")


ggplot(df_normalizado_IQR, aes(x = Trust_norm, y = Satisf_norm)) +
  geom_point(color = "mediumpurple", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(
    title = "Relación entre confianza y satisfacción (modelo sin segmentar)",
    x = "Confianza normalizada",
    y = "Satisfacción normalizada"
  ) +
  theme_minimal(base_size = 12)
































##
# Test para ver rupturas en el modelo.
if (!require("segmented")) install.packages("segmented")
if (!require("mgcv")) install.packages("mgcv")
if (!require("gratia")) install.packages("gratia")
library(segmented)
library(mgcv)
library(gratia)
library(dplyr)

# Davies test
modelo_davies <- lm(Satisf_norm ~ Trust_norm, data = df_model_normalizado)
print(davies.test(modelo_davies, seg.Z = ~ Trust_norm))

# GAM (cambio con curva suave)
modelo_gam <- gam(Satisf_norm ~ s(Trust_norm), data = df_model_normalizado)
cat("\n▶ GAM — modelo suavizado\n")
print(summary(modelo_gam))

#Parametros GAM 
df_gam_pred <- data.frame(
  Trust_norm = seq(min(df_model_normalizado$Trust_norm),
                   max(df_model_normalizado$Trust_norm),
                   length.out = 300)
) %>%
  mutate(
    Satisf_pred = predict(modelo_gam, newdata = ., type = "response")
  )


#Obtencion parametros, buscando primera derivada >0 
derivada <- derivatives(modelo_gam, select = "s(Trust_norm)", interval = "confidence")
deriv_sig <- derivada %>% filter(.lower_ci  > 0)

if (nrow(deriv_sig) > 0) {
  tau_hat_gam <- min(deriv_sig$Trust_norm)
  cat("\n▶ Derivada del GAM\n")
  cat("Umbral estimado τ̂ (primera f’ > 0) ≈", round(tau_hat_gam, 4), "\n")
} else {
  cat("\n▶ Derivada del GAM\n")
  cat("No se detectó punto donde la pendiente sea significativamente positiva.\n")
}



# Analisis curva despues del umbral

# ───────────────────────────────────────
# 0. Librerías necesarias
# ───────────────────────────────────────
library(dplyr)
library(mgcv)
library(purrr)
library(broom)

# ───────────────────────────────────────
# 1. Filtrar puntos después del umbral τ̂ ≈ 1.6507
# ───────────────────────────────────────
tau <- 1.6507

df_post <- df_model_normalizado %>%
  filter(Trust_norm > tau) %>%
  mutate(
    x = Trust_norm,  # confianza relativa al umbral (centrado en 0)
    y = Satisf_norm         # satisfacción normalizada
  )


# ───────────────────────────────────────
# 2. Ajustar modelos candidatos
# ───────────────────────────────────────

# A) Modelo lineal
m_lin <- lm(y ~ x, data = df_post)

# B) Modelo cuadrático
m_quad <- lm(y ~ poly(x, 2), data = df_post)

# C) Exponencial saturante
m_exp <- nls(y ~ a + c * (1 - exp(-g * x)), 
             data = df_post,
             start = list(a = 0.01, c = 0.05, g = 1),
             control = nls.control(maxiter = 100, warnOnly = TRUE))

# D) Logística desplazada
m_log <- nls(y ~ a + c / (1 + exp(-g * (x - k))), 
             data = df_post,
             start = list(a = 0.01, c = 0.05, g = 3, k = 0.2),
             control = nls.control(maxiter = 100, warnOnly = TRUE))

# E) GAM
m_gam <- gam(y ~ s(x, k = 6), data = df_post)


# ───────────────────────────────────────
# 3. Comparar modelos: AIC, BIC, R²
# ───────────────────────────────────────

# Guardar todos los modelos en una lista
mod_list <- list(
  lineal = m_lin,
  cuadratico = m_quad,
  logistica = m_log,
  gam = m_gam
)

# AIC y BIC
tabla_AIC <- map_dfr(mod_list, AIC, .id = "modelo")
tabla_BIC <- map_dfr(mod_list, BIC, .id = "modelo")



# Obtencion Parametros 
# ──────────────────────────────────────────────
# 1.  Valor de T y de la curva GAM en ese punto
# ──────────────────────────────────────────────
T_hat <- 1.6507                              # tu umbral obtenido antes
a_hat <- predict(modelo_gam,
                 newdata = data.frame(Trust_norm = T_hat),
                 type    = "response")       # ← este es 'a'

# ──────────────────────────────────────────────
# 2.  Preparar datos sólo para t ≥ T, centrados
# ──────────────────────────────────────────────
df_post <- df_model_normalizado %>% 
  filter(Trust_norm >= T_hat) %>%            # incluye el propio T
  mutate(
    x_c = Trust_norm - T_hat,                # (t - T)
    y_c = Satisf_norm - a_hat                # f(t) - f(T)
  )

# ──────────────────────────────────────────────
# 3.  Ajuste cuadrático SIN intercepto
# ──────────────────────────────────────────────
m_quad <- lm(y_c ~ 0 + x_c + I(x_c^2), data = df_post)

# ──────────────────────────────────────────────
# 4.  Extraer coeficientes
# ──────────────────────────────────────────────
b1_hat <- coef(m_quad)["x_c"]
b2_hat <- coef(m_quad)["I(x_c^2)"]

cat("a  =", round(a_hat,4), "\n",
    "b1 =", round(b1_hat,4), "\n",
    "b2 =", round(b2_hat,4), "\n")





# ════════════════════════════════════════════════════════════════
# Gráfico comparativo: GAM vs Modelo cuadrático anclado
# ════════════════════════════════════════════════════════════════

library(scales)  # para transparencia con alpha()

# 1. Gráfico base de puntos
plot(df_model_normalizado$Trust_norm,
     df_model_normalizado$Satisf_norm,
     pch  = 16,
     col  = alpha("grey30", 0.4),
     xlab = expression(Delta~Trust[normalizado]),
     ylab = expression(Delta~Satisfaccion[normalizada]))

# 2. Línea GAM (suavizada, azul)
grid_all <- data.frame(
  Trust_norm = seq(min(df_model_normalizado$Trust_norm),
                   max(df_model_normalizado$Trust_norm),
                   length.out = 300)
)

lines(grid_all$Trust_norm,
      predict(modelo_gam, newdata = grid_all),
      lwd = 2, col = "blue")

# 3. Línea cuadrática anclada (roja)
grid_post <- data.frame(
  Trust_norm = seq(T_hat,
                   max(df_model_normalizado$Trust_norm),
                   length.out = 200)
)
grid_post$x_c <- grid_post$Trust_norm - T_hat
pred_quad <- a_hat + predict(m_quad, newdata = grid_post)

lines(grid_post$Trust_norm, pred_quad,
      col = "red", lwd = 2)

# 4. Leyenda
legend("topleft",
       legend = c("GAM", "Modelo Cuadrático"),
       lwd    = 2,
       col    = c("blue", "red"),
       bty    = "n")




##### Grafico FINAL 

# ──────────────────────────────────────────────
# 1.  Extraer b1 y b2 directamente del modelo
# ──────────────────────────────────────────────
b1_hat <- coef(m_quad)[["x_c"]]
b2_hat <- coef(m_quad)[["I(x_c^2)"]]

# ──────────────────────────────────────────────
# 2.  Función por tramos construida con variables
# ──────────────────────────────────────────────
piecewise_fun <- function(x) {
  ifelse(
    x < T_hat,
    a_hat,
    a_hat + b1_hat * (x - T_hat) + b2_hat * (x - T_hat)^2
  )
}

# ──────────────────────────────────────────────
# 3.  Predicciones para dibujar la curva
# ──────────────────────────────────────────────
x_grid <- seq(
  min(df_model_normalizado$Trust_norm, na.rm = TRUE),
  max(df_model_normalizado$Trust_norm, na.rm = TRUE),
  length.out = 400
)

pred_df <- data.frame(
  Trust_norm  = x_grid,
  Satisf_pred = piecewise_fun(x_grid)
)

# ──────────────────────────────────────────────
# 4.  Gráfico final
# ──────────────────────────────────────────────
library(ggplot2)

ggplot(df_model_normalizado,
       aes(Trust_norm, Satisf_norm)) +
  geom_point(alpha = .4, colour = "grey40") +
  geom_line(data = pred_df,
            aes(Trust_norm, Satisf_pred),
            colour = "red", linewidth = 1.2) +
  geom_vline(xintercept = T_hat,
             linetype   = "dashed",
             colour     = "black") +
  ## ←–– Aquí aparece el texto con T y a
  annotate("text",
           x     = T_hat,
           y     = a_hat,
           label = sprintf("T = %.3f\na = %.3f", T_hat, a_hat),
           vjust = -1.0,    # un poquito arriba del punto
           hjust = -0.05,   # a la derecha de la línea
           size  = 4.2,
           fontface = "bold") +
  labs(title = "Función por tramos ajustada (variables dinámicas)",
       x = expression(Delta~Trust[normalizado]),
       y = expression(Delta~Satisfaccion[normalizada])) +
  theme_minimal(base_size = 13)




save(df_model_normalizado, file = "df_model_normalizado.RData")
save(df_model, file = "df_model.RData")
save(consolidado, file = "df_model.RData")

# Instalar si no lo tienes
install.packages("writexl")

# Guardar en Excel
writexl::write_xlsx(consolidado, path = "df_model.xlsx")

writexl::write_xlsx(minimos_por_pais, path = "minimos_normalizacion.xlsx")







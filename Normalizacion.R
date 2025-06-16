# Determinacion puntos Significativos (nuevo) para proxima normalizacion

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "WV567_Filtrados.RData"))   # carga objeto WV567


# ────────────────────
# 1. Cargar librerías
# ────────────────────
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(writexl)
library(segmented)   # ajuste con quiebre
   

WV567_final <- WV567 %>% 
  select(countryISO3c, wave, Trust.mean, Satisfaction.mean, 
         DeltaSatisfaction5_6, DeltaTrust5_6, 
         DeltaSatisfaction6_7, DeltaTrust6_7, 
         DeltaTrust5_7, DeltaSatisfaction5_7)

WV567_deltas <- WV567_final %>%
  group_by(countryISO3c) %>%
  summarise(
    across(starts_with("Delta"),
           ~ if(all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE))
  ) %>%
  ungroup()

# ────────────────────
# 3. Función DEFINITIVA para detectar olas significativas
# ────────────────────

sig_waves <- function(row, pref) {
  waves <- c()
  pares <- list("5_6" = c(5, 6), "6_7" = c(6, 7), "5_7" = c(5, 7))
  
  for (p in names(pares)) {
    delta_value <- row[[paste0("Delta", pref, p)]]
    if (isTRUE(delta_value == 1)) {
      waves <- c(waves, pares[[p]])
    }
  }
  if (length(waves) == 0) return(integer(0))
  unique(waves)
}

# ────────────────────
# 4. Procesamiento completo por país
# ────────────────────

consolidado <- WV567_final %>% 
  group_split(countryISO3c) %>% 
  map_dfr(function(tbl) {
    row <- tbl[1, ]
    wT  <- sig_waves(row, "Trust")
    wS  <- sig_waves(row, "Satisfaction")
    
    tipo <- case_when(
      length(wT) > 0 & length(wS) > 0 ~ 3,
      length(wT) > 0 & length(wS) == 0 ~ 1,
      length(wS) > 0 & length(wT) == 0 ~ 2,
      TRUE ~ 0
    )
    
    waves_all <- unique(c(wT, wS))
    if(tipo == 0) waves_all <- tbl$wave
    
    puntos <- tbl %>% 
      filter(wave %in% waves_all) %>% 
      mutate(tipo = tipo) %>% 
      select(countryISO3c, wave, Trust.mean, Satisfaction.mean, tipo)
    
    if (tipo == 1) {
      S_prom <- tbl %>% filter(wave %in% wT) %>% summarise(S_promedio = mean(Satisfaction.mean, na.rm = TRUE)) %>% pull(S_promedio)
      puntos <- puntos %>% mutate(S_promedio = S_prom, T_promedio = NA_real_)
    } else if (tipo == 2) {
      T_prom <- tbl %>% filter(wave %in% wS) %>% summarise(T_promedio = mean(Trust.mean, na.rm = TRUE)) %>% pull(T_promedio)
      puntos <- puntos %>% mutate(S_promedio = NA_real_, T_promedio = T_prom)
    } else {
      puntos <- puntos %>% mutate(S_promedio = NA_real_, T_promedio = NA_real_)
    }
    
    return(puntos)
  })

# ────────────────────
# 5. Exportar tabla consolidada
# ────────────────────

save(consolidado, file = paste0(TreatedDataPath, "consolidado_puntos_sig.RData"))


# ────────────────────

dir.create("Significancia_paises", showWarnings = FALSE)

paises_validos <- consolidado %>%
  group_by(countryISO3c) %>%
  summarise(tipo_max = max(tipo, na.rm = TRUE)) %>%
  filter(tipo_max != 0) %>%
  pull(countryISO3c)

consolidado %>%
  filter(countryISO3c %in% paises_validos) %>%
  group_split(countryISO3c) %>%
  walk(function(df) {
    pais <- unique(df$countryISO3c)
    tipo <- unique(df$tipo)
    
    if (tipo == 1) {
      # Solo confianza significativa
      df_plot <- df %>%
        mutate(S_plot = first(S_promedio))
      
      g <- ggplot(df_plot, aes(x = Trust.mean, y = S_plot)) +
        geom_point(size = 3, color = "red") +
        geom_path(linewidth = 1.5, color = "red") +
        geom_text(aes(label = wave), vjust = -1, size = 3.5) +
        labs(title = paste("País:", pais, "| Tipo:", tipo),
             x = "Confianza media", y = "Satisfacción media") +
        theme_minimal(base_size = 12)
      
    } else if (tipo == 2) {
      # Solo satisfacción significativa
      df_plot <- df %>%
        mutate(T_plot = first(T_promedio))
      
      g <- ggplot(df_plot, aes(x = T_plot, y = Satisfaction.mean)) +
        geom_point(size = 3, color = "blue") +
        geom_path(linewidth = 1.5, color = "blue") +
        geom_text(aes(label = wave), vjust = -1, size = 3.5) +
        labs(title = paste("País:", pais, "| Tipo:", tipo),
             x = "Confianza media", y = "Satisfacción media") +
        theme_minimal(base_size = 12)
      
    } else if (tipo == 3) {
      # Doble significancia
      g <- ggplot(df, aes(x = Trust.mean, y = Satisfaction.mean)) +
        geom_point(size = 3, color = "green") +
        geom_path(linewidth = 1.5, color = "green") +
        geom_text(aes(label = wave), vjust = -1, size = 3.5) +
        labs(title = paste("País:", pais, "| Tipo:", tipo),
             x = "Confianza media", y = "Satisfacción media") +
        theme_minimal(base_size = 12)
    }
    
    ggsave(file.path("Significancia_paises", paste0(pais, ".png")),
           g, width = 5, height = 4, dpi = 300, bg = "white")
  })

#### Normalizacion por el Min ratio Significativo

df_model <- consolidado %>% 
  mutate(
    Trust  = case_when(
      tipo == 2 ~ T_promedio,      # tipo 2: sustituir Trust
      TRUE      ~ Trust.mean
    ),
    Satisfaction = case_when(
      tipo == 1 ~ S_promedio,      # tipo 1: sustituir Satisfaction
      TRUE      ~ Satisfaction.mean
    )
  ) %>% 
  select(countryISO3c, wave, Trust, Satisfaction, tipo)

# 2️⃣ Normalización min-max país por país
df_model_normalizado <- df_model %>% 
  group_by(countryISO3c) %>% 
  mutate(
    Trust_min  = min(Trust, na.rm = TRUE),
    Trust_max  = max(Trust, na.rm = TRUE),
    Satisf_min = min(Satisfaction, na.rm = TRUE),
    Satisf_max = max(Satisfaction, na.rm = TRUE),
    
    Trust_norm  = (Trust  - Trust_min)  / (Trust_max  - Trust_min),
    Satisf_norm = (Satisfaction - Satisf_min) / (Satisf_max - Satisf_min)
  ) %>% 
  ungroup() %>% 
  select(countryISO3c, wave, Trust_norm, Satisf_norm, tipo)


#####

df_delta <- consolidado %>% 
  mutate(
    Trust  = ifelse(tipo == 2, T_promedio, Trust.mean),
    Satisf = ifelse(tipo == 1, S_promedio, Satisfaction.mean)
  ) %>% 
  filter(tipo != 0) %>%                      # fuera los países inertes
  group_by(countryISO3c) %>% 
  mutate(
    T_min   = min(Trust),
    S_min   = min(Satisf),
    Trust_rel  = Trust  / T_min,             # ≥1
    Satisf_rel = Satisf / S_min,
    dT  = Trust_rel  - 1,                    # Δ respecto al mínimo
    dLS = Satisf_rel - 1
  ) %>% 
  ungroup() %>% 
  select(countryISO3c, wave, dT, dLS)

ggplot(df_delta, aes(dT, dLS)) +
  geom_point(alpha = 0.4, colour = "purple") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "Nube de cambios normalizados (todos los países)",
       x = "ΔTrust normalizado", y = "ΔSatisfaction normalizado") +
  theme_minimal(base_size = 13)

### Obtencion del umbral mediante segmented

sl <- slope(m_seg)$dT     # tabla de pendientes por tramo
print(sl)                 # <-- mira qué columnas tiene realmente

# Número de tramos
n.tram <- nrow(sl)

# Umbral
tau_hat <- if ("U.dT" %in% names(coef(m_seg))) coef(m_seg)["U.dT"] else NA
a_hat   <- coef(m_seg)["(Intercept)"]

# Pendiente "posterior": si hay 2+ filas tomamos la última; si solo 1 => única
beta_hat <- sl[n.tram, "Est."]

# Columna de p-value: busca la primera que coincida
p_col <- grep("pr|p", tolower(colnames(sl)), value = TRUE)[1]

p_beta <- if (!is.na(p_col)) sl[n.tram, p_col] else NA

cat("\n--- Resultados segmentados ---\n",
    "Umbral τ̂ =", ifelse(is.na(tau_hat),"NA", round(tau_hat,3)), "\n",
    "Meseta â =", round(a_hat,3), "\n",
    "Pendiente β̂ =", round(beta_hat,3),
    "(p =", ifelse(is.na(p_beta),"NA", signif(p_beta,3)), ")\n")



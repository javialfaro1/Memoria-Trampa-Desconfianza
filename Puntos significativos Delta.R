library(dplyr)
library(purrr)
library(tibble)
library(fixest)

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "WV567.RData"))   # carga objeto WV567

WV567_ecuacion <- WV567 %>%
  select( countryISO3c,wave, DSatisfaction5_6, DSatisfaction6_7, DSatisfaction5_7,DTrust5_6,DTrust6_7,DTrust5_7,DeltaSatisfaction5_6, DeltaSatisfaction6_7, DeltaSatisfaction5_7,DeltaTrust5_6,DeltaTrust6_7,DeltaTrust5_7)

# 1. Construimos el df_puntos a partir de tu WV567_ecuacion
df_puntos <- WV567_ecuacion %>%
  group_by(countryISO3c) %>%
  reframe(
    Comparacion_de_ola = c("5_6", "6_7", "5_7"),
    
    Valor_Trust = c(
      first(DTrust5_6[!is.na(DTrust5_6)]),
      first(DTrust6_7[!is.na(DTrust6_7)]),
      first(DTrust5_7[!is.na(DTrust5_7)])
    ),
    
    Valor_Satisfaction = c(
      first(DSatisfaction5_6[!is.na(DSatisfaction5_6)]),
      first(DSatisfaction6_7[!is.na(DSatisfaction6_7)]),
      first(DSatisfaction5_7[!is.na(DSatisfaction5_7)])
    ),
    
    DeltaTrust = c(
      first(DeltaTrust5_6[!is.na(DTrust5_6)]),
      first(DeltaTrust6_7[!is.na(DTrust6_7)]),
      first(DeltaTrust5_7[!is.na(DTrust5_7)])
    ),
    
    DeltaSatisfaction = c(
      first(DeltaSatisfaction5_6[!is.na(DSatisfaction5_6)]),
      first(DeltaSatisfaction6_7[!is.na(DSatisfaction6_7)]),
      first(DeltaSatisfaction5_7[!is.na(DSatisfaction5_7)])
    )
  ) %>%
  filter(!is.na(Valor_Trust) & !is.na(Valor_Satisfaction)) %>%
  rename(Pais = countryISO3c) %>%
  arrange(Pais, Comparacion_de_ola)

# 2. Creamos la clasificación de significancia:
df_puntos <- df_puntos %>%
  mutate(
    Clasificacion = case_when(
      DeltaTrust == 1 & DeltaSatisfaction == 1 ~ "Ambas significativas",
      DeltaTrust == 1 & DeltaSatisfaction == 0 ~ "Solo confianza significativa",
      DeltaTrust == 0 & DeltaSatisfaction == 1 ~ "Solo satisfacción significativa",
      DeltaTrust == 0 & DeltaSatisfaction == 0 ~ "Ninguna significativa"
    )
  )


# Puntos significativos para el estudio de los cambios en LS (1,1) y (1,0)
puntos_significativos <- df_puntos %>%
  filter(DeltaTrust == 1)

# Determinacion funcion para parametros

ajustar_tramos <- function(dat, n_grid = 30) {
  taus <- seq(min(dat$Valor_Trust), max(dat$Valor_Trust), length.out = n_grid)
  
  res <- map_dfr(taus, function(tau) {
    dat_mod <- dat %>% mutate(S = ifelse(Valor_Trust > tau, Valor_Trust - tau, 0))
    if (all(dat_mod$S == 0)) return(NULL)
    mod <- feols(Valor_Satisfaction ~ S, data = dat_mod, vcov = "HC1")
    tibble(tau = tau,
           beta = coef(mod)[["S"]],
           a    = coef(mod)[["(Intercept)"]],
           SSR  = sum(residuals(mod)^2))
  })
  
  if (nrow(res) == 0) {
    tibble(tau = NA_real_, beta = NA_real_, a = NA_real_, SSR = NA_real_)
  } else {
    res %>%
      filter(SSR == min(SSR, na.rm = TRUE)) %>%
      slice_min(tau, n = 1)
  }
}

# Ajuste de parámetros
parametros_significativos <- puntos_significativos %>%
  group_by(Pais) %>%
  group_split() %>%
  map_dfr(~ {
    out <- ajustar_tramos(.x)
    out$Pais <- first(.x$Pais)
    out
  }) %>%
  select(Pais, tau, beta, a) %>%
  arrange(Pais)

# 🔧 Nuevo: Filtramos solo los parámetros que lograron ser estimados (sin NA)
parametros_validos <- parametros_significativos %>%
  filter(!is.na(tau) & !is.na(beta) & !is.na(a))

# Unimos solo los válidos con los puntos
df_plot <- puntos_significativos %>%
  inner_join(parametros_validos, by = "Pais")


# Creamos la carpeta para guardar los gráficos
dir.create("graficos_ecuacion_general(puntos significativos)", showWarnings = FALSE)

# Graficamos por país
library(ggplot2)
library(purrr)

# Creamos la carpeta para guardar los gráficos
dir.create("graficos_ecuacion_general(puntos significativos)", showWarnings = FALSE)

# Graficamos por país

df_plot <- df_plot %>%
  mutate(orden_ola = case_when(
    Comparacion_de_ola == "5_6" ~ 1,
    Comparacion_de_ola == "5_7" ~ 2,
    Comparacion_de_ola == "6_7" ~ 3
  ))

# Graficamos por país
graficar_pais <- function(dat) {
  
  # Definir tau formateado
  tau_val <- round(unique(dat$tau), 3)
  
  # Orden temporal (por si luego necesitas)
  dat <- dat %>%
    mutate(orden_ola = case_when(
      Comparacion_de_ola == "5_6" ~ 1,
      Comparacion_de_ola == "5_7" ~ 2,
      Comparacion_de_ola == "6_7" ~ 3
    )) %>%
    arrange(orden_ola)
  
  p <- ggplot(dat, aes(x = Valor_Trust, y = Valor_Satisfaction)) +
    geom_point(size = 3, colour = "steelblue") +  # Puntos observados
    geom_text(aes(label = Comparacion_de_ola), vjust = -1, size = 4) +  # Etiquetas de ola
    geom_vline(aes(xintercept = tau), linetype = "dashed", colour = "grey40") +  # Umbral τ
    annotate("text", x = unique(dat$tau), y = max(dat$Valor_Satisfaction, na.rm = TRUE), 
             label = paste0("τ = ", tau_val), hjust = -0.1, vjust = -0.5, size = 4) +  # Mostrar τ
    stat_function(
      fun = function(x) ifelse(x <= unique(dat$tau),
                               unique(dat$a),
                               unique(dat$a) + unique(dat$beta)*(x - unique(dat$tau))),
      colour = "firebrick", linewidth = 1
    ) +
    labs(title = paste0(dat$Pais[1], " – modelo por tramos"),
         x = "Δ Confianza", y = "Δ Bienestar subjetivo") +
    theme_minimal()
  
  return(p)
}

df_plot %>%
  group_by(Pais) %>%
  group_split() %>%
  walk(function(d) {
    g <- graficar_pais(d)
    
    ggsave(
      filename = file.path("graficos_ecuacion_general(puntos significativos)", paste0(d$Pais[1], "_ecuacion_general.png")),
      plot = g,
      width = 7,
      height = 5,
      dpi = 300
    )
  })


ultimos <- df_puntos %>%
  mutate(ord_ola = case_when(
    Comparacion_de_ola == "5_6" ~ 1,
    Comparacion_de_ola == "5_7" ~ 2,
    Comparacion_de_ola == "6_7" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(ord_ola)) %>%
  group_by(Pais) %>%
  slice_max(order_by = ord_ola, with_ties = FALSE) %>%
  ungroup() %>%
  select(Pais, Delta_T_final = Valor_Trust)

parametros <- parametros %>%
  left_join(ultimos, by = "Pais") %>%
  mutate(Clasificacion = case_when(
    is.na(tau) | is.na(beta) | is.na(Delta_T_final) ~ "No clasificable",
    Delta_T_final < tau                            ~ "Trampa",
    Delta_T_final >= tau & beta > 0                ~ "Salida de la trampa",
    Delta_T_final >= tau & beta <= 0               ~ "Empeoramiento"
  ))


















    
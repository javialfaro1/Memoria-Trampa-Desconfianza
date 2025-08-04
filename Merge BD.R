
library(readxl)
library(dplyr)

library(dplyr)
library(scales)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(writexl)


TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"

# Cargar datos
load(paste0(TreatedDataPath, "consolidado_puntos_sig.RData"))
pwt_data <- read_excel(paste0(TreatedDataPath, "PENN WORLD TABLE.xlsx"))

consolidado$Trust.mean <- consolidado$Trust.mean * 100
consolidado$T_promedio <- consolidado$T_promedio * 100

# Filtrar países y años
paises_wvs <- unique(consolidado$countryISO3c)
pwt_data <- pwt_data %>%
  filter(countrycode %in% paises_wvs, year >= 2005, year <= 2022) %>%
  mutate(wave = case_when(
    year %in% 2005:2009 ~ 5,
    year %in% 2010:2014 ~ 6,
    year %in% 2017:2022 ~ 7,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(wave))

# Selección de variables relevantes para Zak & Knack
pwt_data <- pwt_data %>%
  select(
    countrycode, year, wave,
    rgdpe,         # PIB per cápita  
    csh_i,          # inversión sobre PIB
    pl_i,           # precios relativos de bienes de inversión
    hc,             # capital humano
    pop             # población
  )

# Promediar por wave y país
pwt_data <- pwt_data %>%
  group_by(countrycode, wave) %>%
  summarise(
    pib_per_capita        = mean(rgdpe, na.rm = TRUE),
    log_pib_per_capita    = log(mean(rgdpe, na.rm = TRUE)),
    inversion_sobre_pib   = mean(csh_i, na.rm = TRUE),
    precio_inv_relativo   = mean(pl_i, na.rm = TRUE),
    capital_humano        = mean(hc, na.rm = TRUE),
    poblacion_media       = mean(pop, na.rm = TRUE),
    log_poblacion         = log(mean(pop, na.rm = TRUE)),
    .groups = "drop"
  )

# Guardar base PWT tratada
save(pwt_data, file = paste0(TreatedDataPath, "pwt_data.RData"))

# Hacer merge con datos WVS
consolidado <- consolidado %>%
  rename(countrycode = countryISO3c) %>%
  mutate(wave = as.numeric(as.character(wave)))

base_final <- inner_join(consolidado, pwt_data, by = c("countrycode", "wave"))

base_final <- base_final %>%
  select(-tipo, -S_promedio, -T_promedio)

base_final <- base_final %>%
  mutate(
    log_trust = log(Trust.mean),
    log_satisfaction = log(Satisfaction.mean),
    clasificación = if_else(log_trust < 3.25, "Pre t", "Post t")
  )

df_pre_t <- base_final %>% filter(clasificación == "Pre t")
df_post_t <- base_final %>% filter(clasificación == "Post t")


# Guardar base final para análisis econométrico
save(base_final, file = paste0(TreatedDataPath, "base_final.RData"))


######################################################################################

modelo_base1 <- lm(Satisfaction.mean ~ Trust.mean + log_pib_per_capita + capital_humano + inversion_sobre_pib + precio_inv_relativo , data = base_final)
modelo_base2 <- lm(Satisfaction.mean ~ Trust.mean + log_pib_per_capita + capital_humano, data = base_final)
modelo_base3 <- lm(Satisfaction.mean ~ Trust.mean + log_pib_per_capita, data = base_final)
modelo_base4 <- lm(Satisfaction.mean ~ Trust.mean, data = base_final)

summary(modelo_base1)
summary(modelo_base2)
summary(modelo_base3)
summary(modelo_base4)

######################################################
modelo_base_pib <- lm(log_pib_per_capita ~ Trust.mean + capital_humano + inversion_sobre_pib + precio_inv_relativo , data = base_final)
modelo_base_ch <- lm( capital_humano ~ Trust.mean + inversion_sobre_pib + precio_inv_relativo + log_pib_per_capita , data = base_final)
modelo_base_inv <- lm(inversion_sobre_pib ~ Trust.mean + precio_inv_relativo + log_pib_per_capita + capital_humano , data = base_final)

summary(modelo_base_pib)
summary(modelo_base_ch)
summary(modelo_base_inv)


#######################################################

modelo_base1_ <- lm(log_satisfaction ~ log_trust + log_pib_per_capita + capital_humano + inversion_sobre_pib + precio_inv_relativo , data = base_final)
modelo_base2_ <- lm(log_satisfaction ~ log_trust + log_pib_per_capita + capital_humano, data = base_final)
modelo_base3_ <- lm(log_satisfaction ~ log_trust + log_pib_per_capita, data = base_final)
modelo_base4_ <- lm(log_satisfaction ~ log_trust, data = base_final)

summary(modelo_base1_)
summary(modelo_base2_)
summary(modelo_base3_)
summary(modelo_base4_)






########################################################################################

modelo_interaccion <- lm(Satisfaction.mean ~ Trust.mean * log_pib_per_capita +
                           inversion_sobre_pib + capital_humano + log_poblacion,
                         data = base_final)

summary(modelo_interaccion)




base_final <- base_final %>%
  mutate(grupo_pib = ifelse(pib_per_capita > median(pib_per_capita, na.rm = TRUE), "Alto", "Bajo"))

modelo_alto <- lm(Satisfaction.mean ~ Trust.mean + log_pib_per_capita, data = filter(base_final, grupo_pib == "Alto"))
modelo_bajo <- lm(Satisfaction.mean ~ Trust.mean + log_pib_per_capita, data = filter(base_final, grupo_pib == "Bajo"))


summary(modelo_alto)
summary(modelo_bajo)


################################################################################



# Generar tabla en formato LaTeX
stargazer(modelo_base1, modelo_base2, modelo_base3, modelo_base4,
          type = "latex",
          title = "Resultados de regresiones lineales para satisfacción con la vida",
          column.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),
          dep.var.caption = "Variable dependiente: Satisfacción promedio",
          covariate.labels = c("Confianza", "Log PIB per cápita", "Capital humano", "Inversión / PIB", "Precio relativo de inversión"),
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          label = "tab:modelos_satisfaccion")


# Cargar paquete
library(stargazer)

# Exportar modelos a LaTeX
stargazer(modelo_base1_, modelo_base2_, modelo_base3_, modelo_base4_,
          type = "latex",
          title = "Resultados de regresiones con satisfacción en logaritmo",
          column.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),
          dep.var.caption = "Variable dependiente: log(Satisfacción promedio)",
          covariate.labels = c("log(Confianza)", 
                               "log(PIB per cápita)", 
                               "Capital humano", 
                               "Inversión / PIB", 
                               "Precio relativo de inversión"),
          digits = 3,
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          label = "tab:modelos_log_satisfaccion")



library(stargazer)

stargazer(modelo_base_pib, modelo_base_ch, modelo_base_inv,
          type = "latex",
          title = "Modelos explicativos del PIB per cápita, capital humano e inversión",
          column.labels = c("PIB per cápita", "Capital humano", "Inversión / PIB"),
          dep.var.caption = "Variable dependiente",
          covariate.labels = c("Confianza", 
                               "Capital humano", 
                               "Inversión / PIB", 
                               "Precio relativo de inversión",
                               "log(PIB per cápita)"),
          digits = 3,
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          label = "tab:modelos_macro")


library(purrr)

taus <- seq(from = min(base_final$log_trust, na.rm = TRUE),
            to   = max(base_final$log_trust, na.rm = TRUE),
            by   = 0.05)

chow_test_logpib <- function(data, cut){
  d <- data %>% mutate(grupo = if_else(log_trust > cut, "post", "pre"))
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
  
  m_pre  <- lm(log_pib_per_capita ~ log_trust, data = d_pre)
  m_post <- lm(log_pib_per_capita ~ log_trust, data = d_post)
  m_pool <- lm(log_pib_per_capita ~ log_trust, data = d)
  ft <- anova(m_pool, lm(log_pib_per_capita ~ log_trust * grupo, data = d))
  
  tibble(
    tau = cut,
    n_pre = nrow(d_pre),
    n_post = nrow(d_post),
    slope_pre = coef(m_pre)["log_trust"],
    slope_post = coef(m_post)["log_trust"],
    F_chow = ft$`F`[2],
    p_value = ft$`Pr(>F)`[2]
  )
}

resultados_chow_logpib <- map_dfr(taus, ~chow_test_logpib(base_final, .x))


ggplot(resultados_chow_logpib, aes(x = tau, y = p_value)) +
  geom_line(color = "gray30") +
  geom_point(aes(color = p_value < 0.05), size = 2) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                     labels = c("TRUE" = "Significativo", "FALSE" = "No significativo"),
                     name = "p-value < 0.05") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "green", linewidth = 0.6) +
  annotate("text",
           x = min(resultados_chow_logpib$tau, na.rm = TRUE) + 0.05,
           y = 0.06, label = "P_value = 0.05", color = "black", size = 3, hjust = 0) +
  scale_x_continuous(
    limits = c(min(resultados_chow_logpib$tau, na.rm = TRUE),
               max(resultados_chow_logpib$tau, na.rm = TRUE)),
    breaks = pretty(resultados_chow_logpib$tau, n = 5)
  ) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(title = "Test de Chow: log(PIB per cápita) vs log(Confianza)",
       x = "Tau (Corte en log(confianza))",
       y = "p-value del test de Chow") +
  theme_minimal(base_size = 12)



# Visualiza los mejores candidatos
print(resultados_chow_logpib, n = Inf)

mejor_tau <- resultados_chow_logpib %>%
  filter(!is.na(p_value)) %>%
  slice_min(p_value, n = 1)

tau_optimo <- mejor_tau$tau[1]
print(tau_optimo)

# También puedes fijarlo manualmente si quieres:3.0615908
tau_optimo <- 2.0615908
tau_optimo <- 2.1115908
tau_optimo <- 3.0615908


# Separar los datos según el mejor corte
df_pre  <- base_final %>% filter(log_trust <= tau_optimo)
df_post <- base_final %>% filter(log_trust > tau_optimo)

# Ajustar modelos lineales
modelo_pre  <- lm(log_pib_per_capita ~ log_trust, data = df_pre)
modelo_post <- lm(log_pib_per_capita ~ log_trust, data = df_post)

summary(modelo_pre)
summary(modelo_post)

# Mostrar ecuaciones
coef_pre <- coef(modelo_pre)
coef_post <- coef(modelo_post)

cat("Recta pre-quiebre:   log_pib =", round(coef_pre[1], 3), "+", round(coef_pre[2], 3), "* log_trust\n")
cat("Recta post-quiebre:  log_pib =", round(coef_post[1], 3), "+", round(coef_post[2], 3), "* log_trust\n")

# Segmentación explícita para graficar
df_segmentado <- base_final %>%
  mutate(segmento = ifelse(log_trust <= tau_optimo, "pre", "post"))

# Gráfico segmentado de la relación confianza - PIB
ggplot(df_segmentado, aes(x = log_trust, y = log_pib_per_capita)) +
  geom_point(aes(color = segmento), alpha = 0.7) +
  geom_vline(xintercept = tau_optimo, linetype = "dashed", color = "black") +
  
  geom_smooth(data = df_pre, method = "lm", se = FALSE, color = "turquoise3") +
  geom_smooth(data = df_post, method = "lm", se = FALSE, color = "indianred") +
  
  scale_color_manual(values = c("pre" = "turquoise3", "post" = "indianred")) +
  
  labs(
    title = paste0("Segmentación por test de Chow (τ = ", round(tau_optimo, 2), " )"),
    x = "log(Confianza)",
    y = "log(PIB per cápita)",
    color = "Segmento"
  ) +
  theme_minimal(base_size = 12)


#Posicion Paises en el umbral

library(ggplot2)
library(dplyr)

# Paso 1: Definir el umbral
tau_optimo <- 3.25 

# Paso 2: Crear columna de clasificación y etiquetas
df_segmentado <- base_final %>%
  mutate(
    segmento = ifelse(log_trust <= tau_optimo, "Pre τ", "Post τ"),
    etiqueta = paste0(countrycode, " - W", wave)
  )

# Paso 3: Graficar confianza vs satisfacción
ggplot(df_segmentado, aes(x = log_trust, y = log_satisfaction)) +
  geom_point(aes(color = segmento), alpha = 0.8, size = 2.5) +
  geom_vline(xintercept = tau_optimo, linetype = "dashed", color = "black", linewidth = 0.8) +
  
  # Etiquetas por país y ola
  geom_text(aes(label = etiqueta), size = 3, vjust = -1, check_overlap = TRUE) +
  
  scale_color_manual(values = c("Pre τ" = "steelblue", "Post τ" = "firebrick")) +
  labs(
    title = paste0("Segmentación por log(Confianza) con τ = ", tau_optimo),
    subtitle = "Relación entre log(Confianza) y log(Satisfacción)",
    x = "log(Confianza)",
    y = "log(Satisfacción)",
    color = "Segmento"
  ) +
  theme_minimal(base_size = 13)

# Suponiendo que ya tienes df_segmentado creado como:
df_segmentado <- base_final %>%
  mutate(segmento = ifelse(log_trust > 3.25, "Post τ", "Pre τ"),
  etiqueta = paste0(countrycode, " - W", wave))

# Crear tabla resumen
tabla_resumen <- df_segmentado %>%
  mutate(grupo = ifelse(log_trust > 3.25, "Post τ", "Pre τ")) %>%
  group_by(countrycode, grupo) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = n, values_fill = 0) %>%
  arrange(desc(`Post τ`))  # Opcional: ordena por más "Post τ"




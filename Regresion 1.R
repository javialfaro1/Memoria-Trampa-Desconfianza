TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "WV567_Filtrados.RData"))   # carga objeto WV567

promedios_por_pais <- WV567 %>%
  group_by(countryISO3c) %>%
  summarize(
    avg_trust = mean(Trust.mean, na.rm = TRUE),
    avg_satisfaction = mean(Satisfaction.mean, na.rm = TRUE),
    avg_fairness = mean(Fairness.mean, na.rm = TRUE)
  )

promedios_por_pais <- promedios_por_pais %>%
  mutate(Fairness.mean = replace_na(Fairness.mean, 0))
paises_trampa <- c("AUS", "GHA", "HKG", "LBN", "TUN") 
paises_variantes <- c("AND", "CHN", "DEU", "KOR", "TWN")

# Clasificación
promedios_por_pais <- promedios_por_pais %>%
  mutate(
    clasificacion = case_when(
      countryISO3c %in% paises_trampa ~ "TRAMPA",
      countryISO3c %in% paises_variantes ~ "VARIANTE",
      TRUE ~ "NINGUNO"
    ),
    clasificacion = factor(clasificacion, levels = c("NINGUNO", "TRAMPA", "VARIANTE"))
  )

# Ahora corre la regresión
regresion_total <- lm(avg_satisfaction ~ avg_trust + avg_fairness + clasificacion, data = promedios_por_pais)
coeftest(regresion_total, vcov = vcovHC(regresion_total, type = "HC3"))
# Y ves los resultados
summary(regresion_total)

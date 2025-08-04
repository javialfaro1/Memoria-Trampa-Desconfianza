library(dplyr)
library(tidyr)
library(lmtest)      # para la función coeftest()
library(sandwich)

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "WV567.RData"))   # carga objeto WV567

WV567 <- WV567 %>% select(countryISO3c, Trust.mean, Satisfaction.mean, Fairness.mean)
WV567 <- WV567 %>%
  mutate(Fairness.mean = replace_na(Fairness.mean, 0))

paises_trampa <- c("AUS", "GHA", "HKG", "LBN", "TUN") 
paises_variantes <- c("AND", "CHN", "DEU", "KOR", "TWN")

# Clasificación
WV567 <- WV567 %>%
  mutate(
    clasificacion = case_when(
      countryISO3c %in% paises_trampa ~ "TRAMPA",
      countryISO3c %in% paises_variantes ~ "VARIANTE",
      TRUE ~ "NINGUNO"
    ),
    clasificacion = factor(clasificacion, levels = c("NINGUNO", "TRAMPA", "VARIANTE"))
  )


regresion_total <- lm(Satisfaction.mean ~ Trust.mean + Fairness.mean + clasificacion, data = WV567)
coeftest(regresion_total, vcov = vcovHC(regresion_total, type = "HC3"))
summary(regresion_total)

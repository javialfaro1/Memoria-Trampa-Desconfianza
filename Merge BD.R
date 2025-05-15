# Merge PWT con WVS.

library(readxl)

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "WV567_Merge.RData"))   # carga objeto WV567
pwt_data <- read_excel(paste0(TreatedDataPath, "PENN WORLD TABLE.xlsx"))

# Obtener los códigos de países únicos en WV567
paises_wvs <- unique(WV567$countryISO3c)

# Filtrar la base de datos PWT a esos países
pwt_data <- pwt_data %>%
  filter(countrycode %in% paises_wvs,
         year >= 2005, year <= 2022)

pwt_data <- pwt_data %>%
  mutate(wave = case_when(
    year >= 2005 & year <= 2009 ~ 5,
    year >= 2010 & year <= 2014 ~ 6,
    year >= 2017 & year <= 2022 ~ 7,
    TRUE ~ NA_real_  # si el año no cae en ningún rango
  ))

pwt_data <- pwt_data %>%
  filter(!is.na(wave))

pwt_data <- pwt_data %>%
  select(
    countrycode, year, wave,        # identificadores clave
    rgdpna,                          # PIB real per cápita
    ccon,                            # Consumo real per cápita
    pl_con,                          # Nivel de precios de consumo (proxy IPC)
    pl_gdpo,                         # Nivel de precios del PIB
    csh_i,                           # Inversión como % del PIB
    hc,                              # Capital humano                           # Apertura económica
    pop                              # Población
  )

pwt_data <- pwt_data %>%
  group_by(countrycode, wave) %>%
  summarise(
    pib_per_capita         = mean(rgdpna, na.rm = TRUE),
    inversion_sobre_pib    = mean(csh_i, na.rm = TRUE),
    capital_humano         = mean(hc, na.rm = TRUE),
    .groups = "drop"
  )
save(pwt_data, file = paste0(TreatedDataPath, "pwt_data.RData"))

WV567 <- WV567 %>%
  rename(countrycode = countryISO3c)

WV567$wave <- as.numeric(as.character(WV567$wave))
pwt_data$wave <- as.numeric(pwt_data$wave)

base_final <- inner_join(WV567, pwt_data, by = c("countrycode", "wave"))
save(base_final, file = paste0(TreatedDataPath, "base_final.RData"))

regresion_total <- lm(Satisfaction.mean ~ Trust.mean + Fairness.mean + clasificacion + pib_per_capita + inversion_sobre_pib + capital_humano, data = base_final)
summary(regresion_total)




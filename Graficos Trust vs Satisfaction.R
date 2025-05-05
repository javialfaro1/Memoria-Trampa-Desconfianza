#Graficps Confianza vs Satisfacción

rm(list=ls())

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste(TreatedDataPath,"WV567_Filtrados.RData",sep=""))

WV567 <- WV567 %>%
  filter(complete.cases(DTrust5_6, DTrust6_7, DTrust5_7))

library(ggplot2)

ggplot(data = WV567, aes(x = Trust.mean, y = Satisfaction.mean, color = as.factor(wave), label = countryISO3c)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(vjust = -0.5, size = 3, check_overlap = TRUE) +
  scale_color_brewer(palette = "Set1", name = "Wave") +
  labs(
    title = "Confianza vs. Satisfacción Waves",
    x = " Promedio Confianza",
    y = " Promedio Satisfacción"
  ) +
  theme_minimal()


WV567_filtrados <- WV567 %>%
  select(countryISO3c, wave, Trust.mean, Satisfaction.mean)
write.xlsx(WV567_filtrados, file = "WV567_medias.xlsx")

paises_trampa <- WV567 %>%
  filter(
    (DeltaTrust5_6 == 1 & DeltaSatisfaction5_6 == 0 & DTrust5_6 > 0) |
      (DeltaTrust6_7 == 1 & DeltaSatisfaction6_7 == 0 & DTrust6_7 > 0) |
      (DeltaTrust5_7 == 1 & DeltaSatisfaction5_7 == 0 & DTrust5_7 > 0)
  ) %>%
  distinct(countryISO3c)

print(paises_trampa)



WV567 <- WV567 %>%
  mutate(
    trampa = case_when(
      (DeltaTrust5_6 == 1 & DeltaSatisfaction5_6 == 0 & DTrust5_6 > 0) |
        (DeltaTrust6_7 == 1 & DeltaSatisfaction6_7 == 0 & DTrust6_7 > 0) |
        (DeltaTrust5_7 == 1 & DeltaSatisfaction5_7 == 0 & DTrust5_7 > 0) ~ TRUE,
      TRUE ~ FALSE
    )
  )

#para saber la wave
paises_trampa <- WV567 %>%
  mutate(
    wave_trampa = case_when(
      DeltaTrust5_6 == 1 & DeltaSatisfaction5_6 == 0 & DTrust5_6 > 0 ~ "5_6",
      DeltaTrust6_7 == 1 & DeltaSatisfaction6_7 == 0 & DTrust6_7 > 0 ~ "6_7",
      DeltaTrust5_7 == 1 & DeltaSatisfaction5_7 == 0 & DTrust5_7 > 0 ~ "5_7",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(wave_trampa)) %>%
  select(countryISO3c, wave_trampa) %>%
  distinct()

paises_trampa <- WV567 %>%
  mutate(
    wave_trampa = case_when(
      DeltaTrust5_6 == 1 & DeltaSatisfaction5_6 == 0 & DTrust5_6 > 0 ~ "6",
      DeltaTrust6_7 == 1 & DeltaSatisfaction6_7 == 0 & DTrust6_7 > 0 ~ "7",
      DeltaTrust5_7 == 1 & DeltaSatisfaction5_7 == 0 & DTrust5_7 > 0 ~ "7",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(wave_trampa)) %>%
  mutate(wave_trampa = as.numeric(wave_trampa)) %>%
  filter(wave == wave_trampa) %>%  # Filtra la fila específica de esa ola
  select(countryISO3c, wave, Trust.mean, Satisfaction.mean, Fairness.mean)



WV567 <- WV567 %>%
  mutate(
    Invariante = case_when(
      (DeltaTrust5_6 == 1 & DeltaSatisfaction5_6 == 0) |
        (DeltaTrust6_7 == 1 & DeltaSatisfaction6_7 == 0) |
        (DeltaTrust5_7 == 1 & DeltaSatisfaction5_7 == 0) ~ 1,
      TRUE ~ 0
    )
  )

WV567 <- WV567 %>%
  mutate(
    categoria = case_when(
      # Entrampado: confianza varía, es positiva, y satisfacción no mejora
      (DeltaTrust5_6 == 1 & DTrust5_6 > 0 & DeltaSatisfaction5_6 == 0) |
        (DeltaTrust6_7 == 1 & DTrust6_7 > 0 & DeltaSatisfaction6_7 == 0) |
        (DeltaTrust5_7 == 1 & DTrust5_7 > 0 & DeltaSatisfaction5_7 == 0) ~ "entrampado",
      
      # Invariante: según definición por DeltaTrust y DeltaSatisfaction = 0
      Invariante == 1 ~ "invariante",
      
      # Todo lo demás
      TRUE ~ "otro"
    )
  )

library(dplyr)

# Filtra solo países entrampados e invariantes

WV567 <- WV567 %>%
  select(countryISO3c, wave, Trust.mean, Satisfaction.mean, DeltaSatisfaction5_6,DeltaSatisfaction6_7, DeltaSatisfaction5_7, DeltaTrust5_6,DeltaTrust6_7, DeltaTrust5_7)





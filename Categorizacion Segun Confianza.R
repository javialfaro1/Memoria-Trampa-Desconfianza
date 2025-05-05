#Categorización segun Trust (o variables)

rm(list=ls())

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/3. Invariance/"

load(paste(TreatedDataPath,"Invariance_WV5_6.RData",sep=""))
load(paste(TreatedDataPath,"Invariance_WV5_7.RData",sep=""))
load(paste(TreatedDataPath,"Invariance_WV6_7.RData",sep=""))
load(paste(TreatedDataPath,"InvariantCountries_Merge.RData",sep=""))


WV5_6 <- WV5_6[!(WV5_6$countryISO3c %in% c("BFA", "ETH", "MLI", "RWA", "YEM")), ]
WV5_7 <- WV5_7[!(WV5_7$countryISO3c %in% c("BFA", "ETH", "MLI", "RWA", "YEM")), ]
WV6_7 <- WV6_7[!(WV6_7$countryISO3c %in% c("BFA", "ETH", "MLI", "RWA", "YEM")), ]
invariantCountries <- invariantCountries[!(invariantCountries$countryISO3c %in% c("BFA", "ETH", "MLI", "RWA", "YEM")), ]

#Trust 
WV5_6 %>%
  select(countryISO3c, DTrust5_6) %>%
  arrange(DTrust5_6)

WV5_6 %>%
  mutate(countryISO3c = reorder(countryISO3c, DTrust5_6)) %>%
  ggplot(aes(x = countryISO3c, y = DTrust5_6)) +
  geom_point(color = "#FFB997", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cambio en confianza entre olas 5 y 6 (DTrust5_6)",
    x = "País",
    y = "Diferencia de medias en confianza"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

library(dplyr)

# Crear cuartiles
cuartiles <- WV5_6 %>%
  select(countryISO3c, DTrust5_6) %>%
  mutate(Cuartil = ntile(DTrust5_6, 4))  # Divide en 4 grupos del 1 (más bajo) al 4 (más alto))

# Listar países por cuartil
split(cuartiles$countryISO3c, cuartiles$Cuartil)

#Satisfaction
WV5_6 %>%
  select(countryISO3c, DSatisfaction5_6) %>%
  arrange(DSatisfaction5_6)


WV5_6 %>%
  mutate(countryISO3c = reorder(countryISO3c, DSatisfaction5_6)) %>%
  ggplot(aes(x = countryISO3c, y = DSatisfaction5_6)) +
  geom_point(color = "#FFB997", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cambio en Satisfacción entre olas 5 y 6 ",
    x = "País",
    y = "Diferencia de medias en Satisfacción"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

#Trampa Desconfianza 5_6
WV5_6 %>%
  filter(
    DeltaTrust5_6 == 1,
    DTrust5_6 > 0,
    DeltaSatisfaction5_6 == 0
  ) %>%
  select(countryISO3c, DTrust5_6, DSatisfaction5_6)


#5-7
WV5_7 %>%
  select(countryISO3c, DTrust5_7) %>%
  arrange(DTrust5_7)

WV5_7 %>%
  mutate(countryISO3c = reorder(countryISO3c, DTrust5_7)) %>%
  ggplot(aes(x = countryISO3c, y = DTrust5_7)) +
  geom_point(color = "#FFB997", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cambio en confianza entre olas 5 y 7 (DTrust5_7)",
    x = "País",
    y = "Diferencia de medias en confianza"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

WV5_7 %>%
  select(countryISO3c, DSatisfaction5_7) %>%
  arrange(DSatisfaction5_7)


WV5_7 %>%
  mutate(countryISO3c = reorder(countryISO3c, DSatisfaction5_7)) %>%
  ggplot(aes(x = countryISO3c, y = DSatisfaction5_7)) +
  geom_point(color = "#FFB997", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cambio en Satisfacción entre olas 5 y 7 ",
    x = "País",
    y = "Diferencia de medias en Satisfacción"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )
#Trampa desconfianza
WV5_7 %>%
  filter(
    DeltaTrust5_7 == 1,
    DTrust5_7 > 0,
    DeltaSatisfaction5_7 == 0
  ) %>%
  select(countryISO3c, DTrust5_7, DSatisfaction5_7)


#6_7
# Ordenar por confianza
WV6_7 %>%
  select(countryISO3c, DTrust6_7) %>%
  arrange(DTrust6_7)

# Gráfico de confianza
WV6_7 %>%
  mutate(countryISO3c = reorder(countryISO3c, DTrust6_7)) %>%
  ggplot(aes(x = countryISO3c, y = DTrust6_7)) +
  geom_point(color = "#B5EAD7", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cambio en confianza entre olas 6 y 7 (DTrust6_7)",
    x = "País",
    y = "Diferencia de medias en confianza"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

# Ordenar por satisfacción
WV6_7 %>%
  select(countryISO3c, DSatisfaction6_7) %>%
  arrange(DSatisfaction6_7)

# Gráfico de satisfacción
WV6_7 %>%
  mutate(countryISO3c = reorder(countryISO3c, DSatisfaction6_7)) %>%
  ggplot(aes(x = countryISO3c, y = DSatisfaction6_7)) +
  geom_point(color = "#B5EAD7", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Cambio en Satisfacción entre olas 6 y 7",
    x = "País",
    y = "Diferencia de medias en Satisfacción"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

# Trampa de desconfianza ola 6–7
WV6_7 %>%
  filter(
    DeltaTrust6_7 == 1,
    DTrust6_7 > 0,
    DeltaSatisfaction6_7 == 0
  ) %>%
  select(countryISO3c, DTrust6_7, DSatisfaction6_7)

install.packages("openxlsx")   # Solo una vez
library(openxlsx)


invariantCountries %>%
  select(countryISO3c, DTrust5_6, DTrust6_7, DTrust5_7) -> dtrust_filtrado
dtrust_filtrado <- na.omit(dtrust_filtrado)


invariantCountries %>%
  select(countryISO3c, DSatisfaction5_6, DSatisfaction6_7, DSatisfaction5_7) -> dSatisfaction_filtrado
dSatisfaction_filtrado <- na.omit(dSatisfaction_filtrado)
write.xlsx(dSatisfaction_filtrado, file = "DSatisfaction_Evolucion.xlsx")


dtrust_filtrado <- dtrust_filtrado %>%
  mutate(
    Cambio_5_6 = ifelse(DTrust5_6 > 0, "Aumenta", "Disminuye"),
    Cambio_6_7 = ifelse(DTrust6_7 > 0, "Aumenta", "Disminuye"),
    Cambio_5_7 = ifelse(DTrust5_7 > 0, "Aumenta", "Disminuye"),
  )
write.xlsx(dtrust_filtrado, file = "DTrust_Evolucion.xlsx")

dSatisfaction_filtrado <- dSatisfaction_filtrado %>%
  mutate(
    Cambio_5_6 = ifelse(DSatisfaction5_6 > 0, "Aumenta", "Disminuye"),
    Cambio_6_7 = ifelse(DSatisfaction6_7 > 0, "Aumenta", "Disminuye"),
    Cambio_5_7 = ifelse(DSatisfaction5_7 > 0, "Aumenta", "Disminuye"),
  )
write.xlsx(dSatisfaction_filtrado, file = "DSatisfaction_Evolucion.xlsx")

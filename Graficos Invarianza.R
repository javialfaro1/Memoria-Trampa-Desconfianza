#Creacion graficos de invarianza
rm(list=ls())

TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/3. Invariance/"

load(paste(TreatedDataPath,"Invariance_WV5_6.RData",sep=""))
load(paste(TreatedDataPath,"Invariance_WV5_7.RData",sep=""))
load(paste(TreatedDataPath,"Invariance_WV6_7.RData",sep=""))

WV5_6 <- WV5_6[!(WV5_6$countryISO3c %in% c("BFA", "ETH", "MLI", "RWA", "YEM")), ]
WV5_7 <- WV5_7[!(WV5_7$countryISO3c %in% c("BFA", "ETH", "MLI", "RWA", "YEM")), ]
WV6_7 <- WV6_7[!(WV6_7$countryISO3c %in% c("BFA", "ETH", "MLI", "RWA", "YEM")), ]

# Proporción de paises Invariados WAVE 5-6

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)  # Para formatear el eje Y en porcentaje

# Paso 1: Calcular cantidad y proporción
invar_data_56 <- WV5_6 %>%
  summarise(
    Satisfacción = sum(DeltaSatisfaction5_6 == 0),
    Confianza = sum(DeltaTrust5_6 == 0),
    Justicia = sum(DeltaFairness5_6 == 0)
  ) %>%
  pivot_longer(everything(), names_to = "Dimensión", values_to = "Cantidad")

# Paso 2: Total y porcentaje
total_paises <- nrow(WV5_6)
invar_data <- invar_data %>%
  mutate(Porcentaje = Cantidad / total_paises)

colores_ajustados <- c(
  "Satisfacción" = "#A0C4FF",
  "Confianza"    = "#A9D6E5",
  "Justicia"     = "#D0F4DE"
)
# Paso 3: Gráfico final
ggplot(invar_data, aes(x = Dimensión, y = Cantidad, fill = Dimensión)) +
  geom_col() +
  geom_text(aes(label = percent(Porcentaje, accuracy = 1)),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = colores_similares) +
  ylim(0, max(invar_data$Cantidad) + 2) +
  labs(title = "Cantidad países Invariantes WV 5-6",
       y = "Cantidad de países", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

#Wave 5-7

# Paso 1: Calcular cantidad y proporción
invar_data_57 <- WV5_7 %>%
  summarise(
    Satisfacción = sum(DeltaSatisfaction5_7 == 0),
    Confianza = sum(DeltaTrust5_7 == 0),
  ) %>%
  pivot_longer(everything(), names_to = "Dimensión", values_to = "Cantidad")

# Paso 2: Total y porcentaje
total_paises_57 <- nrow(WV5_7)
invar_data_57 <- invar_data_57 %>%
  mutate(Porcentaje = Cantidad / total_paises_57)

# 🎨 Colores pastel ajustados
colores_ajustados <- c(
  "Satisfacción" = "#A0C4FF",  # celeste más intenso
  "Confianza"    = "#A9D6E5",  # azul verdoso pastel
)

# Paso 3: Gráfico final
ggplot(invar_data_57, aes(x = Dimensión, y = Cantidad, fill = Dimensión)) +
  geom_col() +
  geom_text(aes(label = percent(Porcentaje, accuracy = 1)),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = colores_ajustados) +
  ylim(0, max(invar_data_57$Cantidad) + 2) +
  labs(title = "Cantidad de países con invarianza entre olas 5 y 7",
       y = "Cantidad de países", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# wave 6-7
# Paso 1: Calcular cantidad y proporción

invar_data_67 <- WV6_7 %>%
  summarise(
    Satisfacción = sum(DeltaSatisfaction6_7 == 0),
    Confianza = sum(DeltaTrust6_7 == 0),
  ) %>%
  pivot_longer(everything(), names_to = "Dimensión", values_to = "Cantidad")

# Paso 2: Total y porcentaje
total_paises_67 <- nrow(WV6_7)
invar_data_67 <- invar_data_67 %>%
  mutate(Porcentaje = Cantidad / total_paises_67)

# 🎨 Colores pastel ajustados
colores_ajustados <- c(
  "Satisfacción" = "#A0C4FF",  # celeste pastel
  "Confianza"    = "#A9D6E5",  # azul verdoso pastel
)

# Paso 3: Gráfico final
ggplot(invar_data_67, aes(x = Dimensión, y = Cantidad, fill = Dimensión)) +
  geom_col() +
  geom_text(aes(label = percent(Porcentaje, accuracy = 1)),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = colores_ajustados) +
  ylim(0, max(invar_data_67$Cantidad) + 2) +
  labs(title = "Cantidad de países con invarianza entre olas 6 y 7",
       y = "Cantidad de países", x = "") +
  theme_minimal() +
  theme(legend.position = "none")




#Variacion de las variables 5-6

library(ggplot2)
library(dplyr)
library(tidyr)

# Paso 1: Preparar los datos con nombres legibles
data_heatmap <- WV5_6 %>%
  mutate(
    Orden = DeltaTrust5_6,
    countryISO3c = as.character(countryISO3c)
  ) %>%
  arrange(Orden, countryISO3c) %>%
  select(countryISO3c,
         Satisfacción = DeltaSatisfaction5_6,
         Confianza = DeltaTrust5_6,
         Justicia = DeltaFairness5_6) %>%
  pivot_longer(-countryISO3c, names_to = "Variable", values_to = "Cambio") %>%
  mutate(Cambio = factor(Cambio, levels = c(0, 1), labels = c("No", "Sí")))

# Paso 2: Reordenar países por cambio en confianza
orden_paises <- WV5_6 %>%
  arrange(DeltaTrust5_6, countryISO3c) %>%
  pull(countryISO3c)

data_heatmap$countryISO3c <- factor(data_heatmap$countryISO3c, levels = orden_paises)

# Paso 3: Orden personalizado de variables con Confianza al centro
data_heatmap$Variable <- factor(data_heatmap$Variable,
                                levels = c("Satisfacción", "Confianza", "Justicia"))

# Paso 4: Crear el heatmap
ggplot(data_heatmap, aes(x = Variable, y = countryISO3c, fill = Cambio)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("No" = "#E0E0E0", "Sí" = "#BFD7FF")) +
  labs(
    title = "Heatmap binario de cambios entre oleadas 5 y 6 ",
    x = "Variables",
    y = "País",
    fill = "¿Hubo cambio?"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 7)
  )

# 5-7 
library(ggplot2)
library(dplyr)
library(tidyr)

# Paso 1: Preparar los datos sin la columna de justicia
data_heatmap <- WV5_7 %>%
  mutate(
    Orden = DeltaTrust5_7,
    countryISO3c = as.character(countryISO3c)
  ) %>%
  arrange(desc(Orden), countryISO3c) %>%  # ¡Invertido para que los de trust 1 estén arriba!
  select(countryISO3c,
         Satisfacción = DeltaSatisfaction5_7,
         Confianza = DeltaTrust5_7) %>%
  pivot_longer(-countryISO3c, names_to = "Variable", values_to = "Cambio") %>%
  mutate(Cambio = factor(Cambio, levels = c(0, 1), labels = c("No", "Sí")))

# Paso 2: Reordenar países (los que tienen cambio en confianza quedan arriba)
orden_paises <- WV5_7 %>%
  arrange(DeltaTrust5_7, countryISO3c) %>%
  pull(countryISO3c)

data_heatmap$countryISO3c <- factor(data_heatmap$countryISO3c, levels = orden_paises)

# Paso 3: Asegurar orden con confianza al centro (aunque hay solo 2)
data_heatmap$Variable <- factor(data_heatmap$Variable, levels = c("Satisfacción", "Confianza"))

# Paso 4: Graficar
ggplot(data_heatmap, aes(x = Variable, y = countryISO3c, fill = Cambio)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("No" = "#F0F0F0", "Sí" = "#FFB997")) +  # colores para wave 5–7
  labs(
    title = "Heatmap binario de cambios entre oleadas 5 y 7 ",
    x = "Variable",
    y = "País",
    fill = "¿Hubo cambio?"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 5)
  )


#6-7

library(ggplot2)
library(dplyr)
library(tidyr)

# Paso 1: Preparar los datos SIN la columna de justicia
data_heatmap67 <- WV6_7 %>%
  mutate(
    Orden = DeltaTrust6_7,
    countryISO3c = as.character(countryISO3c)
  ) %>%
  arrange(Orden, countryISO3c) %>%
  select(countryISO3c,
         Satisfacción = DeltaSatisfaction6_7,
         Confianza = DeltaTrust6_7) %>%
  pivot_longer(-countryISO3c, names_to = "Variable", values_to = "Cambio") %>%
  mutate(Cambio = factor(Cambio, levels = c(0, 1), labels = c("No", "Sí")))

# Paso 2: Reordenar países por cambio en confianza
orden_paises <- WV6_7 %>%
  arrange(DeltaTrust6_7, countryISO3c) %>%
  pull(countryISO3c)

data_heatmap$countryISO3c <- factor(data_heatmap$countryISO3c, levels = orden_paises)

# Paso 3: Reordenar variables con confianza al centro (aunque ahora hay solo 2)
data_heatmap$Variable <- factor(data_heatmap$Variable, levels = c("Confianza", "Satisfacción"))

# Paso 4: Crear el gráfico
ggplot(data_heatmap, aes(x = Variable, y = countryISO3c, fill = Cambio)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("No" = "#E0E0E0", "Sí" = "#84A59D")) +
  labs(
    title = "Heatmap binario de cambios entre oleadas 6 y 7 (sin justicia)",
    x = "Variable",
    y = "País (ordenado por cambio en confianza)",
    fill = "¿Hubo cambio?"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 6)
  )





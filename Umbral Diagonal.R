##############################################################################
# ░░ BLOQUE COMPLETO ░░ Entrada fija + Tabla + Gráficos mejorados
##############################################################################
TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "WV567_Filtrados.RData"))   # carga objeto WV567

rm(list = setdiff(ls(), "WV567"))
library(dplyr) ; library(stringr) ; library(purrr)
library(ggplot2); library(writexl)

# ── 1. Filtrar países relevantes ───────────────────────────────────────────
WV567_final <- WV567 %>% 
  select(countryISO3c, wave, Trust.mean, Satisfaction.mean,
         DeltaSatisfaction5_6, DeltaSatisfaction6_7, DeltaSatisfaction5_7,
         DeltaTrust5_6,       DeltaTrust6_7,       DeltaTrust5_7) %>% 
  group_by(countryISO3c) %>% 
  filter(!all(across(starts_with("Delta"), ~ (.x == 0 | is.na(.x))))) %>% 
  ungroup()

# ── 2. Helper: identificar par doble significancia ─────────────────────────
doble_sig_pair <- function(row){
  for(p in c("5_6","6_7","5_7")){
    if(isTRUE(row[[paste0("DeltaTrust",p)]] == 1) &&
       isTRUE(row[[paste0("DeltaSatisfaction",p)]] == 1)){
      return(as.integer(str_split(p, "_")[[1]]))
    }
  }
  integer(0)
}

# ── 3. Construir tabla de puntos (T1,S1) y (T2,S2) ─────────────────────────
intervalos_validos <- WV567_final %>%
  group_split(countryISO3c) %>%
  map_dfr(function(tbl){
    row   <- tbl[1, ]
    pair  <- doble_sig_pair(row)
    if(length(pair) == 0) return(NULL)
    
    tbl <- tbl %>% mutate(wave = as.integer(as.character(wave)))
    
    P1 <- tbl %>% filter(wave == pair[1]) %>%
      summarise(T1 = first(Trust.mean), S1 = first(Satisfaction.mean))
    P2 <- tbl %>% filter(wave == pair[2]) %>%
      summarise(T2 = first(Trust.mean), S2 = first(Satisfaction.mean))
    
    if (nrow(P1) == 0 || nrow(P2) == 0) return(NULL)
    
    tibble(country = row$countryISO3c,
           ola_1 = pair[1],
           ola_2 = pair[2],
           T1 = P1$T1, S1 = P1$S1,
           T2 = P2$T2, S2 = P2$S2)
  })


# ── 4. Guardar tabla usada ─────────────────────────────────────────────────
write_xlsx(intervalos_validos, "intervalos_doble_sig.xlsx")
save(intervalos_validos,       file = "intervalos_doble_sig.RData")
cat("Tablas guardadas: .xlsx .csv .RData\n")

# ── 5. Graficar cada país con etiquetas (número de ola + valores) ──────────
dir.create("plots_doble", showWarnings = FALSE)

pwalk(intervalos_validos,
      function(country, ola_1, ola_2, T1, S1, T2, S2){
        
        x_pad <- max(0.20 * abs(T2 - T1), 0.01)
        y_pad <- max(0.20 * abs(S2 - S1), 0.01)
        
        g <- ggplot() +
          geom_segment(aes(x = T1, y = S1, xend = T2, yend = S2),
                       colour = "forestgreen", linewidth = 1.8) +
          geom_point(aes(c(T1, T2), c(S1, S2)), size = 3) +
          
          # ─── número de ola encima
          geom_text(aes(c(T1, T2), c(S1, S2)),
                    label = c(ola_1, ola_2),
                    vjust = -1.2, fontface = "bold", size = 3.5) +
          
          # ─── valores (T,S) debajo
          geom_text(aes(c(T1, T2), c(S1, S2)),
                    label = sprintf("(%.3f , %.3f)", c(T1, T2), c(S1, S2)),
                    vjust = 1.8, size = 3.2, colour = "gray30") +
          
          scale_x_continuous(limits = c(min(T1, T2) - x_pad,
                                        max(T1, T2) + x_pad)) +
          scale_y_continuous(limits = c(min(S1, S2) - y_pad,
                                        max(S1, S2) + y_pad)) +
          labs(title    = paste("País:", country,
                                "– par", ola_1, "→", ola_2),
               subtitle = "ΔTrust = ΔSatisfacción = 1 en ese par",
               x = "Confianza media", y = "Satisfacción media") +
          theme_minimal(base_size = 12)
        
        ggsave(file.path("plots_doble", paste0(country, ".png")),
               g, width = 5, height = 4, dpi = 300, bg = "white")
      })

cat("¡Gráficos guardados en plots_doble/!\n")


##############################################################################
# ░░░  ENTRADA FIJA – tal como la pediste  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
##############################################################################
TreatedDataPath <- "C:/Users/Javiera/OneDrive - miuandes.cl/Documentos/Universidad/DistrustTrap/Bases Datos WVS/4. Merge WV567/"
load(paste0(TreatedDataPath, "WV567_Filtrados.RData"))   # crea objeto WV567

rm(list = setdiff(ls(), "WV567"))
library(dplyr) ; library(stringr) ; library(writexl)

WV567_clasificacion <- WV567 %>% 
  select(countryISO3c, wave, Trust.mean, Satisfaction.mean,
         DeltaSatisfaction5_6, DeltaSatisfaction6_7, DeltaSatisfaction5_7,
         DeltaTrust5_6,       DeltaTrust6_7,       DeltaTrust5_7)

WV567_final <- WV567_clasificacion %>% 
  group_by(countryISO3c) %>% 
  filter(!all(DeltaSatisfaction5_6 == 0,
              DeltaSatisfaction6_7 == 0,
              DeltaSatisfaction5_7 == 0,
              DeltaTrust5_6 == 0,
              DeltaTrust6_7 == 0,
              DeltaTrust5_7 == 0)) %>% 
  ungroup()

##############################################################################
# 1) helper – olas con Δ==1 (NA se trata como 0) -----------------------------
##############################################################################
sig_waves <- function(row, pref){
  names(row) %>% 
    str_subset(paste0("^Delta", pref)) %>% 
    keep(~ isTRUE(row[[.x]] == 1)) %>%          # NA → FALSE
    str_extract_all("\\d") %>% unlist() %>% as.integer() %>% unique()
}

##############################################################################
# 2)  casos “solo confianza” (tipo1)  o  “solo satisfacción” (tipo2) ---------
##############################################################################
solo_tabla <- WV567_final %>% 
  group_split(countryISO3c) %>% 
  purrr::map_dfr(function(tbl){
    row <- tbl[1, ]
    wT  <- sig_waves(row, "Trust")
    wS  <- sig_waves(row, "Satisfaction")
    
    # ───────────  SOLO CONFIANZA: tipo 1  ──────────────────────────────────
    if(length(wT) > 0 && length(wS) == 0){
      T_sig <- tbl %>% filter(wave %in% wT) %>% pull(Trust.mean)        %>% na.omit()
      S_sig <- tbl %>% filter(wave %in% wT) %>% pull(Satisfaction.mean) %>% na.omit()
      tibble(country = row$countryISO3c,
             tipo    = 1,
             T_min   = min(T_sig),     # intervalo variante
             T_max   = max(T_sig),
             S_prom  = mean(S_sig))    # promedio invariante
    }
    # ───────────  SOLO SATISFACCIÓN: tipo 2  ──────────────────────────────
    else if(length(wS) > 0 && length(wT) == 0){
      S_sig <- tbl %>% filter(wave %in% wS) %>% pull(Satisfaction.mean) %>% na.omit()
      T_sig <- tbl %>% filter(wave %in% wS) %>% pull(Trust.mean)        %>% na.omit()
      tibble(country = row$countryISO3c,
             tipo    = 2,
             S_min   = min(S_sig),
             S_max   = max(S_sig),
             T_prom  = mean(T_sig))
    } else {
      NULL          # descarta países donde cambian ambas variables o ninguna
    }
  })

write_xlsx(solo_tabla, "solo_conf_o_satisf.xlsx")
solo_tabla

library(ggplot2)
dir.create("plots_solo", showWarnings = FALSE)

# asegurarnos de que las columnas existan (por si alguna quedó ausente)
solo_tabla <- solo_tabla %>% 
  mutate(
    S_prom = ifelse(tipo == 1, S_prom, NA_real_),
    T_prom = ifelse(tipo == 2, T_prom, NA_real_)
  )

purrr::pwalk(
  list(country = solo_tabla$country,
       tipo    = solo_tabla$tipo,
       T_min   = solo_tabla$T_min,
       T_max   = solo_tabla$T_max,
       S_min   = solo_tabla$S_min,
       S_max   = solo_tabla$S_max,
       S_prom  = solo_tabla$S_prom,
       T_prom  = solo_tabla$T_prom),
  function(country, tipo, T_min, T_max, S_min, S_max, S_prom, T_prom){
    
    if(tipo == 1){                  # ── solo confianza  → horizontal (rojo)
      g <- ggplot() +
        geom_segment(aes(x = T_min, y = S_prom,
                         xend = T_max, yend = S_prom),
                     colour = "red", linewidth = 1.8) +
        geom_point(aes(c(T_min, T_max), c(S_prom, S_prom)), size = 3) +
        labs(title = paste("País:", country),
             subtitle = "Solo confianza significativa",
             x = "Confianza media", y = "Satisfacción media") +
        theme_minimal(base_size = 12)
      
    } else {                        # ── solo satisfacción → vertical (azul)
      g <- ggplot() +
        geom_segment(aes(x = T_prom, y = S_min,
                         xend = T_prom, yend = S_max),
                     colour = "blue", linewidth = 1.8) +
        geom_point(aes(c(T_prom, T_prom), c(S_min, S_max)), size = 3) +
        labs(title = paste("País:", country),
             subtitle = "Solo satisfacción significativa",
             x = "Confianza media", y = "Satisfacción media") +
        theme_minimal(base_size = 12)
    }
    
    ggsave(file.path("plots_solo", paste0(country, ".png")),
           g, width = 5, height = 4, dpi = 300, bg = "white")
  }
)

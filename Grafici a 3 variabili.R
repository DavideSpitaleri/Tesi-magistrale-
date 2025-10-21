library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

dati <- read_excel("M:/Tesi/Dati tesi modified.xlsx", sheet = "Foglio1")

summary(dati)
str(dati)  # Controlla la struttura del dataset

# Convertiamo le colonne che contengono percentuali o numeri come caratteri in numeri
dati$`FG%` <- as.numeric(dati$`FG%`)
dati$`3P%` <- as.numeric(dati$`3P%`)
dati$`FT%` <- as.numeric(dati$`FT%`)

# Rimuoviamo gli eventuali caratteri (ad esempio 'min' o altri simboli) per altre colonne
dati$`MP` <- as.numeric(gsub("min", "", dati$`MP`))  # Minuti di gioco
dati$`PTS` <- as.numeric(dati$`PTS`)  # Punti
dati$`RB` <- as.numeric(dati$`RB`)  # Rimbalzi
dati$`AST` <- as.numeric(dati$`AST`)  # Assisti
dati$`WS` <- as.numeric(dati$`WS`)  # Win Shares
dati$`WS/48` <- as.numeric(dati$`WS/48`)  # Win Shares per 48 minuti
dati$`BPM` <- as.numeric(dati$`BPM`)  # Box Plus-Minus
dati$`VORP` <- as.numeric(dati$`VORP`)  # Value Over Replacement Player
dati$PLAYER <- as.factor(dati$PLAYER)
dati$AFFILATION <- as.factor(dati$AFFILATION)
dati$TEAM <- as.factor(dati$TEAM)
dati$YEAR <- as.factor(dati$YEAR)

str(dati)  # Controlla la struttura aggiornata del dataset
summary(dati)
# -------------------------
# Grafici a 3 varaibili
# -------------------------
# Conta quanti giocatori provengono da ciascun college
table(dati$AFFILATION)

# Seleziona solo i college con più di 10 giocatori
dati_sub <- dati[which(dati$AFFILATION %in% names(which(table(dati$AFFILATION) > 10))), ]

# Visualizzazione: relazione tra posizione al draft e durata della carriera NBA
ggplot(dati_sub, aes(x = `OVERALL PICK`, y = `NBA SEASON`)) +
  geom_point(alpha = 0.5, color = "blue") +  # Punti per ogni giocatore
  geom_smooth(method = "lm", color = "red", size = 1) +  # Regressione lineare
  facet_wrap(~AFFILATION) +  # Un grafico per ogni college
  labs(title = "Durata della carriera NBA in relazione alla posizione al draft",
       x = "Posizione al Draft (Overall Pick)",
       y = "Durata della carriera NBA (anni)") +
  theme_minimal()

ggplot(dati, aes(x = 'OVERALL PICK', y = 'NBA SEASON', col=YEAR)) +
  geom_point(alpha = 0.5) +  # Punti blu per ogni giocatore
  
  labs(title = "Durata della carriera NBA in relazione alla posizione al draft",
       x = "Posizione al Draft (Overall Pick)",
       y = "Durata della carriera NBA (anni)") +
  theme_minimal()

#dividere dal 2000/2008; 2009/2016; 2017/2024
ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, col = YEAR)) +
  geom_line() +
  labs(title = "Durata della carriera NBA in relazione alla posizione al draft",
       x = "Posizione al Draft (Overall Pick)",
       y = "Durata della carriera NBA (anni)") +
  theme_minimal()

# Assicurati che YEAR sia numerico
dati$YEAR <- as.numeric(as.character(dati$YEAR))

# Crea la colonna per le 3 fasce temporali
dati <- dati %>%
  mutate(ANNATA = case_when(
    YEAR >= 2000 & YEAR <= 2008 ~ "2000-2008",
    YEAR >= 2009 & YEAR <= 2016 ~ "2009-2016",
    YEAR >= 2017 & YEAR <= 2024 ~ "2017-2024",
    TRUE ~ NA_character_
  ))

# Ordina la variabile ANNATA per mantenere l’ordine nei facet
dati$ANNATA <- factor(dati$ANNATA, levels = c("2000-2008", "2009-2016", "2017-2024"))

# Grafico: una linea per ogni anno, divisa in 3 pannelli (ANNATA)
ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, group = YEAR, color = factor(YEAR))) +
  geom_line(alpha = 0.6, size = 1) +
  facet_wrap(~ ANNATA, nrow = 1) +
  labs(
    title = "Durata della carriera NBA in relazione alla posizione al draft",
    subtitle = "Linee per ogni anno di draft, suddivise in 3 fasce temporali",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata della carriera NBA (anni)",
    color = "Anno di Draft"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

dati_media <- dati %>%
  group_by(ANNATA, `OVERALL PICK`) %>%
  summarise(NBA_SEASON = mean(`NBA SEASON`, na.rm = TRUE), .groups = "drop")

ggplot(dati_media, aes(x = `OVERALL PICK`, y = NBA_SEASON, color = ANNATA)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("2000-2008" = "#1b9e77", "2009-2016" = "#d95f02", "2017-2024" = "#7570b3")) +
  labs(
    x = "Posizione al Draft",
    y = "Durata media carriera NBA (anni)",
    color = "ANNATA"
  ) +
  theme_minimal(base_size = 14)+
  theme(axis.title = element_text(size = 15, face= "bold" ),  
        axis.text = element_text(size = 12, color = "black"),  
        axis.ticks.x = element_blank())

# Prima converti YEAR in numerico
dati$YEAR <- as.numeric(as.character(dati$YEAR))

# Poi fai la colonna ANNATA
dati <- dati %>%
  mutate(ANNATA = case_when(
    YEAR >= 2000 & YEAR <= 2008 ~ "2000-2008",
    YEAR >= 2009 & YEAR <= 2016 ~ "2009-2016",
    YEAR >= 2017 & YEAR <= 2024 ~ "2017-2024",
    TRUE ~ NA_character_
  ))

ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, color = ANNATA)) +
  geom_line() +
  labs(
    title = "Durata della carriera NBA in relazione alla posizione al draft",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata della carriera NBA (anni)",
    color = "ANNATA"
  ) +
  theme_minimal()

# Assicurati che YEAR sia numerico
dati$YEAR <- as.numeric(as.character(dati$YEAR))

# Crea la colonna per fasce di annate
dati <- dati %>%
  mutate(ANNATA = case_when(
    YEAR >= 2000 & YEAR <= 2008 ~ "2000–2008",
    YEAR >= 2009 & YEAR <= 2016 ~ "2009–2016",
    YEAR >= 2017 & YEAR <= 2024 ~ "2017–2024",
    TRUE ~ NA_character_
  ))

# Aggrega: media della durata per overall pick e annata
dati_agg <- dati %>%
  group_by(ANNATA, `OVERALL PICK`) %>%
  summarise(NBA_SEASON = mean(`NBA SEASON`, na.rm = TRUE), .groups = "drop")

# Grafico con linee e facet per annata
ggplot(dati_agg, aes(x = `OVERALL PICK`, y = NBA_SEASON, color = ANNATA, group = ANNATA)) +
  geom_line(size = 1) +
  facet_wrap(~ ANNATA, nrow = 1) +
  scale_color_manual(values = c(
    "2000–2008" = "#1b9e77",
    "2009–2016" = "#d95f02",
    "2017–2024" = "#7570b3"
  ))+
  labs(
    x = "Posizione al Draft",
    y = "Durata media carriera NBA (anni)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines"),
    axis.title = element_text(size = 15, face= "bold"),  
    axis.text = element_text(size = 12, color = "black"))



# -------------------------
#Grafico dove metto in relazione Nba season ed overall pick per vedere se effettivamente le prime scelte del draft sono quelle che giocano più anni in nba
# -------------------------
library(ggplot2)
library(viridis)

ggplot(dati_sub, aes(x = `OVERALL PICK`, y = `NBA SEASON`, color = `AFFILATION`)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_d(option = "plasma", direction = -1, name = "Durata carriera (anni)") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  scale_x_reverse(breaks = seq(0, 60, by = 5)) +  # Prima scelta a sinistra
  labs(
    title = "🏀 Carriera NBA e Posizione al Draft",
    subtitle = "Le prime scelte giocano più a lungo? Analisi dei dati storici",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata della carriera NBA (anni)",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    plot.caption = element_text(size = 10, face = "italic")
  )

library(ggplot2)
library(viridis)
library(ggrepel)

breaks_x <- c(1:10, seq(15, 60, by = 5))

ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, 
                 color = `NBA SEASON`, size = G)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(option = "", direction = -1, name = "Durata carriera (anni)") +
  scale_size(range = c(2, 8), name = "Partite Giocate") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  geom_text_repel(data = subset(dati, `NBA SEASON` > 15), 
                  aes(label = PLAYER), size = 3, max.overlaps = 10) +
  scale_x_reverse(breaks = breaks_x) +
  labs(
    title = "🏀 Durata carriera NBA vs Posizione al Draft",
    subtitle = "Dimensione punti = partite giocate; etichette per carriere > 15 anni",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata carriera NBA (anni)",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    plot.caption = element_text(size = 10, face = "italic")
  )

breaks_x <- c(1:10, seq(15, 60, by = 5))

ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, 
                 color = `NBA SEASON`, size = G)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(option = "cividis", direction = -1, name = "Durata carriera (anni)") +
  scale_size(range = c(2, 8), name = "Partite Giocate") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  geom_text_repel(
    data = subset(dati, `NBA SEASON` > 15),
    aes(label = PLAYER),
    size = 3,
    max.overlaps = 10,
    segment.color = "grey50",
    box.padding = 1,        # aumenta spazio attorno all’etichetta
    point.padding = 0.6,    # aumenta spazio tra etichetta e punto
    force = 2,              # aumenta forza per allontanare le etichette
    min.segment.length = 0, # sempre mostra le linee di connessione
    direction = "both",     # lascia muovere le etichette sia verticalmente che orizzontalmente
    nudge_y = 0.5           # sposta un po’ le etichette verso l’alto
  ) +
  scale_x_reverse(breaks = breaks_x) +
  labs(
    title = "🏀 Durata carriera NBA vs Posizione al Draft",
    subtitle = "Dimensione punti = partite giocate; etichette per carriere > 15 anni",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata carriera NBA (anni)",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    plot.caption = element_text(size = 10, face = "italic")
  )

library(ggplot2)
library(RColorBrewer)
library(ggrepel)

breaks_x <- c(1:10, seq(15, 60, by = 5))

ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, 
                 color = `NBA SEASON`, size = G)) +
  geom_point(alpha = 0.7) +
  scale_color_gradientn(
    colors = brewer.pal(9, "BuPu"),
    name = "Durata carriera (anni)"
  ) +
  scale_size(range = c(2, 8), name = "Partite Giocate") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  geom_text_repel(
    data = subset(dati, `NBA SEASON` > 15),
    aes(label = PLAYER),
    size = 5,
    max.overlaps = 10,
    segment.color = "black",
    box.padding = 1,
    point.padding = 0.8,
    force = 2,
    min.segment.length = 0,
    direction = "both",
    nudge_y = 0.5
  ) +
  scale_x_reverse(breaks = breaks_x) +
  labs(
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata carriera NBA (anni)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 14, color = "#2c3e50"),
  )

library(ggplot2)
library(viridis)
library(ggrepel)

# -------------------------
#Grafico per vedere se effettivamente i giocatori scelti nelle prime posizioni sono quelli che giocano per più stagioni e hanno anche le statistiche migliori
# -------------------------
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)

# Prepariamo i dati in formato lungo (long format)
dati_long <- dati %>%
  select(`OVERALL PICK`, `NBA SEASON`, TPTS, TAST, TRB) %>%
  rename(`Nba Season` = `NBA SEASON`,
         `Punti Totali` = TPTS,
         `Assist Totali` = TAST,
         `Rimbalzi Totali` = TRB) %>%
  pivot_longer(
    cols = c(`Nba Season`, `Punti Totali`, `Assist Totali`, `Rimbalzi Totali`),
    names_to = "Statistica",
    values_to = "Valore"
  )

dati_long$Statistica <- factor(
  dati_long$Statistica,
  levels = c("Nba Season", "Punti Totali", "Assist Totali", "Rimbalzi Totali")
)
# Breaks personalizzati per l'asse X
breaks_x <- c(seq(0, 60, by = 5))

# Grafico finale local polinomial regression7 spiegare cos è la linea rossa
ggplot(dati_long, aes(x = `OVERALL PICK`, y = Valore)) +
  geom_jitter(width = 0.6, alpha = 0.2, color = "slateblue") +  # punti leggeri
  geom_smooth(method = "loess", se = FALSE, color = "gray38", size = 1.2) +  # linea tendenza
  facet_wrap(~Statistica, scales = "free_y", ncol = 2) +  # un pannello per ogni statistica
  scale_x_reverse(breaks = breaks_x) +
  labs(
    x = "Posizione al Draft (Overall Pick)",
    y = "Valore",
    ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_text(color = "black", size= 12),
    axis.title = element_text(size = 15, face= "bold"),
    panel.grid.minor = element_blank()
  )

#versione 2 divido scelte da 1 a 10/ da 11 a 30/ da 31 a 60
# Creiamo due gruppi: Prime 10 scelte vs Altri
dati_grouped <- dati %>%
  mutate(Draft_Group = ifelse(`OVERALL PICK` <= 10, "Top 10 Pick", "Pick 11-60")) %>%
  select(Draft_Group, `NBA SEASON`, TPTS, TAST, TRB) %>%
  rename(`Durata NBA` = `NBA SEASON`,
         `Punti Totali` = TPTS,
         `Assist Totali` = TAST,
         `Rimbalzi Totali` = TRB) %>%
  pivot_longer(cols = -Draft_Group, names_to = "Statistica", values_to = "Valore")

# Boxplot
ggplot(dati_grouped, aes(x = Draft_Group, y = Valore, fill = Draft_Group)) +
  geom_boxplot(outlier.alpha = 0.1, width = 0.6) +
  facet_wrap(~Statistica, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("#1f77b4", "#fca311")) +
  labs(
    title = "📦 Produzione dei Top 10 Pick vs Altri",
    subtitle = "Confronto di carriera e statistiche principali",
    x = "",
    y = "Valore",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40")
  )

#versione 3
dati_long <- dati %>%
  mutate(Draft_Group = ifelse(`OVERALL PICK` <= 10, "Top 10 Pick", "Pick 11-60")) %>%
  select(Draft_Group, `OVERALL PICK`, `NBA SEASON`, TPTS, TAST, TRB) %>%
  rename(
    `Durata NBA` = `NBA SEASON`,
    `Punti Totali` = TPTS,
    `Assist Totali` = TAST,
    `Rimbalzi Totali` = TRB
  ) %>%
  pivot_longer(cols = c(`Durata NBA`, `Punti Totali`, `Assist Totali`, `Rimbalzi Totali`),
               names_to = "Statistica",
               values_to = "Valore")

ggplot(dati_long, aes(x = `OVERALL PICK`, y = Valore, color = Draft_Group)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  scale_x_reverse(breaks = c(1:10, seq(15, 60, 5))) +
  facet_wrap(~Statistica, scales = "free_y") +
  scale_color_manual(values = c("Top 10 Pick" = "#1f77b4", "Pick 11-60" = "#fca311")) +
  labs(
    title = "📊 Statistiche NBA in funzione della posizione al draft",
    subtitle = "Confronto tra Top 10 e Pick 11–60 su 4 metriche",
    x = "Posizione al Draft (Overall Pick)",
    y = "Valore",
    color = "Gruppo Draft",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

# -------------------------
#heatmap
# -------------------------
# 1) Calcolo medie per OVERALL PICK fino a 60
dati_heatmap <- dati %>%
  filter(`OVERALL PICK` <= 60) %>%
  group_by(`OVERALL PICK`) %>%
  summarise(
    NBA_SEASON = mean(`NBA SEASON`, na.rm = TRUE),
    TPTS = mean(TPTS, na.rm = TRUE),
    TRB = mean(TRB, na.rm = TRUE),
    TAST = mean(TAST, na.rm = TRUE)
  )

# 2) Normalizzazione (0-1) per confronto visivo
dati_heatmap_norm <- dati_heatmap %>%
  mutate(across(c(NBA_SEASON, TPTS, TRB, TAST), ~ scales::rescale(.))) %>%
  pivot_longer(-`OVERALL PICK`, names_to = "Statistica", values_to = "Valore_Normalizzato") %>%
mutate(
  ColoreTesto = ifelse(Valore_Normalizzato > 0.6, "white", "black")
)

# 3) Heatmap 
ggplot(dati_heatmap_norm, aes(x = Statistica, y = factor(`OVERALL PICK`, levels = rev(unique(`OVERALL PICK`))), fill = Valore_Normalizzato)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "BuPu"),   values = scales::rescale(c(0, 1)), name = "Valore Normalizzato") +
  geom_text(aes(label = sprintf("%.2f", Valore_Normalizzato), color = ColoreTesto), size = 3, fontface = "bold") +
  scale_fill_gradientn( +
  labs(
    title = "🔥 Heatmap delle Prestazioni Normalizzate per Posizione al Draft (1-60)",
    subtitle = "Durata carriera e statistiche principali: punti, rimbalzi, assist",
    x = NULL,
    y = "Posizione al Draft",
    ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 22, color = "#4b0082"),
    plot.subtitle = element_text(size = 15, color = "#6a5acd", margin = margin(b = 15)),
    axis.text.y = element_text(size = 9, color = "#4b0082"),
    axis.text.x = element_text(size = 13, face = "bold", color = "#4b0082"),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

   # 1) Calcolo medie per OVERALL PICK fino a 60
  dati_heatmap <- dati %>%
    filter(`OVERALL PICK` <= 60) %>%
    group_by(`OVERALL PICK`) %>%
    summarise(
      NBA_SEASON = mean(`NBA SEASON`, na.rm = TRUE),
      TPTS = mean(TPTS, na.rm = TRUE),
      TRB = mean(TRB, na.rm = TRUE),
      TAST = mean(TAST, na.rm = TRUE)
    )
  
  # 2) Normalizzazione (0-1) per confronto visivo e aggiunta colore testo
  dati_heatmap_norm <- dati_heatmap %>%
    mutate(across(c(NBA_SEASON, TPTS, TRB, TAST), ~ scales::rescale(.))) %>%
    pivot_longer(-`OVERALL PICK`, names_to = "Statistica", values_to = "Valore_Normalizzato") %>%
    mutate(
      Statistica = recode(Statistica,
                          "NBA_SEASON" = "Nba Season",
                          "TPTS" = "Punti Totali",
                          "TAST" = "Assist Totali",
                          "TRB" = "Rimbalzi Totali"
      ),
      Statistica = factor(Statistica, levels = c(
        "Nba Season", "Punti Totali", "Assist Totali", "Rimbalzi Totali"
      )),
           ColoreTesto = ifelse(Valore_Normalizzato > 0.6, "white", "black")
    )
  
  # 3) Heatmap
  ggplot(dati_heatmap_norm, aes(x = Statistica, y = factor(`OVERALL PICK`, levels = rev(unique(`OVERALL PICK`))), fill = Valore_Normalizzato)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Valore_Normalizzato), color = ColoreTesto), size = 3, fontface = "bold") +
    scale_fill_gradientn(
      colors = brewer.pal(9, "BuPu"),
      values = scales::rescale(c(0, 1)),
      name = "Valore Normalizzato"
    ) +
    scale_color_identity() +
    labs(
      x = NULL,
      y = "Posizione al Draft"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 22, color = "#4b0082"),
      plot.subtitle = element_text(size = 15, color = "#6a5acd", margin = margin(b = 15)),
      axis.text.y = element_text(size = 9, color = "#4b0082"),
      axis.text.x = element_text(size = 13, face = "bold", color = "#4b0082"),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.position = "right",
      legend.title = element_text(size = 14, face = "bold"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )

  # 1) Calcolo medie per OVERALL PICK fino a 60
  dati_heatmap <- dati %>%
    filter(`OVERALL PICK` <= 60) %>%
    group_by(`OVERALL PICK`) %>%
    summarise(
      NBA_SEASON = mean(`NBA SEASON`, na.rm = TRUE),
      TPTS = mean(TPTS, na.rm = TRUE),
      TRB = mean(TRB, na.rm = TRUE),
      TAST = mean(TAST, na.rm = TRUE)
    )
  
  # 2) Normalizzazione + etichette + colori custom
  dati_heatmap_norm <- dati_heatmap %>%
    mutate(across(c(NBA_SEASON, TPTS, TRB, TAST), scales::rescale)) %>%
    pivot_longer(-`OVERALL PICK`, names_to = "Statistica", values_to = "Valore_Normalizzato") %>%
    mutate(
      Statistica = recode(Statistica,
                          "NBA_SEASON" = "Nba Season",
                          "TPTS" = "Punti Totali",
                          "TAST" = "Assist Totali",
                          "TRB" = "Rimbalzi Totali"
      ),
      Statistica = factor(Statistica, levels = c(
        "Nba Season", "Punti Totali", "Assist Totali", "Rimbalzi Totali"
      )),
      
      # Calcolo colore in base alla Statistica
      FillColor = case_when(
        Statistica == "Nba Season" ~ scales::col_numeric(palette = c("#f8bbd0", "#880e4f"), domain = c(0, 1))(Valore_Normalizzato),
        Statistica == "Punti Totali" ~ scales::col_numeric(palette = c("#fddddd", "#8b0000"), domain = c(0, 1))(Valore_Normalizzato),
        Statistica == "Assist Totali" ~ scales::col_numeric(palette = c("#ffe0b2", "#ff8c00"), domain = c(0, 1))(Valore_Normalizzato),
        Statistica == "Rimbalzi Totali" ~ scales::col_numeric(palette = c("#fff8dc", "#b8860b"), domain = c(0, 1))(Valore_Normalizzato),
        TRUE ~ "#CCCCCC"
      ),
      
      # Colore testo leggibile
      ColoreTesto = ifelse(Valore_Normalizzato > 0.6, "white", "black")
    )
  
  # 3) Heatmap
  ggplot(dati_heatmap_norm, aes(
    x = Statistica,
    y = factor(`OVERALL PICK`, levels = rev(unique(`OVERALL PICK`)))
  )) +
    geom_tile(aes(fill = FillColor), color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Valore_Normalizzato), color = ColoreTesto),
              size = 3, fontface = "bold") +
    scale_fill_identity() +
    scale_color_identity() +
    labs(
      x = NULL,
      y = "Posizione al Draft"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 22, color = "#4b0082"),
      plot.subtitle = element_text(size = 15, color = "#6a5acd", margin = margin(b = 15)),
      axis.text.y = element_text(size = 9, color = "#4b0082"),
      axis.text.x = element_text(size = 13, face = "bold", color = "#4b0082"),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.position = "none",  # non serve, ogni colonna ha scala propria
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )  
  
# -------------------------
#Le prime scelte al Draft giocano più anni e segnano di più?
# -------------------------
ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, color = TPTS)) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_viridis_c(option = "plasma", name = "Punti Totali") +
  scale_x_reverse(breaks = c(1:10, seq(15, 60, 5))) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  labs(
    title = "🔥 Le prime scelte al Draft giocano più anni e segnano di più?",
    subtitle = "Posizione al Draft (asse X) vs durata carriera NBA (asse Y)\nColori indicano punti totali segnati",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata carriera NBA (anni)",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e", margin = margin(b = 15)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2c3e50"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )

dati_sub2= dati[which(dati$`NBA SEASON` >5),]
ggplot(dati_sub2, aes(x = `OVERALL PICK`, y = `NBA SEASON`, color = TPTS)) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_viridis_c(option = "plasma", name = "Punti Totali") +
  scale_x_reverse(breaks = c(1:10, seq(15, 60, 5))) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  labs(
    title = "🔥 Le prime scelte al Draft giocano più anni e segnano di più?",
    subtitle = "Posizione al Draft (asse X) vs durata carriera NBA (asse Y)\nColori indicano punti totali segnati",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata carriera NBA (anni)",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e", margin = margin(b = 15)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2c3e50"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )

dati_sub2= dati[which(dati$`NBA SEASON` >5),]
ggplot(dati_sub2, aes(x = `OVERALL PICK`, y = `NBA SEASON`, color = PTS)) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_viridis_c(option = "plasma", name = "Punti Totali") +
  scale_x_reverse(breaks = c(1:10, seq(15, 60, 5))) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  labs(
    title = "🔥 Le prime scelte al Draft giocano più anni e segnano di più?",
    subtitle = "Posizione al Draft (asse X) vs durata carriera NBA (asse Y)\nColori indicano punti totali segnati",
    x = "Posizione al Draft (Overall Pick)",
    y = "Durata carriera NBA (anni)",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e", margin = margin(b = 15)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2c3e50"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )


# -------------------------
#Confronto statistiche per gruppi di posizione al Draft 
# -------------------------
dati <- dati %>%
  mutate(DraftGroup = case_when(
    `OVERALL PICK` <= 10 ~ "Top 10",
    `OVERALL PICK` <= 30 ~ "Mid 11-30",
    TRUE ~ "Late >30"
  ))

dati_long <- dati %>%
  select(DraftGroup, TPTS, TRB, TAST,`NBA SEASON`) %>%
  pivot_longer(cols = c(TPTS, TRB, TAST,`NBA SEASON`), names_to = "Statistica", values_to = "Valore")

ggplot(dati_long, aes(x = DraftGroup, y = Valore, fill = DraftGroup)) +
  geom_boxplot(alpha = 0.85, outlier.color = "#e74c3c", outlier.size = 2) +
  facet_wrap(~ Statistica, scales = "free_y") +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "📊 Confronto statistiche per gruppi di posizione al Draft",
    subtitle = "Top 10, Medio (11-30) e Tardive (>30)",
    x = "Gruppo Draft",
    y = "Valore Statistica",
    ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e", margin = margin(b = 15)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 13, color = "#2c3e50"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 16, color = "#2c3e50"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )

# Creazione gruppo Draft
dati <- dati %>%
  mutate(DraftGroup = case_when(
    `OVERALL PICK` <= 10 ~ "1-10",
    `OVERALL PICK` <= 30 ~ "11-30",
    TRUE ~ "31-60"
  ))


# Ristrutturazione dati e assegnazione colori + nomi statistiche
dati_long <- dati %>%
  select(DraftGroup, TPTS, TRB, TAST, `NBA SEASON`, `PLAYER`) %>%
  pivot_longer(cols = c(TPTS, TRB, TAST, `NBA SEASON`), names_to = "Statistica", values_to = "Valore") %>%
  mutate(
    Statistica = recode(Statistica,
                        TPTS = "Punti Totali",
                        TRB = "Rimbalzi Totali",
                        TAST = "Assist Totali",
                        `NBA SEASON` = "Nba Season"),
    Statistica = factor(Statistica, levels = c(
      "Nba Season",
      "Punti Totali",
      "Assist Totali",
      "Rimbalzi Totali"
    )),
    Colore = case_when(
      Statistica == "Punti Totali" ~ case_when(
        DraftGroup == "1-10" ~ "firebrick2",
        DraftGroup == "11-30" ~ "firebrick3",
        DraftGroup == "31-60" ~ "firebrick4"
      ),
      Statistica == "Assist Totali" ~ case_when(
        DraftGroup == "1-10" ~ "darkorange2",
        DraftGroup == "11-30" ~ "darkorange3",
        DraftGroup == "31-60" ~ "darkorange4"
      ),
      Statistica == "Rimbalzi Totali" ~ case_when(
        DraftGroup == "1-10" ~ "goldenrod2",
        DraftGroup == "11-30" ~ "goldenrod3",
        DraftGroup == "31-60" ~ "goldenrod4"
      ),
      Statistica == "Nba Season" ~ case_when(
        DraftGroup == "1-10" ~ "orchid2",
        DraftGroup == "11-30" ~ "orchid3",
        DraftGroup == "31-60" ~ "orchid4"
      ),
      TRUE ~ "#CCCCCC"
    )
  )
outliers <- dati_long %>%
  group_by(DraftGroup, Statistica) %>%
  mutate(
    Q1 = quantile(Valore, 0.25, na.rm = TRUE),
    Q3 = quantile(Valore, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier = Valore < (Q1 - 1.5 * IQR) | Valore > (Q3 + 1.5 * IQR)
  ) %>%
  ungroup() %>%
  filter(is_outlier) %>%
  select(DraftGroup, Statistica, Valore, `PLAYER`, Colore)
# Plot
ggplot(dati_long, aes(x = DraftGroup, y = Valore, fill = Colore)) +
  geom_boxplot(alpha = 0.85, outlier.color = "steelblue4", outlier.size = 2) +
  geom_text(
    data = outliers,
    aes(label = PLAYER),
    size = 3.5,
    hjust = -0.1,
    vjust = 0,
    color = "black",
    check_overlap = TRUE
  ) +
   facet_wrap(~ Statistica, scales = "free_y") +
  scale_fill_identity() +
  labs(
    x = "Posizione Draft",
    y = "Valore Statistica"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e", margin = margin(b = 15)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2c3e50"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 16, color = "#2c3e50"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )


# 1. Crea gruppi Draft
dati <- dati %>%
  mutate(DraftGroup = case_when(
    `OVERALL PICK` <= 10 ~ "1-10",
    `OVERALL PICK` <= 30 ~ "11-30",
    TRUE ~ "31-60"
  ))

# 2. Ristruttura dati in formato long + estrai cognome + colori + nomi statistiche
dati_long <- dati %>%
  select(PLAYER, DraftGroup, TPTS, TRB, TAST, `NBA SEASON`) %>%
  pivot_longer(cols = c(TPTS, TRB, TAST, `NBA SEASON`), 
               names_to = "Statistica", values_to = "Valore") %>%
  mutate(
    Cognome = word(PLAYER, -1),  # Estrae il cognome (ultima parola)
    Statistica = recode(Statistica,
                        TPTS = "Punti Totali",
                        TRB = "Rimbalzi Totali",
                        TAST = "Assist Totali",
                        `NBA SEASON` = "Nba Season"),
    Statistica = factor(Statistica, levels = c(
      "Nba Season",
      "Punti Totali",
      "Assist Totali",
      "Rimbalzi Totali"
    )),
    Colore = case_when(
      Statistica == "Punti Totali" ~ case_when(
        DraftGroup == "1-10" ~ "firebrick2",
        DraftGroup == "11-30" ~ "firebrick3",
        DraftGroup == "31-60" ~ "firebrick4"
      ),
      Statistica == "Assist Totali" ~ case_when(
        DraftGroup == "1-10" ~ "darkorange2",
        DraftGroup == "11-30" ~ "darkorange3",
        DraftGroup == "31-60" ~ "darkorange4"
      ),
      Statistica == "Rimbalzi Totali" ~ case_when(
        DraftGroup == "1-10" ~ "goldenrod2",
        DraftGroup == "11-30" ~ "goldenrod3",
        DraftGroup == "31-60" ~ "goldenrod4"
      ),
      Statistica == "Nba Season" ~ case_when(
        DraftGroup == "1-10" ~ "orchid2",
        DraftGroup == "11-30" ~ "orchid3",
        DraftGroup == "31-60" ~ "orchid4"
      ),
      TRUE ~ "#CCCCCC"
    )
  )

# 3. Calcola outlier con metodo IQR
outliers <- dati_long %>%
  group_by(DraftGroup, Statistica) %>%
  mutate(
    Q1 = quantile(Valore, 0.25, na.rm = TRUE),
    Q3 = quantile(Valore, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier = Valore > (Q3 + 1.5 * IQR)  # Solo outlier superiori
  ) %>%
  ungroup() %>%
  filter(is_outlier)

# 4. Seleziona SOLO i top 5 outlier per ogni Statistica e DraftGroup
outliers_top <- outliers %>%
  group_by(Statistica, DraftGroup) %>%
  slice_max(Valore, n = 5, with_ties = FALSE) %>%
  ungroup()

# 5. Plot finale
ggplot(dati_long, aes(x = DraftGroup, y = Valore, fill = Colore)) +
  geom_boxplot(alpha = 0.85, outlier.color = "steelblue4", outlier.size = 2) +
  geom_text(
    data = outliers_top,
    aes(label = Cognome),
    size = 5,
    fontface = "bold",    # 👉 grassetto
    hjust = -0.1,
    vjust = 0,
    color = "black",
    check_overlap = TRUE
  ) +
  facet_wrap(~ Statistica, scales = "free_y") +
  scale_fill_identity() +
  labs(
    x = "Posizione Draft",
    y = "Valore Statistica"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#34495e", margin = margin(b = 15)),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 14, color = "#2c3e50"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 16, color = "#2c3e50"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )
# Statistiche di sintesi per ogni gruppo e statistica
riassunto_boxplot <- dati_long %>%
  group_by(Statistica, DraftGroup) %>%
  summarise(
    min = min(Valore, na.rm = TRUE),
    Q1 = quantile(Valore, 0.25, na.rm = TRUE),
    median = median(Valore, na.rm = TRUE),
    mean = mean(Valore, na.rm = TRUE),
    Q3 = quantile(Valore, 0.75, na.rm = TRUE),
    max = max(Valore, na.rm = TRUE),
    .groups = "drop"
  )

# Visualizza la tabella
riassunto_boxplot





# possibilità di fare test medie appaiati (la media assit primi 10 è uguale alla media assist da 11 a 30)

# -------------------------
#Confronto statistiche per gruppi di posizione al Draft
# -------------------------
dati <- dati %>%
  mutate(DraftGroup = case_when(
    `OVERALL PICK` <= 10 ~ "Top 10",
    `OVERALL PICK` <= 30 ~ "Mid 11-30",
    TRUE ~ "Late >30"
  ))

dati_long <- dati %>%
  select(DraftGroup, TPTS, TRB, TAST) %>%
  pivot_longer(cols = c(TPTS, TRB, TAST), names_to = "Statistica", values_to = "Valore")

ggplot(dati_long, aes(x = DraftGroup, y = Valore, fill = DraftGroup)) +
  geom_boxplot(alpha = 0.9, outlier.colour = "#ff6f61", outlier.size = 3) +
  facet_wrap(~ Statistica, scales = "free_y", ncol = 1) +
  scale_fill_manual(
    values = c("Top 10" = "#ff6f61", "Mid 11-30" = "#6b5b95", "Late >30" = "#88b04b")
  ) +
  labs(
    title = "📊 Confronto statistiche per gruppi di posizione al Draft",
    subtitle = "Top 10, Medio (11-30) e Tardive (>30)",
    x = "Gruppo Draft",
    y = "Valore Statistica",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "#121212"),
    panel.background = element_rect(fill = "#121212"),
    plot.title = element_text(face = "bold", size = 26, color = "white"),
    plot.subtitle = element_text(size = 16, color = "gray90", margin = margin(b = 20)),
    axis.title = element_text(size = 16, face = "bold", color = "white"),
    axis.text = element_text(size = 12, color = "gray90"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 18, color = "white"),
    panel.grid.major = element_line(color = "gray40"),
    panel.grid.minor = element_blank()
  )


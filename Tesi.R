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
# Grafici ad 1 varaibile
# -------------------------
# -------------------------
# Grafici per PTS
# -------------------------
# boxplot
library(ggrepel)
# Calcolo quartili e IQR per definire gli outlier (opzionale, qui serve solo per info)
q1 <- quantile(dati$PTS, 0.25, na.rm = TRUE)
q3 <- quantile(dati$PTS, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_superiore <- q3 + 1.5 * iqr

# Filtra solo outlier con PTS > 25
outlier_pts <- dati %>%
  filter(PTS > 25) %>%
  mutate(x = 1.01)  # spostiamo le etichette a destra del boxplot

# Boxplot con outlier etichettati (solo PTS > 25)
ggplot(dati, aes(x = factor(1), y = PTS)) +
  geom_boxplot(fill = "lightblue") +
  geom_text_repel(data = outlier_pts, aes(x = x, y = PTS, label = PLAYER),
                  color = "red", size = 3, hjust = 0, nudge_x = 0.01) +
  coord_cartesian(xlim = c(0.8, 1.3)) +  # spazio per etichette a destra
  labs(title = "Boxplot - Punti (PTS) con outlier etichettati (PTS > 25)", x = "", y = "PTS") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# istogramma
ggplot(dati, aes(x = PTS)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Istogramma - Punti (PTS)", x = "PTS", y = "Frequenza")

# -------------------------
# Grafici per AST
# -------------------------
# boxplot
# Calcolo quartili e IQR per definire gli outlier (opzionale, qui serve solo per info)
q1 <- quantile(dati$AST, 0.25, na.rm = TRUE)
q3 <- quantile(dati$AST, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_superiore <- q3 + 1.5 * iqr

# Filtra solo outlier con AST > 7.5
outlier_ast <- dati %>%
  filter(AST > 7.5) %>%
  mutate(x = 1.01)  # spostiamo le etichette a destra del boxplot

# Boxplot con outlier etichettati (solo AST > 7.5)
ggplot(dati, aes(x = factor(1), y = AST)) +
  geom_boxplot(fill = "lightblue") +
  geom_text_repel(data = outlier_ast, aes(x = x, y = AST, label = PLAYER),
                  color = "red", size = 3, hjust = 0, nudge_x = 0.01) +
  coord_cartesian(xlim = c(0.8, 1.3)) +  # spazio per etichette a destra
  labs(title = "Boxplot - Assist (AST) con outlier etichettati (AST > 7.5)", x = "", y = "AST") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# istogramma
ggplot(dati, aes(x = AST)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Istogramma - Assist (AST)", x = "AST", y = "Frequenza")

# -------------------------
# Grafici per RB
# -------------------------
# boxplot
# Calcolo quartili e IQR per definire gli outlier (opzionale, qui serve solo per info)
q1 <- quantile(dati$RB, 0.25, na.rm = TRUE)
q3 <- quantile(dati$RB, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_superiore <- q3 + 1.5 * iqr

# Filtra solo outlier con RB > 10
outlier_rb <- dati %>%
  filter(RB > 10) %>%
  mutate(x = 1.02)  # spostiamo le etichette a destra del boxplot

# Boxplot con outlier etichettati (solo RB > 10)
ggplot(dati, aes(x = factor(1), y = RB)) +
  geom_boxplot(fill = "lightblue") +
  geom_text_repel(data = outlier_rb, aes(x = x, y = RB, label = PLAYER),
                  color = "red", size = 3, hjust = 0, nudge_x = 0.01) +
  coord_cartesian(xlim = c(0.8, 1.3)) +  # spazio per etichette a destra
  labs(title = "Boxplot - Rimbalzi (RB) con outlier etichettati (RB > 10)", x = "", y = "RB") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# istogramma
ggplot(dati, aes(x = RB)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Istogramma - Rimbalzi (RB)", x = "RB", y = "Frequenza")

# -------------------------
# Grafici per WS
# -------------------------
# boxplot
# Calcolo quartili e IQR per definire gli outlier (opzionale, qui serve solo per info)
q1 <- quantile(dati$WS, 0.25, na.rm = TRUE)
q3 <- quantile(dati$WS, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_superiore <- q3 + 1.5 * iqr

# Filtra solo outlier con WS > 125
outlier_ws <- dati %>%
  filter(WS > 125) %>%
  mutate(x = 1.02)  # spostiamo le etichette a destra del boxplot

# Boxplot con outlier etichettati (solo WS > 125)
ggplot(dati, aes(x = factor(1), y = WS)) +
  geom_boxplot(fill = "lightblue") +
  geom_text_repel(data = outlier_ws, aes(x = x, y = WS, label = PLAYER),
                  color = "red", size = 3, hjust = 0, nudge_x = 0.01) +
  coord_cartesian(xlim = c(0.8, 1.3)) +  # spazio per etichette a destra
  labs(title = "Boxplot - Win rate share (WS) con outlier etichettati (WS > 125)", x = "", y = "WS") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# istogramma
ggplot(dati, aes(x = WS)) +
  geom_density(fill = "lightcoral", alpha = 0.5) +
  labs(title = "Density Plot - Win Shares (WS)", x = "WS")

# -------------------------
# Grafici per BPM
# -------------------------
# boxplot
# Calcolo quartili e IQR per definire gli outlier (opzionale, qui serve solo per info)
q1 <- quantile(dati$BPM, 0.25, na.rm = TRUE)
q3 <- quantile(dati$BPM, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_superiore <- q3 + 1.5 * iqr

# Filtra solo outlier con BPM  > 5 | BPM < -20
outlier_bpm <- dati %>% 
  filter(BPM  > 5 | BPM < -20) %>%
  mutate(x = 1.02)  # spostiamo le etichette a destra del boxplot

# Boxplot con outlier etichettati (solo BPM  > 5 | BPM < -20)
ggplot(dati, aes(x = factor(1), y = BPM)) +
  geom_boxplot(fill = "lightblue") +
  geom_text_repel(data = outlier_bpm, aes(x = x, y = BPM, label = PLAYER),
                  color = "red", size = 3, hjust = 0, nudge_x = 0.01) +
  coord_cartesian(xlim = c(0.8, 1.3)) +  # spazio per etichette a destra
  labs(title = "Boxplot - Box Plus Minus (BPM) con outlier etichettati (BPM  > 5 | BPM < -20)", x = "", y = "BPM") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# istogramma
ggplot(dati, aes(x = BPM)) +
  geom_density(fill = "mediumpurple", alpha = 0.5) +
  labs(title = "Density Plot - Box Plus Minus (BPM)", x = "BPM")

# -------------------------
# Grafici per MP
# -------------------------
ggplot(dati, aes(x = MP)) +
  geom_histogram(binwidth = 2, fill = "gold", color = "black") +
  labs(title = "Istogramma - Minuti giocati (MP)", x = "MP", y = "Frequenza")

# -------------------------
# Grafici per % di tiro (FG%, FT%, 3P%)
# -------------------------
library(scales)  # carica il pacchetto scales

# 1. Controlla struttura e tipo dati
str(dati$`FG%`)
print(head(dati$`FG%`, 20))

# 2. Converti in numerico se non lo è già
dati$`FG%` <- as.numeric(dati$`FG%`)

# 3. Filtra solo valori validi: numerici, non NA, tra 0 e 1
dati_fg <- dati %>%
  filter(!is.na(`FG%`), `FG%` >= 0, `FG%` <= 1)

# 4. Controlla riepilogo e range
print(summary(dati_fg$`FG%`))
print(range(dati_fg$`FG%`))

# 5. Controlla se ci sono valori 0 o esattamente 1 (possono disturbare)
print(table(dati_fg$`FG%` == 0))
print(table(dati_fg$`FG%` == 1))

# 6. Stampa qualche valore per sicurezza
print(head(dati_fg$`FG%`, 20))

# 7. Ora prova istogramma con binwidth 0.01 (1%)
ggplot(dati_fg, aes(x = `FG%`)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "black") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Istogramma - FG%", x = "FG%", y = "Frequenza") +
  theme_minimal()

# 8. Density plot (opzionale)
ggplot(dati_fg, aes(x = `FG%`)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Density plot - FG%", x = "FG%", y = "Densità") +
  theme_minimal()

# 1. Controlla struttura e tipo dati
str(dati$`3P%`)
print(head(dati$`3P%`, 20))

# 2. Converti in numerico se non lo è già
dati$`3P%` <- as.numeric(dati$`3P%`)

# 3. Filtra solo valori validi: numerici, non NA, tra 0 e 1
dati_3p <- dati %>%
  filter(!is.na(`3P%`), `3P%` >= 0, `3P%` <= 1)

# 4. Controlla riepilogo e range
print(summary(dati_3p$`3P%`))
print(range(dati_3p$`3P%`))

# 5. Controlla se ci sono valori 0 o esattamente 1 (possono disturbare)
print(table(dati_3p$`3P%` == 0))
print(table(dati_3p$`3P%` == 1))

# 6. Stampa qualche valore per sicurezza
print(head(dati_3p$`3P%`, 20))

# 7. Istogramma con binwidth 0.05 (5%)
ggplot(dati_3p, aes(x = `3P%`)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Istogramma - 3P%", x = "3P%", y = "Frequenza") +
  theme_minimal()

# 8. Density plot (opzionale)
ggplot(dati_3p, aes(x = `3P%`)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Density plot - 3P%", x = "3P%", y = "Densità") +
  theme_minimal()

# 1. Controlla struttura e tipo dati
str(dati$`FT%`)
print(head(dati$`FT%`, 20))

# 2. Converti in numerico se non lo è già
dati$`FT%` <- as.numeric(dati$`FT%`)

# 3. Filtra solo valori validi: numerici, non NA, tra 0 e 1
dati_ft <- dati %>%
  filter(!is.na(`FT%`), `FT%` >= 0, `FT%` <= 1)

# 4. Controlla riepilogo e range
print(summary(dati_ft$`FT%`))
print(range(dati_ft$`FT%`))

# 5. Controlla se ci sono valori 0 o esattamente 1 (possono disturbare)
print(table(dati_ft$`FT%` == 0))
print(table(dati_ft$`FT%` == 1))

# 6. Stampa qualche valore per sicurezza
print(head(dati_ft$`FT%`, 20))

# 7. Istogramma con binwidth 0.05 (5%)
ggplot(dati_ft, aes(x = `FT%`)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Istogramma - FT%", x = "FT%", y = "Frequenza") +
  theme_minimal()

# 8. Density plot (opzionale)
ggplot(dati_ft, aes(x = `FT%`)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Density plot - FT%", x = "FT%", y = "Densità") +
  theme_minimal()

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

ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, col = YEAR)) +
  geom_point(alpha = 0.5) +
  labs(title = "Durata della carriera NBA in relazione alla posizione al draft",
       x = "Posizione al Draft (Overall Pick)",
       y = "Durata della carriera NBA (anni)") +
  theme_minimal()

# -------------------------
#Grafico dove metto in relazione Nba season ed overall pick per vedere se effettivamente le prime scelte del draft sono quelle che giocano più anni in nba
# -------------------------
library(ggplot2)
library(viridis)

ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, color = `NBA SEASON`)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Durata carriera (anni)") +
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
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Durata carriera (anni)") +
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
  rename(`Durata NBA` = `NBA SEASON`,
         `Punti Totali` = TPTS,
         `Assist Totali` = TAST,
         `Rimbalzi Totali` = TRB) %>%
  pivot_longer(
    cols = c(`Durata NBA`, `Punti Totali`, `Assist Totali`, `Rimbalzi Totali`),
    names_to = "Statistica",
    values_to = "Valore"
  )

# Breaks personalizzati per l'asse X
breaks_x <- c(1:10, seq(15, 60, by = 5))

# Grafico finale
ggplot(dati_long, aes(x = `OVERALL PICK`, y = Valore)) +
  geom_jitter(width = 0.6, alpha = 0.2, color = "#1f77b4") +  # punti leggeri
  geom_smooth(method = "loess", se = FALSE, color = "#e63946", size = 1.2) +  # linea tendenza
  facet_wrap(~Statistica, scales = "free_y", ncol = 2) +  # un pannello per ogni statistica
  scale_x_reverse(breaks = breaks_x) +
  labs(
    title = "📈 Impatto della Posizione al Draft su Carriera e Statistiche NBA",
    subtitle = "I giocatori scelti prima durano e producono di più?",
    x = "Posizione al Draft (Overall Pick)",
    y = "Valore",
    caption = "Fonte: Dataset NBA Draft"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_text(color = "gray20"),
    panel.grid.minor = element_blank()
  )


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
  pivot_longer(-`OVERALL PICK`, names_to = "Statistica", values_to = "Valore_Normalizzato")

# 3) Heatmap "figa"
ggplot(dati_heatmap_norm, aes(x = Statistica, y = factor(`OVERALL PICK`, levels = rev(unique(`OVERALL PICK`))), fill = Valore_Normalizzato)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis(option = "magma", direction = -1, name = "Valore Normalizzato") +
  geom_text(aes(label = sprintf("%.2f", Valore_Normalizzato)), color = "white", size = 3, fontface = "bold") +
  labs(
    title = "🔥 Heatmap delle Prestazioni Normalizzate per Posizione al Draft (1-60)",
    subtitle = "Durata carriera e statistiche principali: punti, rimbalzi, assist",
    x = NULL,
    y = "Posizione al Draft (Overall Pick)",
    caption = "Fonte: Dataset NBA Draft"
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

ggplot(dati, aes(x = `OVERALL PICK`, y = `NBA SEASON`, color = TPTS)) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_viridis(option = "plasma", name = "Punti Totali") +
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
  geom_boxplot(alpha = 0.85, outlier.color = "#e74c3c", outlier.size = 2) +
  facet_wrap(~ Statistica, scales = "free_y") +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(
    title = "📊 Confronto statistiche per gruppi di posizione al Draft",
    subtitle = "Top 10, Medio (11-30) e Tardive (>30)",
    x = "Gruppo Draft",
    y = "Valore Statistica",
    caption = "Fonte: Dataset NBA Draft"
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


# Modello lineare: MP totale in base alla posizione al draft
mod1 <- lm(MP ~ `OVERALL PICK`, data = dati)
summary(mod1)

# Grafico con ggplot2
library(ggplot2)

ggplot(dati, aes(x = `OVERALL PICK`, y = MP)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Minuti totali giocati vs. posizione al draft",
       x = "OVERALL PICK (posizione al draft)",
       y = "Minuti giocati (MP)")



# Calcolare la correlazione tra durata della carriera (NBA SEASON) e posizione al draft (Overall Pick)
correlazione <- cor(dati$`NBA SEASON`, dati$`OVERALL PICK`, use = "complete.obs")
correlazione






library(mgcv)
mod_lm = lm(`NBA SEASON` ~ PTS, data = dati)
summary(mod_lm)

#creo subset senza considerare player
mod_lm = lm(`NBA SEASON` ~ PTS+AST+RB+WS, data = dati)
summary(mod_lm)

mod_lm = lm(`NBA SEASON` ~ ., data = dati[, !names(dati) %in% c("PLAYER", "TEAM", "AFFILATION", "carriera")])
summary(mod_lm)


mod_glm = glm(`NBA SEASON` ~ PTS+AST+RB+WS, data = dati,family="poisson")
summary(mod_glm)

mod_glm = glm(`NBA SEASON` ~ ., data = dati[, !names(dati) %in% c("PLAYER", "TEAM", "AFFILATION", "carriera")],family="poisson")
summary(mod_glm)

dati$carriera=dati$`NBA SEASON`
mod_gam1 = gam(carriera ~ s(PTS, bs="cr"), data = dati, family="poisson")
summary(mod_gam1)


# Rinomina tutte le colonne problematiche in dati
names(dati) <- gsub(" ", "_", names(dati))
names(dati) <- gsub("%", "perc", names(dati))
names(dati) <- gsub("/", "_", names(dati))
names(dati) <- gsub("3", "tre", names(dati))

# Controlla i nomi ora
print(names(dati))


mod_gam1 <- gam(NBA_SEASON ~ 
                  s(YEAR, bs = "cr") + 
                  
                  s(ROUND_PICK, bs = "cr") + 
                  s(OVERALL_PICK, bs = "cr") + 
                  s(G, bs = "cr") + 
                  s(TMP, bs = "cr") + 
                  s(TPTS, bs = "cr") + 
                  s(TRB, bs = "cr") + 
                  s(TAST, bs = "cr") + 
                  s(FGperc, bs = "cr") + 
                  s(trePperc, bs = "cr") + 
                  s(FTperc, bs = "cr") + 
                  s(MP, bs = "cr") + 
                  s(PTS, bs = "cr") + 
                  s(RB, bs = "cr") + 
                  s(AST, bs = "cr") + 
                  s(WS, bs = "cr") + 
                  s(WS_48, bs = "cr") + 
                  s(BPM, bs = "cr") + 
                  s(VORP, bs = "cr"),
                data = dati[, !names(dati) %in% c("PLAYER", "TEAM", "AFFILATION", "ROUND_NUMBER")], 
                family = "poisson")
summary(mod_gam1)



AIC(mod_lm)

summary(mod_lm)$sp.criterion

summary(mod_lm)$r.sq  # adjusted R squared

AIC(mod_glm)

summary(mod_glm)$sp.criterion

summary(mod_glm)$r.sq  # adjusted R squared

AIC(mod_gam1)

summary(mod_gam1)$sp.criterion

summary(mod_gam1)$r.sq  # adjusted R squared

# AIC e MSE per mod_lm
cat("AIC mod_lm:", AIC(mod_lm), "\n")
cat("sp.criterion mod_lm:", summary(mod_lm)$sp.criterion, "\n")
pred_lm <- predict(mod_lm, newdata = dati)
mse_lm <- mean((dati$NBA_SEASON - pred_lm)^2)
cat("MSE mod_lm:", mse_lm, "\n\n")

# AIC e MSE per mod_glm
cat("AIC mod_glm:", AIC(mod_glm), "\n")
cat("sp.criterion mod_glm:", summary(mod_glm)$sp.criterion, "\n")
pred_glm <- predict(mod_glm, newdata = dati, type = "response")
mse_glm <- mean((dati$NBA_SEASON - pred_glm)^2)
cat("MSE mod_glm:", mse_glm, "\n\n")

# AIC, sp.criterion e MSE per mod_gam1
cat("AIC mod_gam1:", AIC(mod_gam1), "\n")
cat("sp.criterion mod_gam1:", summary(mod_gam1)$sp.criterion, "\n")
pred_gam <- predict(mod_gam1, newdata = dati, type = "response")
mse_gam <- mean((dati$NBA_SEASON - pred_gam)^2)
cat("MSE mod_gam1:", mse_gam, "\n")


dati_modello <- subset(dati, select = -c(PLAYER, TEAM, AFFILATION))
# Initialize a model with all predictors
backward_model <- lm(`NBA_SEASON`~ ., data = dati_modello)
# Backward stepwise regression
backward_model <- step(backward_model, direction = "backward")

# ROUND NUMBER
mod_lm1 <- gam(`NBA SEASON` ~ `ROUND NUMBER`, data = dati)
mod_gam1 <- gam(`NBA SEASON` ~ s(`ROUND NUMBER`, bs = "cr"), data = dati)
cat("=== ROUND NUMBER ===\n")
cat("AIC modello lineare: ", AIC(mod_lm1), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm1)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm1)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam1))

# ROUND PICK
mod_lm2 <- gam(`NBA SEASON` ~ `ROUND PICK`, data = dati)
mod_gam2 <- gam(`NBA SEASON` ~ s(`ROUND PICK`, bs = "cr"), data = dati)
cat("\n=== ROUND PICK ===\n")
cat("AIC modello lineare: ", AIC(mod_lm2), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm2)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm2)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam2))

# OVERALL PICK
mod_lm3 <- gam(`NBA SEASON` ~ `OVERALL PICK`, data = dati)
mod_gam3 <- gam(`NBA SEASON` ~ s(`OVERALL PICK`, bs = "cr"), data = dati)
cat("\n=== OVERALL PICK ===\n")
cat("AIC modello lineare: ", AIC(mod_lm3), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm3)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm3)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam3))

# G
mod_lm4 <- gam(`NBA SEASON` ~ G, data = dati)
mod_gam4 <- gam(`NBA SEASON` ~ s(G, bs = "cr"), data = dati)
cat("\n=== G ===\n")
cat("AIC modello lineare: ", AIC(mod_lm4), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm4)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm4)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam4))

# TMP
mod_lm5 <- gam(`NBA SEASON` ~ TMP, data = dati)
mod_gam5 <- gam(`NBA SEASON` ~ s(TMP, bs = "cr"), data = dati)
cat("\n=== TMP ===\n")
cat("AIC modello lineare: ", AIC(mod_lm5), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm5)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm5)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam5))

# TPTS
mod_lm6 <- gam(`NBA SEASON` ~ TPTS, data = dati)
mod_gam6 <- gam(`NBA SEASON` ~ s(TPTS, bs = "cr"), data = dati)
cat("\n=== TPTS ===\n")
cat("AIC modello lineare: ", AIC(mod_lm6), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm6)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm6)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam6))

# TRB
mod_lm7 <- gam(`NBA SEASON` ~ TRB, data = dati)
mod_gam7 <- gam(`NBA SEASON` ~ s(TRB, bs = "cr"), data = dati)
cat("\n=== TRB ===\n")
cat("AIC modello lineare: ", AIC(mod_lm7), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm7)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm7)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam7))

# TAST
mod_lm8 <- gam(`NBA SEASON` ~ TAST, data = dati)
mod_gam8 <- gam(`NBA SEASON` ~ s(TAST, bs = "cr"), data = dati)
cat("\n=== TAST ===\n")
cat("AIC modello lineare: ", AIC(mod_lm8), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm8)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm8)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam8))

# MP
mod_lm9 <- gam(`NBA SEASON` ~ MP, data = dati)
mod_gam9 <- gam(`NBA SEASON` ~ s(MP, bs = "cr"), data = dati)
cat("\n=== MP ===\n")
cat("AIC modello lineare: ", AIC(mod_lm9), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm9)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm9)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam9))

# PTS
mod_lm10 <- gam(`NBA SEASON` ~ PTS, data = dati)
mod_gam10 <- gam(`NBA SEASON` ~ s(PTS, bs = "cr"), data = dati)
cat("\n=== PTS ===\n")
cat("AIC modello lineare: ", AIC(mod_lm10), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm10)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm10)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam10))

# RB
mod_lm11 <- gam(`NBA SEASON` ~ RB, data = dati)
mod_gam11 <- gam(`NBA SEASON` ~ s(RB, bs = "cr"), data = dati)
cat("\n=== RB ===\n")
cat("AIC modello lineare: ", AIC(mod_lm11), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm11)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm11)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam11))

# AST
mod_lm12 <- gam(`NBA SEASON` ~ AST, data = dati)
mod_gam12 <- gam(`NBA SEASON` ~ s(AST, bs = "cr"), data = dati)
cat("\n=== AST ===\n")
cat("AIC modello lineare: ", AIC(mod_lm12), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm12)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm12)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam12))

# WS
mod_lm13 <- gam(`NBA SEASON` ~ WS, data = dati)
mod_gam13 <- gam(`NBA SEASON` ~ s(WS, bs = "cr"), data = dati)
cat("\n=== WS ===\n")
cat("AIC modello lineare: ", AIC(mod_lm13), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm13)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm13)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam13))

# WS/48
mod_lm14 <- gam(`NBA SEASON` ~ `WS/48`, data = dati)
mod_gam14 <- gam(`NBA SEASON` ~ s(`WS/48`, bs = "cr"), data = dati)
cat("\n=== WS/48 ===\n")
cat("AIC modello lineare: ", AIC(mod_lm14), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm14)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm14)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam14))

# BPM
mod_lm15 <- gam(`NBA SEASON` ~ BPM, data = dati)
mod_gam15 <- gam(`NBA SEASON` ~ s(BPM, bs = "cr"), data = dati)
cat("\n=== BPM ===\n")
cat("AIC modello lineare: ", AIC(mod_lm15), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm15)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm15)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam15))

# VORP
mod_lm16 <- gam(`NBA SEASON` ~ VORP, data = dati)
mod_gam16 <- gam(`NBA SEASON` ~ s(VORP, bs = "cr"), data = dati)
cat("\n=== VORP ===\n")
cat("AIC modello lineare: ", AIC(mod_lm16), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm16)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm16)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam16))

# FG%
mod_lm17 <- gam(`NBA SEASON` ~ `FG%`, data = dati)
mod_gam17 <- gam(`NBA SEASON` ~ s(`FG%`, bs = "cr"), data = dati)
cat("\n=== FG% ===\n")
cat("AIC modello lineare: ", AIC(mod_lm17), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm17)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm17)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam17))

# 3P%
mod_lm18 <- gam(`NBA SEASON` ~ `3P%`, data = dati)
mod_gam18 <- gam(`NBA SEASON` ~ s(`3P%`, bs = "cr"), data = dati)
cat("\n=== 3P% ===\n")
cat("AIC modello lineare: ", AIC(mod_lm18), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm18)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm18)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam18))

# FT%
mod_lm19 <- gam(`NBA SEASON` ~ `FT%`, data = dati)
mod_gam19 <- gam(`NBA SEASON` ~ s(`FT%`, bs = "cr"), data = dati)
cat("\n=== FT% ===\n")
cat("AIC modello lineare: ", AIC(mod_lm19), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm19)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm19)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam19))


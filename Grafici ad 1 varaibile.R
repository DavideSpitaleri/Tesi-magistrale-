library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggrepel)

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
  geom_boxplot(fill = "firebrick3") +
  geom_text_repel(data = outlier_pts, aes(x = x, y = PTS, label = PLAYER),
                  color = "black", size = 6, hjust = 0, nudge_x = 0.02) +
  coord_cartesian(xlim = c(0.8, 1.3)) +  # spazio per etichette a destra
  labs(x = "", y = "PUNTI") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),  
        axis.text.y = element_text(size = 12, color = "black"),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# istogramma
ggplot(dati, aes(x = log(PTS))) +
  geom_histogram(fill = "firebrick3", color = "black") +
  labs(x = "Punti", y = "Frequenza") +
  scale_y_continuous(breaks = c(0, 40, 80, 120)) +
theme_minimal() +
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))
 

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
  geom_boxplot(fill = "darkorange2") +
  geom_text_repel(data = outlier_ast, aes(x = x, y = AST, label = PLAYER),
                  color = "black", size = 6, hjust = 0, nudge_x = 0.02) +
  coord_cartesian(xlim = c(0.7, 1.3)) +  # spazio per etichette a destra
  labs(x = "", y = "ASSIST") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),  
        axis.text.y = element_text(size = 12, color = "black"),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# istogramma
ggplot(dati, aes(x = log(AST))) +
  geom_histogram(fill = "darkorange2", color = "black") +
  labs(x = "Assist", y = "Frequenza") +
  theme_minimal() +
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))

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
  geom_boxplot(fill = "goldenrod") +
  geom_text_repel(data = outlier_rb, aes(x = x, y = RB, label = PLAYER),
                  color = "black", size = 4.5, hjust = 0, nudge_x = 0.015, direction = "both",        # consente spostamenti orizzontali e verticali
                  force = 1,                 # forza di repulsione
                  max.overlaps = Inf,        # mostra tutte le etichette
                  box.padding = 0.2,         # margine intorno all'etichetta
                  point.padding = 0.2,       # margine intorno al punto
                  segment.color = "gray60",
                  segment.size = 0.3
                  ) +
  coord_cartesian(xlim = c(0.7, 1.3)) +  # spazio per etichette a destra
  labs(title = "Boxplot - Rimbalzi (RB) con outlier etichettati (RB > 10)", x = "", y = "RB") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),  
        axis.text.y = element_text(size = 12, color = "black"),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


outlier_rb <- dati %>%
  filter(RB > 10) %>%
  arrange(RB) %>%
  mutate(
    x_point = 1,
    n = row_number(),
    x_label = ifelse(n %% 2 == 0, 1.005, 0.995),
    side = ifelse(n %% 2 == 0, "right", "left"),
    hjust_pos = ifelse(side == "right", 0, 1)
  )

# Applichiamo l'offset verticale separatamente per lato e per RB
outlier_rb <- outlier_rb %>%
  group_by(RB, side) %>%
  mutate(
    duplicati = n(),
    seq_dup = row_number(),
    # offset alternato sopra/sotto per ciascun lato
    y_label = RB + (seq_dup - 1) * 0.1 * ifelse(seq_dup %% 2 == 0, 1, -1)
  ) %>%
  ungroup()

ggplot(dati, aes(x = factor(1), y = RB)) +
  geom_boxplot(fill = "goldenrod") +
  
  geom_segment(
    data = outlier_rb,
    aes(x = x_label, y = y_label, xend = x_point, yend = RB),
    color = "gray40",
    size = 0.5
  ) +
  
  geom_text(
    data = outlier_rb,
    aes(x = x_label, y = y_label, label = PLAYER, hjust = hjust_pos),
    size = 6,
    color = "black"
  ) +
  
  coord_cartesian(xlim = c(0.99, 1.01)) +
  labs(x = "", y = "RIMBALZI"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),  
        axis.text.y = element_text(size = 12, color = "black"),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# Calcola i quartili e IQR per i rimbalzi (RB)
q1 <- quantile(dati$RB, 0.25, na.rm = TRUE)
q3 <- quantile(dati$RB, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_superiore <- q3 + 1.5 * iqr

# Filtra solo gli outlier con RB > 10 e crea colonna per il cognome
outlier_rb <- dati %>%
  filter(RB > 10) %>%
  mutate(
    
    Cognome = word(PLAYER, -1)  # prende l'ultima parola come cognome
  )

# Boxplot con outlier etichettati
ggplot(dati, aes(x = factor(1), y = RB)) +
  geom_boxplot(fill = "goldenrod") +
  geom_text_repel(data = outlier_rb,
                  aes(x = factor(1), y = RB, label = Cognome),
                  color = "black",
                  size = 5,
                  hjust = 0,
                  nudge_x = 0.02,
                  direction = "both",
                  force = 1,
                  max.overlaps = Inf,
                  box.padding = 0.2,
                  point.padding = 0.2,
                  segment.color = "gray60",
                  segment.size = 0.3) +
  coord_cartesian(xlim = c(0.7, 1.3)) +  # spazio per etichette
  labs(x = "",
       y = "RIMBALZI") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),  
    axis.text.y = element_text(size = 12, color = "black"),  
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# istogramma
ggplot(dati, aes(x = log(RB))) +
  geom_histogram(fill = "goldenrod", color = "black") +
  labs(x = "Rimbalzi", y = "Frequenza") +
  theme_minimal() +
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))

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
  filter(WS > 125) 
 

# Boxplot con outlier etichettati (solo WS > 125)
ggplot(dati, aes(x = factor(1), y = WS)) +
  geom_boxplot(fill = "greenyellow") +
  geom_text_repel(data = outlier_ws, aes(x = factor(1), y = WS, label = PLAYER),
                  color = "black", size = 6, hjust = 0, nudge_x = 0) +
  coord_cartesian(xlim = c(0.7, 1.3)) +  # spazio per etichette a destra
  labs( x = "", y = "WINSHARE") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),  
        axis.text.y = element_text(size = 12, color = "black"),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Primo passaggio: calcola posizioni etichette con geom_text_repel senza plottare
repel <- ggplot(dati, aes(x = factor(1), y = WS)) +
  geom_boxplot(fill = "greenyellow") +
  geom_text_repel(
    data = outlier_ws,
    aes(x = x, y = WS, label = PLAYER),
    direction = "y",
    nudge_x = ifelse(outlier_ws$x > 1, 0.15, -0.15),
    hjust = ifelse(outlier_ws$x > 1, 0, 1),
    box.padding = 0.1,
    point.padding = 0,
    segment.color = NA,   # no segmenti ora
    force = 1.5,
    max.overlaps = Inf,
    seed = 42
  )

outlier_ws <- dati %>%
  filter(WS > 125) %>%        # filtro su WS invece di RB
  arrange(WS) %>%
  mutate(
    x_point = 1,
    n = row_number(),
    # alterna le etichette leggermente a destra o sinistra del boxplot
    x_label = ifelse(n %% 2 == 0, 1.005, 0.995),
    side = ifelse(n %% 2 == 0, "right", "left"),
    hjust_pos = ifelse(side == "right", 0, 1)
  ) %>%
  group_by(side) %>%
  mutate(
    duplicati = n(),
    seq_dup = row_number(),
    # alterna offset verticale per evitare sovrapposizione
    y_label = WS + 0.1 * (seq_dup %/% 2 + 1) * ifelse(seq_dup %% 2 == 0, 1, -1)
  ) %>%
  ungroup()

ggplot(dati, aes(x = factor(1), y = WS)) +
  geom_boxplot(fill = "greenyellow") +
  
  geom_segment(
    data = outlier_ws,
    aes(x = x_label, y = y_label, xend = x_point, yend = WS),
    color = "gray40",
    size = 0.5
  ) +
  
  geom_text(
    data = outlier_ws,
    aes(x = x_label, y = y_label, label = PLAYER, hjust = hjust_pos),
    size = 3,
    color = "black"
  ) +
  
  coord_cartesian(xlim = c(0.99, 1.01)) +
  labs(x = "", y = "WINSHARE") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# istogramma
ggplot(dati, aes(x = WS)) +
  geom_histogram(fill = "lightcoral") +
  labs(title = "Density Plot - Win Shares (WS)", x = "WS")

ggplot(dati, aes(x = log(WS))) +
  geom_histogram(fill = "yellowgreen", color = "black") +
  labs(x = "Winshare", y = "Frequenza") +
  theme_minimal()+
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))

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

outlier_bpm <- dati %>%
  filter(BPM  > 5 | BPM < -20) %>%        # filtro su BPM
  arrange(BPM) %>%
  mutate(
    x_point = 1,
    n = row_number(),
    # alterna le etichette leggermente a destra o sinistra del boxplot
    x_label = ifelse(n %% 2 == 0, 1.005, 0.995),
    side = ifelse(n %% 2 == 0, "right", "left"),
    hjust_pos = ifelse(side == "right", 0, 1)
  ) %>%
  group_by(side) %>%
  mutate(
    duplicati = n(),
    seq_dup = row_number(),
    # offset verticale alternato per evitare sovrapposizioni
    y_label = BPM + 0.1 * (seq_dup %/% 2 + 1) * ifelse(seq_dup %% 2 == 0, 1, -1)
  ) %>%
  ungroup()

ggplot(dati, aes(x = factor(1), y = BPM)) +
  geom_boxplot(fill = "skyblue") +
  
  geom_segment(
    data = outlier_bpm,
    aes(x = x_label, y = y_label, xend = x_point, yend = BPM),
    color = "gray40",
    size = 0.5
  ) +
  
  geom_text(
    data = outlier_bpm,
    aes(x = x_label, y = y_label, label = PLAYER, hjust = hjust_pos),
    size = 3,
    color = "black"
  ) +
  
  coord_cartesian(xlim = c(0.99, 1.01)) +
  labs(x = "", y = "BPM") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



# Calcolo IQR e quartili
iqr <- IQR(dati$BPM, na.rm = TRUE)
q1 <- quantile(dati$BPM, 0.25, na.rm = TRUE)
q3 <- quantile(dati$BPM, 0.75, na.rm = TRUE)

# Filtra solo outlier (fuori 1.5*IQR)
outlier_bpm_all <- dati %>%
  filter(!is.na(BPM)) %>%
  filter(BPM < (q1 - 1.5*iqr) | BPM > (q3 + 1.5*iqr)) %>%
  arrange(BPM)

# Prendi massimo 5 valori più bassi e 5 più alti
outlier_bpm <- bind_rows(
  head(outlier_bpm_all, 5),
  tail(outlier_bpm_all, 5)
) %>%
  arrange(BPM) %>%
  mutate(
    x_point = 1,
    n = row_number(),
    x_label = ifelse(n %% 2 == 0, 1.005, 0.995),
    side = ifelse(n %% 2 == 0, "right", "left"),
    hjust_pos = ifelse(side == "right", 0, 1)
  ) %>%
  group_by(side) %>%
  mutate(
    seq_dup = row_number(),
    y_label = BPM + 0.15 * seq_dup
  ) %>%
  ungroup()

ggplot(dati, aes(x = factor(1), y = BPM)) +
  geom_boxplot(fill = "mediumpurple") +
  
  geom_segment(
    data = outlier_bpm,
    aes(x = x_label, y = y_label, xend = x_point, yend = BPM),
    color = "gray40",
    size = 1
  ) +
  
  geom_text(
    data = outlier_bpm,
    aes(x = x_label, y = y_label, label = PLAYER, hjust = hjust_pos),
    size = 5,
    color = "black"
  ) +
  
  coord_cartesian(xlim = c(0.99, 1.01)) +
  labs(x = "", y = "BPM") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),  
        axis.text.y = element_text(size = 12, color = "black"),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot(dati, aes(x = factor(1), y = BPM)) +
  geom_boxplot(fill = "mediumpurple") +
  
  geom_text_repel(
    data = outlier_bpm,
    aes(x = factor(1), y = BPM, label = PLAYER),
    size = 5,
    color = "black",
    direction = "both",       # ✅ consente spostamenti verticali e orizzontali
    nudge_x = 0,              # non forzare lo spostamento
    segment.color = "gray40",
    segment.size = 0.6,
    box.padding = 0.25,
    point.padding = 0.2,
    force = 2,                # aumenta la repulsione
    max.overlaps = Inf
  ) +
  
  coord_cartesian(xlim = c(0.95, 1.05)) +  # lascia spazio su entrambi i lati
  labs(x = "", y = "BPM") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# istogramma
ggplot(dati, aes(x = log(BPM))) +
  geom_histogram(fill = "mediumpurple", color = "black") +
  labs(x = "BPM", y = "Frequenza") +
  theme_minimal()+
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))

# -------------------------
# Grafici per MP
# -------------------------
ggplot(dati, aes(x = MP)) +
  geom_histogram(binwidth = 2, fill = "peru", color = "black") +
  labs(x = "MP", y = "Frequenza")+
  theme_minimal()+
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))
#0 inflation

# Calcola media di MP
lambda_hat <- mean(dati$MP)

# Crea una sequenza di valori interi da 0 al massimo osservato
max_mp <- max(dati$MP)
valori <- 0:max_mp

# Frequenze osservate
freq_osservate <- as.data.frame(table(dati$MP))
colnames(freq_osservate) <- c("MP", "Frequenza")
freq_osservate$MP <- as.numeric(as.character(freq_osservate$MP))

# Frequenze attese con distribuzione Poisson
freq_attese <- data.frame(
  MP = valori,
  Frequenza_Poisson = dpois(valori, lambda = lambda_hat) * nrow(dati)
)

# Merge osservato e atteso
df_plot <- merge(freq_osservate, freq_attese, by = "MP", all = TRUE)
df_plot[is.na(df_plot)] <- 0  # Sostituisce i NA con 0

# Plot
ggplot(df_plot, aes(x = MP)) +
  geom_col(aes(y = Frequenza), fill = "gold", color = "black") +
  geom_line(aes(y = Frequenza_Poisson), color = "red", size = 1) +
  labs(title = "Confronto: Frequenze osservate vs attese (Poisson)",
       x = "MP", y = "Frequenza") +
  theme_minimal()

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
  geom_histogram(binwidth = 0.01, fill = "navyblue", color = "black") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "FG%", y = "Frequenza") +
  theme_minimal() +
  ylim(0,120)+
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))



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
  geom_histogram(binwidth = 0.05, fill = "steelblue2", color = "black") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "3P%", y = "Frequenza") +
  theme_minimal() +
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))

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
  geom_histogram(binwidth = 0.05, fill = "lightskyblue2", color = "black") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "FT%", y = "Frequenza") +
  theme_minimal() +
  theme(axis.title = element_text(size = 15),  
        axis.text = element_text(size = 12, color = "black"))


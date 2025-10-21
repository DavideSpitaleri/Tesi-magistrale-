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
dati$YEAR <- as.numeric(dati$YEAR)

str(dati)  # Controlla la struttura aggiornata del dataset
summary(dati)




# 2. Seleziona le colonne numeriche (verifica che questi nomi corrispondano esattamente!)
colonne_numeriche <- c("YEAR", "ROUND NUMBER", "ROUND PICK", "OVERALL PICK",
                       "NBA SEASON", "G", "TMP", "TPTS", "TRB", "TAST", 
                       "FG%", "3P%", "FT%", "MP", "PTS", "RB", "AST", 
                       "WS", "WS/48", "BPM", "VORP")

dati_num <- dati %>%
  select(all_of(colonne_numeriche)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()
# 2. Calcola matrice correlazione
corr_mat <- cor(dati_num, use = "pairwise.complete.obs")

# 3. Plot con corrplot personalizzato
library(RColorBrewer)
library(corrplot)

corrplot(corr_mat, 
         method = "color",
         outline = TRUE,
         addgrid.col = "darkgray",
         order = "hclust",
         addrect = 4,
         rect.col = "black",
         rect.lwd = 5,
         cl.pos = "b",
         tl.col = "indianred4",
         tl.cex = 1,
         cl.cex = 1,
         addCoef.col = "black",
         number.digits = 2,
         number.cex = 0.75,
         col = colorRampPalette(c("darkred", "white", "midnightblue"))(100)
)

# 1. Calcolo matrice di correlazione
colonne_numeriche <- c("YEAR", "ROUND NUMBER", "ROUND PICK", "OVERALL PICK",
                       "NBA SEASON", "G", "TMP", "TPTS", "TRB", "TAST", 
                       "FG%", "3P%", "FT%", "MP", "PTS", "RB", "AST", 
                       "WS", "WS/48", "BPM", "VORP")

dati_num <- dati %>%
  select(all_of(colonne_numeriche)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

corr_mat <- cor(dati_num, use = "pairwise.complete.obs")

# 2. Crea matrice dei colori per i numeri
colore_testo <- ifelse(abs(corr_mat) < 0.4, "black", "white")

# 3. Plot finale con testo a colori condizionali
corrplot(corr_mat, 
         method = "color",
         outline = TRUE,
         addgrid.col = "darkgray",
         order = "hclust",
         addrect = 4,
         rect.col = "black",
         rect.lwd = 5,
         cl.pos = "b",
         tl.col = "indianred4",
         tl.cex = 1.5,
         cl.cex = 1.5,
         number.cex = 0.75,
         addCoef.col = colore_testo,   # 🎯 Questa è la chiave
         number.digits = 2,
         col = colorRampPalette(c("darkred", "white", "midnightblue"))(100)
)

# Estrai la colonna della correlazione con NBA SEASON
corr_with_NBA <- corr_mat[, "NBA SEASON"]

# Variabili con correlazione > 0.99 (esclusa NBA SEASON stessa)
vars_troppo_correlate <- names(corr_with_NBA)[abs(corr_with_NBA) > 0.99 & names(corr_with_NBA) != "NBA SEASON"]

print(vars_troppo_correlate)





library(GGally)

# 2️⃣ Scegli solo le colonne più importanti (secondo la heatmap)
colonne_scelte <- c("PTS", "MP", "TPTS", "TRB", "TAST", 
                    "WS", "VORP", "BPM", "AST", "RB", 
                    "TMP", "ROUND PICK", "OVERALL PICK")

# 3️⃣ Sottocampiona il dataframe
# ATTENZIONE: Sostituisci 'dati' con il nome del tuo dataframe
dati_selezionati <- dati[, colonne_scelte]

# 4️⃣ Controlla che non ci siano fattori (devono essere numerici!)
str(dati_selezionati)

# 5️⃣ Crea la matrice di coppie identica alla prima foto
p <- ggpairs(
  dati_selezionati,
  upper = list(continuous = wrap("cor", stars = TRUE, size = 4)),
  diag = list(continuous = wrap("densityDiag")),
  lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.5))
)

# 6️⃣ Mostra la matrice
print(p)



# 2️⃣ Colonne scelte (più significative)
colonne_scelte <- c("PTS", "MP", "TPTS", "TRB", "TAST", 
                    "WS", "VORP", "BPM", "AST", "RB", 
                    "TMP", "ROUND PICK", "OVERALL PICK")

# 3️⃣ Seleziona dati
dati_selezionati <- dati[, colonne_scelte]

# 4️⃣ Matrice con colori personalizzati
p <- ggpairs(
  dati_selezionati,
  lower = list(
    continuous = wrap("smooth", 
                      colour = "skyblue",   # linea smooth azzurra
                      alpha = 0.3,          # trasparenza punti
                      size = 0.5)           # spessore linea
  ),
  diag = list(
    continuous = wrap("densityDiag",
                      colour = "black",     # curva densità nera
                      fill = "gray80")      # riempimento chiaro
  ),
  upper = list(
    continuous = wrap("cor",
                      size = 4,             # dimensione testo correlazione
                      stars = TRUE,
                      colour = "black")     # testo nero
  )
) +
  theme_bw() +  # fondo bianco
  theme(
    panel.grid = element_blank()
  )

# 5️⃣ Mostra il grafico
print(p)


# 2️⃣ Seleziona le colonne significative (in base alla correlazione)
colonne_scelte <- c("PTS", "MP", "TPTS", "TRB", "TAST",
                    "WS", "VORP", "BPM", "AST", "RB",
                    "TMP", "ROUND PICK", "OVERALL PICK")

# 3️⃣ Seleziona i dati numerici
dati_selezionati <- dati[, colonne_scelte]

# 4️⃣ Funzione custom per punti + linea con `linewidth` corretto
my_smooth <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_point(color = "orange", alpha = 0.5, size = 1.5) +   # punti arancioni
    geom_smooth(color = "skyblue", se = FALSE, linewidth = 0.7, ...)  # linea azzurra con linewidth
}

# 5️⃣ Crea il ggpairs identico
p <- ggpairs(
  dati_selezionati,
  lower = list(continuous = my_smooth),
  diag = list(continuous = wrap("densityDiag",
                                colour = "black", fill = "gray80")),
  upper = list(continuous = wrap("cor",
                                 size = 4,
                                 stars = TRUE,
                                 colour = "black")),
  progress = FALSE
) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

# 6️⃣ Mostra il grafico
print(p)


# 1️⃣ Carica la libreria per i GAM
library(mgcv)

# 2️⃣ Esempio: modello GAM per vedere come PTS dipende da MP (minuti giocati)
mod_gam <- gam(PTS ~ s(MP, bs = "cr"), data = dati)

# 3️⃣ Mostra riepilogo
summary(mod_gam)

# 4️⃣ Plotta lo spline stimato
plot(mod_gam, shade = TRUE, col = "blue", lwd = 2)
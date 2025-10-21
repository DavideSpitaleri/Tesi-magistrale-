library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(tibble)
library(ggimage)

# Leggi dati
dati <- read_excel("M:/Tesi/Dati tesi modified.xlsx", sheet = "Foglio1")

# Converti colonne in numeric
dati$`FG%` <- as.numeric(dati$`FG%`)
dati$`3P%` <- as.numeric(dati$`3P%`)
dati$`FT%` <- as.numeric(dati$`FT%`)
dati$`MP` <- as.numeric(gsub("min", "", dati$`MP`))
dati$`PTS` <- as.numeric(dati$`PTS`)
dati$`RB` <- as.numeric(dati$`RB`)
dati$`AST` <- as.numeric(dati$`AST`)
dati$`WS` <- as.numeric(dati$`WS`)
dati$`WS/48` <- as.numeric(dati$`WS/48`)
dati$`BPM` <- as.numeric(dati$`BPM`)
dati$`VORP` <- as.numeric(dati$`VORP`)

# Fattori
dati$PLAYER <- as.factor(dati$PLAYER)
dati$AFFILATION <- as.factor(dati$AFFILATION)
dati$TEAM <- as.factor(dati$TEAM)
dati$YEAR <- as.factor(dati$YEAR)

# MVP e colori personalizzati
mvp_names <- c("Joel Embiid", "Nikola Jokić", "Giannis Antetokounmpo",
               "James Harden", "Russell Westbrook", "Stephen Curry",
               "Kevin Durant", "Derrick Rose", "LeBron James")

player_colors <- c(
  "LeBron James (1)" = "#860038",
  "Derrick Rose (1)" = "#CE1141",
  "Kevin Durant (2)" = "#1D428A",
  "Stephen Curry (7)" = "#FDB927",
  "Russell Westbrook (4)" = "#007AC1",
  "James Harden (3)" = "#E03A3E",
  "Giannis Antetokounmpo (15)" = "#00471B",
  "Nikola Jokić (41)" = "#0E2240",
  "Joel Embiid (3)" = "#006BB6"
)

# --- Funzione aggiornata per loghi, per LeBron restituisco due righe distinte con 2 loghi ---
get_logos_multiple <- function(player) {
  if (player == "LeBron James") {
    return(c("loghi/cavaliers.png", "loghi/heats.png"))
  } else if (player == "Derrick Rose") {
    return("loghi/bulls.png")
  } else if (player == "Kevin Durant") {
    return("loghi/warriors.png")
  } else if (player == "Stephen Curry") {
    return("loghi/warriors.png")
  } else if (player == "Russell Westbrook") {
    return("loghi/thunder.png")
  } else if (player == "James Harden") {
    return("loghi/rockets.png")
  } else if (player == "Giannis Antetokounmpo") {
    return("loghi/bucks.png")
  } else if (player == "Nikola Jokić") {
    return("loghi/nuggets.png")
  } else if (player == "Joel Embiid") {
    return("loghi/sixers.png")
  } else {
    return(NA_character_)
  }
}

# --- Prepara dati per facet wrap con duplicazione delle righe LeBron per i due loghi ---
dati_mvp <- dati %>%
  filter(as.character(PLAYER) %in% mvp_names) %>%
  mutate(
    PLAYER_draft = paste0(as.character(PLAYER), " (", `OVERALL PICK`, ")")
  ) %>%
  select(PLAYER_draft, PLAYER, `NBA SEASON`, PTS, RB, AST, `YEAR`)

# Aggiungi i loghi (più righe per LeBron)
dati_loghi <- dati_mvp %>%
  rowwise() %>%
  mutate(logo_path = list(get_logos_multiple(as.character(PLAYER)))) %>%
  unnest(cols = c(logo_path))

# Passaggio a formato long per facet_wrap
dati_long <- dati_loghi %>%
  pivot_longer(cols = c(`NBA SEASON`, PTS, AST, RB),
               names_to = "Variabile", values_to = "Valore")

# Ordina giocatori in base a NBA SEASON decrescente
ordina_giocatori <- dati_mvp %>%
  arrange(desc(`YEAR`)) %>%
  pull(PLAYER_draft)

# Modifica i livelli di Variabile per avere nomi personalizzati
dati_long <- dati_long %>%
  mutate(
    PLAYER_draft = factor(PLAYER_draft, levels = ordina_giocatori),
    Variabile = recode(Variabile,
                       `NBA SEASON` = "Nba season",
                       PTS = "Punti",
                       AST = "Assist",
                       RB = "Rimbalzi"),
    Variabile = factor(Variabile, levels = c("Nba season", "Punti", "Assist", "Rimbalzi")),
    show_logo = ifelse(Variabile %in% c("Nba season", "Assist"), TRUE, FALSE)
  )

# Dataset per barre ed etichette (senza duplicazioni LeBron)
dati_barre <- dati_long %>%
  group_by(PLAYER_draft, Variabile) %>%
  slice(1) %>%
  ungroup()

# Calcolo massimo valore per posizionare i loghi
max_val <- max(dati_barre$Valore, na.rm = TRUE)

# Offset fisso per spostare loghi a destra senza estendere limiti asse
offset_fixed <- 3

dati_long <- dati_long %>%
  mutate(
    logo_x = case_when(
      show_logo & PLAYER == "LeBron James" & logo_path == "loghi/cavaliers.png" ~ max_val + offset_fixed,
      show_logo & PLAYER == "LeBron James" & logo_path == "loghi/heats.png" ~ max_val + offset_fixed + 3.5,  # affiancati
      show_logo ~ max_val + offset_fixed,
      TRUE ~ NA_real_
    ),
    logo_y = PLAYER_draft,
    logo_size = 0.1
  )

# Grafico barre orizzontali con loghi più a destra senza modificare limite
ggplot(dati_barre, aes(y = PLAYER_draft, x = Valore, fill = PLAYER_draft)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(x = Valore / 2, label = round(Valore, 1)),
    color = "white",
    size = 5,
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_image(
    data = dati_long %>% filter(show_logo == TRUE),
    aes(x = logo_x, y = logo_y, image = logo_path, size = logo_size),
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Variabile, scales = "free_x", ncol = 2) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = player_colors) +
  scale_size_identity() +
  labs(x = "Quantità", y = "Giocatore (Posizione al Draft)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 12), # Titolo asse Y (se lo vuoi)
    axis.text.y = element_text(size = 13, face = "bold"),    # Nomi dei giocatori
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title.x = element_text(size = 12),
    plot.margin = margin(5.5, 50, 5.5, 5.5)  # più spazio a destra per i loghi
  )


# --- GRAFICO SINGOLO ---

# Preparo i dati con la stessa logica degli altri grafici
dati_g <- dati %>%
  filter(as.character(PLAYER) %in% mvp_names) %>%
  mutate(
    PLAYER_draft = paste0(as.character(PLAYER), " (", `OVERALL PICK`, ")")
  ) %>%
  select(PLAYER_draft, PLAYER, G, `NBA SEASON`, `YEAR`)

# Ordino in base a NBA SEASON
ordina_giocatori_g <- dati_g %>%
  arrange(desc(`YEAR`)) %>%
  pull(PLAYER_draft)

# Aggiungi i loghi (duplicazione LeBron)
dati_g_loghi <- dati_g %>%
  rowwise() %>%
  mutate(logo_path = list(get_logos_multiple(as.character(PLAYER)))) %>%
  unnest(cols = c(logo_path)) %>%
  ungroup() %>%
  mutate(
    PLAYER_draft = factor(PLAYER_draft, levels = ordina_giocatori_g)
  )

# Dataset per barre ed etichette (senza duplicazioni LeBron)
dati_g_barre <- dati_g_loghi %>%
  group_by(PLAYER_draft) %>%
  slice(1) %>%
  ungroup()

# ✅ Calcolo della posizione orizzontale centrata per le etichette
dati_g_barre <- dati_g_barre %>%
  mutate(label_x = G / 2)

# Aggiungi loghi (duplicazione LeBron) e posizionamento dei loghi uno affiancato all'altro
dati_g_loghi <- dati_g_loghi %>%
  mutate(
    # Posizione loghi di LeBron uno accanto all'altro
    logo_x = case_when(
      PLAYER == "LeBron James" & logo_path == "loghi/cavaliers.png" ~ G + max(G, na.rm = TRUE) * 0.04,  # Primo logo
      PLAYER == "LeBron James" & logo_path == "loghi/heats.png" ~ G + max(G, na.rm = TRUE) * 0.12,      # Secondo logo
      TRUE ~ G + max(G, na.rm = TRUE) * 0.04  # Altri loghi
    ),
    logo_y = as.numeric(PLAYER_draft)  # Posizione verticale sopra la barra
  )

# Grafico finale con barre orizzontali, etichette centrate e loghi uno accanto all'altro sopra
ggplot(dati_g_barre, aes(x = G, y = PLAYER_draft, fill = PLAYER_draft)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Barre orizzontali
  geom_text(
    aes(x = label_x, y = PLAYER_draft, label = G),  # Etichetta al centro della barra orizzontale
    size = 5,
    color = "white",
    hjust = 0.5
  ) +
  geom_image(
    data = dati_g_loghi,
    aes(x = logo_x, y = logo_y, image = logo_path),  # Posizioniamo i loghi sopra la barra
    size = 0.07,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(breaks = breaks_fun_g) +
  scale_fill_manual(values = player_colors) +
  labs(x = "Partite giocate", y = "Giocatore (Posizione al Draft)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 12), # Titolo asse Y (se lo vuoi)
    axis.text.y = element_text(size = 13, face = "bold"),    # Nomi dei giocatori
    axis.text.x = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title.x = element_text(size = 12),
    plot.margin = margin(5.5, 50, 5.5, 5.5)  # Maggiore spazio per i loghi
  )

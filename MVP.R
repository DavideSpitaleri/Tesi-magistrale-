library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(tibble)
library(ggimage)  # Per inserire immagini

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

# Assegna loghi in base al giocatore
get_logo <- function(player) {
  case_when(
    player == "LeBron James" ~ "loghi/cavaliers.png",
    player == "Derrick Rose" ~ "loghi/bulls.png",
    player == "Kevin Durant" ~ "loghi/thunder.png",
    player == "Stephen Curry" ~ "loghi/warriors.png",
    player == "Russell Westbrook" ~ "loghi/thunder.png",
    player == "James Harden" ~ "loghi/rockets.png",
    player == "Giannis Antetokounmpo" ~ "loghi/bucks.png",
    player == "Nikola Jokić" ~ "loghi/nuggets.png",
    player == "Joel Embiid" ~ "loghi/sixers.png",
    TRUE ~ NA_character_
  )
}

# Prepara dati per facet wrap
dati_mvp <- dati %>%
  filter(as.character(PLAYER) %in% mvp_names) %>%
  mutate(
    PLAYER_draft = paste0(as.character(PLAYER), " (", `OVERALL PICK`, ")"),
    logo_path = get_logo(as.character(PLAYER))
  ) %>%
  select(PLAYER_draft, `NBA SEASON`, PTS, RB, AST, logo_path)

# Passaggio a formato long per facet_wrap
dati_long <- dati_mvp %>%
  pivot_longer(cols = c(`NBA SEASON`, PTS, AST, RB),
               names_to = "Variabile", values_to = "Valore")

# Ordina giocatori in base a NBA SEASON decrescente
ordina_giocatori <- dati_mvp %>%
  arrange(desc(`NBA SEASON`)) %>%
  pull(PLAYER_draft)

dati_long <- dati_long %>%
  mutate(
    PLAYER_draft = factor(PLAYER_draft, levels = ordina_giocatori),
    Variabile = factor(Variabile, levels = c("NBA SEASON", "PTS", "AST", "RB"))
  )

# Funzione per breaks asse y
breaks_fun <- function(x) seq(0, ceiling(max(x) / 5) * 5, by = 5)

# Creo un vettore che indica se mostrare il logo o no (solo per i primi 2 grafici del facet)
dati_long <- dati_long %>%
  mutate(show_logo = ifelse(Variabile %in% c("NBA SEASON", "PTS"), TRUE, FALSE))

# Grafico facet con loghi solo sopra "NBA SEASON" e "PTS"
ggplot(dati_long, aes(x = PLAYER_draft, y = Valore, fill = PLAYER_draft)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Valore, 1)),
            position = position_stack(vjust = 0.5),
            size = 4, color = "white") +
  geom_image(
    data = dati_long %>% filter(show_logo == TRUE),
    aes(image = logo_path, y = Valore + max(Valore)*0.05), # posiziona sopra barra
    size = 0.06
  ) +
  facet_wrap(~ Variabile, scales = "free_y", ncol = 2) +
  scale_y_continuous(breaks = breaks_fun) +
  scale_fill_manual(values = player_colors) +
  labs(x = "Giocatore (Posizione al Draft)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title.y = element_blank()
  )

# GRAFICO PARTITE GIOCATE CON LOGHI SOPRA LE COLONNE

dati_g <- dati %>%
  filter(as.character(PLAYER) %in% mvp_names) %>%
  mutate(
    PLAYER_draft = paste0(as.character(PLAYER), " (", `OVERALL PICK`, ")"),
    logo_path = get_logo(as.character(PLAYER))
  ) %>%
  select(PLAYER_draft, G, logo_path)

# Ordina i giocatori per partite giocate (G)
ordina_giocatori_g <- dati_g %>%
  arrange(desc(G)) %>%
  pull(PLAYER_draft)

dati_g <- dati_g %>%
  mutate(PLAYER_draft = factor(PLAYER_draft, levels = ordina_giocatori_g))

# Funzione breaks per asse Y per G
breaks_fun_g <- function(x) seq(0, ceiling(max(x) / 200) * 200, by = 200)

# Grafico partite giocate con logo sopra la barra
ggplot(dati_g, aes(x = PLAYER_draft, y = G, fill = PLAYER_draft)) +
  geom_col(show.legend = FALSE) +
  geom_image(aes(image = logo_path, y = G + max(G)*0.05), size = 0.06) +  # logo sopra barra
  geom_text(aes(label = G),
            position = position_stack(vjust = 0.5),
            size = 4, color = "white") +
  scale_y_continuous(breaks = breaks_fun_g) +
  scale_fill_manual(values = player_colors) +
  labs(x = "Giocatore (Posizione al Draft)", y = "Partite giocate") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title.y = element_text(size = 12)
  )



#versione orizzontale
# Ordina i giocatori per NBA SEASON 
ordina_giocatori <- dati_mvp %>%
  arrange(desc(`NBA SEASON`)) %>% 
  pull(PLAYER_draft)

# Applichiamo ordinamento e rinominiamo i facet
dati_long <- dati_long %>%
  mutate(
    PLAYER_draft = factor(PLAYER_draft, levels = ordina_giocatori),
    Variabile = factor(Variabile, 
                       levels = c("Nba season", "Punti", "Assist", "Rimbalzi"))
  )

# Funzione breaks personalizzati
breaks_fun <- function(x) {
  seq(0, ceiling(max(x) / 5) * 5, by = 5)
}

# Grafico finale con asse x = giocatori (ruotato)
ggplot(dati_long, aes(x = PLAYER_draft, y = Valore, fill = PLAYER_draft)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Valore, 1)),
            position = position_stack(vjust = 0.5),
            size = 4, color = "white") +
  facet_wrap(~ Variabile, scales = "free_y", ncol = 2) +
  coord_flip() +
  scale_y_continuous(breaks = breaks_fun) + 
  scale_fill_manual(values = player_colors) +
  labs(x = "Giocatore (Posizione al Draft)", y = NULL) +  # <- Etichetta sull'asse verticale
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
    # <-- axis.title rimosso da qui
  )



#GRAFICO PARTITE GIOCATE
# Crea nuovo dataset solo con G (partite giocate)
dati_g <- dati %>%
  filter(as.character(PLAYER) %in% mvp_names) %>%
  mutate(
    PLAYER_draft = paste0(as.character(PLAYER), " (", `OVERALL PICK`, ")")
  ) %>%
  select(PLAYER_draft, G)

# Ordina giocatori per partite giocate
ordina_giocatori_g <- dati_g %>%
  arrange(desc(G)) %>%
  pull(PLAYER_draft)

dati_g <- dati_g %>%
  mutate(PLAYER_draft = factor(PLAYER_draft, levels = ordina_giocatori_g))

# Funzione breaks per asse X
breaks_fun <- function(x) seq(0, ceiling(max(x) / 200) * 200, by = 200)

# Grafico
ggplot(dati_g, aes(x = G, y = PLAYER_draft, fill = PLAYER_draft)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = G), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_x_continuous(breaks = breaks_fun) +
  scale_fill_manual(values = player_colors) +
  labs(x = "Partite giocate in carriera", y = "Giocatore (Posizione al Draft)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
  )


library(mgcv)
library(dplyr)
library(Metrics)  # per RMSE e MAE

# Funzione per pulire i nomi delle colonne (adatta se serve)
clean_names <- function(df) {
  names(df) <- names(df) %>%
    gsub(" ", "_", .) %>%
    gsub("/", "_", .) %>%
    gsub("%", "perc", .) %>%
    gsub("^([0-9])", "X\\1", .)
  return(df)
}

# Pulisci i nomi di dati_mvp
dati_mvp_clean <- clean_names(dati_mvp)

# Normalizza variabili numeriche se necessario (adatta i nomi)
vars_to_numeric <- c("FGperc", "X3Pperc", "FTperc")
for (var in vars_to_numeric) {
  if(var %in% names(dati_mvp_clean)) {
    dati_mvp_clean[[var]] <- as.numeric(as.character(dati_mvp_clean[[var]]))
  }
}

# Definisci la variabile risposta (adatta se serve)
response <- "NBA_SEASON"

# Definisci predittori: tutte le colonne eccetto risposta e chiavi giocatore
predictors <- setdiff(names(dati_mvp_clean), c(response, "PLAYER_draft", "PLAYER"))

# Formula per modello full e modello nullo (solo intercetta)
full_formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
full_model <- lm(full_formula, data = dati_mvp_clean)
null_model <- lm(as.formula(paste(response, "~1")), data = dati_mvp_clean)

# Selezione stepwise forward
forward_model <- step(null_model, 
                      scope = list(lower = null_model, upper = full_model), 
                      direction = "forward", 
                      trace = 1)

# Estrai variabili selezionate (senza intercetta)
vars_forward <- names(coef(forward_model))[-1]

# Funzione per fit e valutazione di LM e GAM con variabili selezionate
fit_and_eval <- function(vars_selected, dati, response) {
  if(length(vars_selected) == 0) {
    stop("Nessuna variabile selezionata")
  }
  
  # Formula lineare
  formula_lin <- as.formula(paste(response, "~", paste(vars_selected, collapse = "+")))
  lm_mod <- lm(formula_lin, data = dati)
  
  # Formula GAM con spline smoothing (k=5 come esempio)
  formula_gam <- as.formula(paste(response, "~", paste(paste0("s(", vars_selected, ", k=5)"), collapse = "+")))
  gam_mod <- mgcv::gam(formula_gam, data = dati, method = "REML", select = TRUE)
  
  # Predizioni sui dati stessi (attenzione: no test set qui)
  pred_lm <- predict(lm_mod, newdata = dati)
  pred_gam <- predict(gam_mod, newdata = dati)
  
  # Metriche di valutazione
  rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
  mae <- function(actual, predicted) mean(abs(actual - predicted))
  
  metrics_lm <- c(RMSE = rmse(dati[[response]], pred_lm),
                  MAE = mae(dati[[response]], pred_lm),
                  R_squared = summary(lm_mod)$r.squared)
  
  metrics_gam <- c(RMSE = rmse(dati[[response]], pred_gam),
                   MAE = mae(dati[[response]], pred_gam),
                   R_squared = summary(gam_mod)$r.sq)
  
  # AIC e BIC
  aic_lm <- AIC(lm_mod)
  bic_lm <- BIC(lm_mod)
  aic_gam <- AIC(gam_mod)
  bic_gam <- BIC(gam_mod)
  
  # Output
  list(
    lm_model = lm_mod,
    gam_model = gam_mod,
    metrics = data.frame(
      Model = c("Linear Model", "GAM"),
      AIC = c(aic_lm, aic_gam),
      BIC = c(bic_lm, bic_gam),
      RMSE = c(metrics_lm["RMSE"], metrics_gam["RMSE"]),
      MAE = c(metrics_lm["MAE"], metrics_gam["MAE"]),
      R_squared = c(metrics_lm["R_squared"], metrics_gam["R_squared"])
    )
  )
}

# Esegui fit e valutazione solo per selezione forward
result_forward <- fit_and_eval(vars_forward, dati_mvp_clean, response)

# Stampa metriche
print(result_forward$metrics)







#Lm e gam degli mvp
library(mgcv)
library(gam)
library(dplyr)
library(Metrics)  # per RMSE e MAE

# --- Pulisci nomi delle colonne come fatto prima, adattalo se serve
clean_names <- function(df) {
  names(df) <- names(df) %>%
    gsub(" ", "_", .) %>%
    gsub("/", "_", .) %>%
    gsub("%", "perc", .) %>%
    gsub("^([0-9])", "X\\1", .)
  return(df)
}

# Applica clean_names a dati_mvp
dati_mvp_clean <- clean_names(dati_mvp)

# --- Normalizza variabili numeriche se serve (esempio percentuali)
vars_to_numeric <- c("FGperc", "X3Pperc", "FTperc") # Adatta se le colonne ci sono
for (var in vars_to_numeric) {
  if(var %in% names(dati_mvp_clean)) {
    dati_mvp_clean[[var]] <- as.numeric(as.character(dati_mvp_clean[[var]]))
  }
}

# --- Definisci risposta e predittori
response <- "NBA_SEASON"  # o "NBA_SEASON" a seconda del nome pulito

# Tutte le variabili disponibili tranne risposta e chiave giocatore
predictors <- setdiff(names(dati_mvp_clean), c(response, "PLAYER_draft", "PLAYER"))

# --- Stepwise selection (forward e backward) usando lm

# Full model
full_formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
full_model <- lm(full_formula, data = dati_mvp_clean)

# Null model (solo intercetta)
null_model <- lm(as.formula(paste(response, "~1")), data = dati_mvp_clean)

# Forward selection
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward", trace = 1)



# --- Estrai variabili selezionate (nomi puliti)
vars_forward <- names(coef(forward_model))[-1]  # tolgo intercetta


# --- Funzione per fit lm e gam con queste variabili, e valutare su dati_mvp_clean
fit_and_eval <- function(vars_selected, dati, response = response) {
  if(length(vars_selected) == 0) {
    stop("Nessuna variabile selezionata")
  }
  
  # Formula lineare
  formula_lin <- as.formula(paste(response, "~", paste(vars_selected, collapse = "+")))
  lm_mod <- lm(formula_lin, data = dati)
  
  # Formula GAM con smoothing spline
  formula_gam <- as.formula(paste(response, "~", paste(paste0("s(", vars_selected, ", k=5)"), collapse = "+")))
  gam_mod <- mgcv::gam(formula_gam, data = dati, method = "REML", select = TRUE)
  
  # Predizioni su dati stessi (non hai separato train/test, usiamo cross-validation dopo se vuoi)
  pred_lm <- predict(lm_mod, newdata = dati)
  pred_gam <- predict(gam_mod, newdata = dati)
  
  # Metriche di valutazione
  rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
  mae <- function(actual, predicted) mean(abs(actual - predicted))
  
  metrics_lm <- c(RMSE = rmse(dati[[response]], pred_lm),
                  MAE = mae(dati[[response]], pred_lm),
                  R_squared = summary(lm_mod)$r.squared)
  
  metrics_gam <- c(RMSE = rmse(dati[[response]], pred_gam),
                   MAE = mae(dati[[response]], pred_gam),
                   R_squared = summary(gam_mod)$r.sq)
  
  # AIC e BIC
  aic_lm <- AIC(lm_mod)
  bic_lm <- BIC(lm_mod)
  aic_gam <- AIC(gam_mod)
  bic_gam <- BIC(gam_mod)
  
  # Output
  list(
    lm_model = lm_mod,
    gam_model = gam_mod,
    metrics = data.frame(
      Model = c("Linear", "GAM"),
      AIC = c(aic_lm, aic_gam),
      BIC = c(bic_lm, bic_gam),
      RMSE = c(metrics_lm["RMSE"], metrics_gam["RMSE"]),
      MAE = c(metrics_lm["MAE"], metrics_gam["MAE"]),
      R_squared = c(metrics_lm["R_squared"], metrics_gam["R_squared"])
    )
  )
}

# --- Esegui fit e valutazione per forward e backward
result_forward <- fit_and_eval(vars_forward, dati_mvp_clean, response = response)


# --- Visualizza metriche
print(result_forward$metrics)


# Crea tabella xtable da result_forward
xtab <- xtable(result_forward$metrics, 
               caption = "Metriche di valutazione per modelli LM e GAM (MVP)", 
               label = "tab:forward_mvp")

# Stampa codice LaTeX
print(xtab, include.rownames = FALSE, digits = 3)



#forward-backward lm e gam su giocatori mvp (dati_mvp) ho usato datimodello perchè in dati mvp consideravo solo PTS, RB,AST e nba season
# Escludi le variabili non utili o collineari, mantenendo quasi tutto il resto
dati_modello <- subset(dati, select = -c(PLAYER, TEAM, AFFILATION, YEAR, `ROUND NUMBER`, `ROUND PICK`))

# Pulisci i nomi delle colonne
dati_modello_clean <- clean_names(dati_modello)

# Assicurati che la risposta sia presente
response <- "NBA_SEASON"

# Converti variabili percentuali se presenti
vars_to_numeric <- c("FGperc", "X3Pperc", "FTperc") 
for (var in vars_to_numeric) {
  if (var %in% names(dati_modello_clean)) {
    dati_modello_clean[[var]] <- as.numeric(as.character(dati_modello_clean[[var]]))
  }
}

# Definisci i predittori: tutte le variabili tranne la risposta
predictors <- setdiff(names(dati_modello_clean), response)

# Formula full e null per stepwise
full_formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
full_model <- lm(full_formula, data = dati_modello_clean)
null_model <- lm(as.formula(paste(response, "~1")), data = dati_modello_clean)

# Stepwise
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward", trace = 1)
backward_model <- step(full_model, direction = "backward", trace = 1)

# Variabili selezionate
vars_forward <- names(coef(forward_model))[-1]
vars_backward <- names(coef(backward_model))[-1]

# Usa la funzione fit_and_eval per valutare i modelli
result_forward <- fit_and_eval(vars_forward, dati_modello_clean, response = response)
result_backward <- fit_and_eval(vars_backward, dati_modello_clean, response = response)

# Stampa le metriche
print(result_forward$metrics)
print(result_backward$metrics)

view(result_forward$metrics)
view(result_backward$metrics)
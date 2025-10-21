library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(xtable)

dati <- read_excel("M:/Tesi/Dati tesi modified.xlsx", sheet = "Foglio1")

summary(dati)
str(dati)  # Controlla la struttura del dataset

# Convertiamo le colonne che contengono percentuali o numeri come caratteri in numeri
dati$`FG` <- as.numeric(dati$`FG%`)
dati$`3P` <- as.numeric(dati$`3P%`)
dati$`FT` <- as.numeric(dati$`FT%`)

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



# Prepara il dataset eliminando variabili collineari dirette
dati_modello <- subset(dati, select = -c(PLAYER, TEAM, AFFILATION, YEAR, `ROUND NUMBER`, `ROUND PICK` ))

# --- MODELLO INIZIALE (Null model e Full model)
null_model <- lm(`NBA SEASON` ~ 1, data = dati_modello)  # solo intercetta
full_model <- lm(`NBA SEASON` ~ ., data = dati_modello)  # tutti i predittori

# --- FORWARD STEPWISE REGRESSION
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(forward_model)

# --- BACKWARD STEPWISE REGRESSION
backward_model <- step(full_model, direction = "backward")
summary(backward_model)



smp_size <- floor(0.80 * nrow(dati))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dati_modello)), size = smp_size)

dati_train <- dati_modello[train_ind, ]
test <- dati_modello[-train_ind, ]

# --- MODELLO INIZIALE (Null model e Full model)
null_model <- lm(`NBA SEASON` ~ 1, data = dati_train)  # solo intercetta
full_model <- lm(`NBA SEASON` ~ ., data = dati_train)  # tutti i predittori

# --- FORWARD STEPWISE REGRESSION
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(forward_model)

# --- BACKWARD STEPWISE REGRESSION
backward_model <- step(full_model, direction = "backward")
summary(backward_model)


#forward-backward sia lm che gam
library(gam)
library(dplyr)
library(Metrics)  # per RMSE e MAE

# --- Step 0: funzione per pulire nomi colonne
clean_names <- function(df) {
  names(df) <- names(df) %>%
    gsub(" ", "_", .) %>%
    gsub("/", "_", .) %>%
    gsub("%", "perc", .) %>%
    gsub("^([0-9])", "X\\1", .)
  return(df)
}

# --- Pulisci nomi su train e test
dati_train <- clean_names(dati_train)
test <- clean_names(test)

# --- Normalizza tipi di variabili numeriche importanti in train e test
vars_to_numeric <- c("FGperc", "X3Pperc", "FTperc")

normalize_types <- function(train_df, test_df, vars_to_numeric) {
  for (var in vars_to_numeric) {
    train_df[[var]] <- as.numeric(as.character(train_df[[var]]))
    test_df[[var]]  <- as.numeric(as.character(test_df[[var]]))
  }
  list(train = train_df, test = test_df)
}

norm_data <- normalize_types(dati_train, test, vars_to_numeric)
dati_train <- norm_data$train
test <- norm_data$test

# --- Funzione per mappare nomi originali a quelli puliti
map_to_clean_names <- function(orig_names, clean_names_vec) {
  sapply(orig_names, function(nm) {
    nm_clean <- nm %>%
      gsub(" ", "_", .) %>%
      gsub("/", "_", .) %>%
      gsub("%", "perc", .) %>%
      gsub("^([0-9])", "X\\1", .)
    matched <- clean_names_vec[clean_names_vec == nm_clean]
    if(length(matched) == 1) return(matched)
    else stop(paste("Nome non trovato o duplicato:", nm))
  })
}

# --- Estrai variabili dai modelli forward e backward (nomi originali)
vars_forward_orig <- all.vars(formula(forward_model))[-1]
vars_backward_orig <- all.vars(formula(backward_model))[-1]

# --- Converti i nomi originali in nomi puliti corrispondenti a dati_train
vars_forward <- map_to_clean_names(vars_forward_orig, names(dati_train))
vars_backward <- map_to_clean_names(vars_backward_orig, names(dati_train))

# --- Funzione per fit e valutazione modelli
fit_and_eval <- function(vars_selected, dati_train, test, response = "NBA_SEASON") {
  
  # Formula lineare
  formula_lin <- as.formula(paste(response, "~", paste(vars_selected, collapse = "+")))
  
  # Fit modello lineare
  lm_mod <- lm(formula_lin, data = dati_train)
  
  # Formula GAM (con smooth)
  formula_gam <- as.formula(paste(response, "~", paste(paste0("s(", vars_selected, ")"), collapse = "+")))
  
  # Fit modello GAM
  gam_mod <- gam(formula_gam, data = dati_train)
  
  # Predizioni su test
  pred_lm <- predict(lm_mod, newdata = test)
  pred_gam <- predict(gam_mod, newdata = test)
  
  # Metriche di valutazione
  eval_metrics <- function(true, pred) {
    c(
      RMSE = rmse(true, pred),
      MAE = mae(true, pred),
      R_squared = cor(true, pred)^2
    )
  }
  
  metrics_lm <- eval_metrics(test[[response]], pred_lm)
  metrics_gam <- eval_metrics(test[[response]], pred_gam)
  
  # AIC e BIC sui modelli fitted (train)
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

# --- Esempi di esecuzione

result_forward <- fit_and_eval(vars_forward, dati_train, test, response = "NBA_SEASON")
result_backward <- fit_and_eval(vars_backward, dati_train, test, response = "NBA_SEASON")

# --- Visualizza le metriche
print(result_forward$metrics)
print(result_backward$metrics)

# Combina i risultati delle metriche in un'unica tabella
results_table <- rbind(
  cbind(Selection = "Forward", result_forward$metrics),
  cbind(Selection = "Backward", result_backward$metrics)
)

# Stampa la tabella in console
print(results_table)

xtab_results <- xtable(
  results_table,
  caption = "Confronto tra modelli lineari e GAM con selezione forward e backward.",
  label = "tab:forward_backward_confronto"
)

print(xtab_results,
      include.rownames = FALSE,
      digits = c(0, 0, 4, 4, 4, 4, 4),  # AIC, BIC, RMSE, MAE, R²
      type = "latex")


#gam su dati_train (tutte variabili)
# Carica la libreria
library(mgcv)

# Lista delle variabili predittive (quelle che vuoi usare nel GAM)
vars <- c("OVERALL_PICK", "G", "TMP", "TPTS", "TRB",
          "TAST", "MP", "PTS", "RB", "AST", "WS", "WS_48", "BPM", "VORP",
          "FGperc", "X3Pperc", "FTperc")

# Crea la formula dinamicamente: NBA_SEASON ~ s(VAR1) + s(VAR2) + ...
formula_gam_all <- as.formula(
  paste("NBA_SEASON ~", paste(paste0("s(", vars, ")"), collapse = " + "))
)

# Allenamento del modello GAM sul training set
gam_all <- mgcv::gam(formula_gam_all, data = dati_train, method = "REML", select = TRUE)


# Riassunto del modello
summary(gam_all)

# Predizione sul test set
pred_all_gam <- predict(gam_all, newdata = test)

# Funzioni di errore (RMSE e MAE) se non già definite
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

# Calcolo errori
rmse_all_gam <- rmse(test$NBA_SEASON, pred_all_gam)
mae_all_gam <- mae(test$NBA_SEASON, pred_all_gam)

# Output
cat("GAM multivariato:\n")
cat("RMSE:", rmse_all_gam, "\n")
cat("MAE:", mae_all_gam, "\n")
# Calcolo AIC e BIC
aic_all_gam <- AIC(gam_all)
bic_all_gam <- BIC(gam_all)

# Output aggiuntivo
cat("AIC:", round(aic_all_gam, 2), "\n")
cat("BIC:", round(bic_all_gam, 2), "\n")










#faccio un gam con tutte le variabili insieme
# Librerie necessarie
# Librerie necessarie
library(mgcv)
library(dplyr)
library(Metrics)

# --- 0. Pulizia nomi colonne per evitare errori nei modelli
clean_names <- function(df) {
  names(df) <- names(df) %>%
    gsub(" ", "_", .) %>%
    gsub("/", "_", .) %>%
    gsub("%", "perc", .) %>%
    gsub("^([0-9])", "X\\1", .)
  return(df)
}

dati <- clean_names(dati)

# --- 1. Suddividi in train / test
set.seed(123)
smp_size <- floor(0.80 * nrow(dati))
train_ind <- sample(seq_len(nrow(dati)), size = smp_size)

dati_train <- dati[train_ind, ]
dati_test  <- dati[-train_ind, ]

# --- 2. Separa le variabili
response_var <- "NBA_SEASON"

# Elenco tutte le variabili (escludendo la risposta)
all_vars <- setdiff(names(dati_train), response_var)

# Variabili fattoriali → lineari
factor_vars <- names(dati_train)[sapply(dati_train, is.factor)]
factor_vars <- setdiff(factor_vars, response_var)  # escludi la risposta se è factor

# Variabili numeriche → smooth
numeric_vars <- setdiff(all_vars, factor_vars)

# --- 3. Costruisci formula GAM
terms_smooth <- paste0("s(", numeric_vars, ")")
terms_linear <- factor_vars

# Unisci tutto nella formula finale
full_formula <- as.formula(
  paste(response_var, "~", paste(c(terms_smooth, terms_linear), collapse = " + "))
)

# --- 4. Fit del modello GAM
gam_mod <- gam(full_formula, data = dati_train, method = "REML", select = TRUE)

# --- 5. Valutazione su test
pred <- predict(gam_mod, newdata = dati_test)
obs <- dati_test[[response_var]]

# --- 6. Metriche
rmse_val <- rmse(obs, pred)
mae_val <- mae(obs, pred)
r2_val <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

aic_val <- AIC(gam_mod)
bic_val <- BIC(gam_mod)

# --- 7. Risultati in tabella
results <- data.frame(
  Model = "GAM (tutte le variabili)",
  AIC = aic_val,
  BIC = bic_val,
  RMSE = rmse_val,
  MAE = mae_val,
  R_squared = r2_val
)

# --- 8. Visualizza
View(results)

# --- 9. (Opzionale) Plot
plot(gam_mod, pages = 1, se = TRUE, shade = TRUE)
summary(gam_mod)




#risultati del modello con variabili che ho scelto io
library(mgcv)
library(dplyr)

response_var <- "NBA_SEASON"

# Identifica variabili numeriche e fattoriali
all_vars <- setdiff(names(dati), response_var)
factor_vars <- names(dati)[sapply(dati, is.factor)]
numeric_vars <- setdiff(all_vars, factor_vars)

# Filtra numeriche con almeno 4 valori unici
numeric_vars_filtered <- numeric_vars[sapply(dati[, numeric_vars], function(x) length(unique(na.omit(x))) >= 4)]

# Costruisci formula GAM
terms_smooth <- paste0("s(", numeric_vars_filtered, ")")
terms_linear <- factor_vars

full_formula <- as.formula(
  paste(response_var, "~", paste(c(terms_smooth, terms_linear), collapse = " + "))
)

# Rimuovi righe con NA nel dataset per variabili usate nel modello
vars_usate <- c(response_var, numeric_vars_filtered, factor_vars)
dati_clean <- dati %>% filter(complete.cases(select(., all_of(vars_usate))))

# Fit modello GAM
gam_mod <- gam(NBA_SEASON ~ ..., data = dati_clean, family = poisson(), select = TRUE)

summary(gam_mod)

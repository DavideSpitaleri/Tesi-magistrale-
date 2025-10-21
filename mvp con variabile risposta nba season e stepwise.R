library(readxl)
library(dplyr)
library(MASS)
library(mgcv)

# Funzioni RMSE e MAE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Caricamento dati (adatta al tuo percorso)
dati <- read_excel("M:/Tesi/Dati tesi modified.xlsx", sheet = "Foglio1")
names(dati) <- gsub(" ", "_", names(dati))
names(dati) <- gsub("%", "perc", names(dati))
names(dati) <- gsub("/", "_", names(dati))
names(dati) <- gsub("3", "tre", names(dati))

dati$FGperc <- as.numeric(dati$FGperc)
dati$trePperc <- as.numeric(dati$trePperc)
dati$FTperc <- as.numeric(dati$FTperc)
dati$MP <- as.numeric(gsub("min", "", dati$MP))
dati$PTS <- as.numeric(dati$PTS)
dati$RB <- as.numeric(dati$RB)
dati$AST <- as.numeric(dati$AST)
dati$WS <- as.numeric(dati$WS)
dati$WS_48 <- as.numeric(dati$WS_48)
dati$BPM <- as.numeric(dati$BPM)
dati$VORP <- as.numeric(dati$VORP)

dati$PLAYER <- as.factor(dati$PLAYER)
dati$AFFILATION <- as.factor(dati$AFFILATION)
dati$TEAM <- as.factor(dati$TEAM)
dati$YEAR <- as.factor(dati$YEAR)

# Lista MVP
mvp_list <- c("Joel Embiid", "Nikola Jokić", "Giannis Antetokounmpo",
              "James Harden", "Russell Westbrook", "Stephen Curry",
              "Kevin Durant", "Derrick Rose", "LeBron James")

mvp_data <- dati %>% filter(PLAYER %in% mvp_list)

# Variabili da usare
vars <- c("NBA_SEASON", "OVERALL_PICK", "WS", "G", "VORP")
mvp_data_sub <- dplyr::select(mvp_data, vars)



# --- LM Stepwise ---

full_lm <- lm(NBA_SEASON ~ ., data = mvp_data_sub)
null_lm <- lm(NBA_SEASON ~ 1, data = mvp_data_sub)

lm_forward <- stepAIC(null_lm, scope = list(lower=null_lm, upper=full_lm), direction = "forward", trace=FALSE)
lm_backward <- stepAIC(full_lm, direction = "backward", trace=FALSE)

# --- GAM Forward manual ---

gam_null <- gam(NBA_SEASON ~ 1, data = mvp_data_sub)

# Costruiamo modelli con ogni singolo smooth
gam_candidates <- list(
  s_OVERALL_PICK = gam(NBA_SEASON ~ s(OVERALL_PICK, k=5), data = mvp_data_sub),
  s_WS = gam(NBA_SEASON ~ s(WS, k=5), data = mvp_data_sub),
  s_G = gam(NBA_SEASON ~ s(G, k=5), data = mvp_data_sub),
  s_VORP = gam(NBA_SEASON ~ s(VORP, k=5), data = mvp_data_sub)
)

# Calcola AIC e scegli migliore variabile
aics <- sapply(gam_candidates, AIC)
best_var <- names(which.min(aics))
gam_forward <- gam_candidates[[best_var]]

# (Per semplicità fermiamoci qui per forward)

# --- GAM Backward manual ---

# Full GAM con tutte smooth
gam_full <- gam(NBA_SEASON ~ 
                  s(OVERALL_PICK, k=5) + 
                  s(WS, k=5) + 
                  s(G, k=5) + 
                  s(VORP, k=5), 
                data = mvp_data_sub)


# Prova a togliere un smooth alla volta e calcola AIC
gam_drop1 <- list(
  drop_OVERALL_PICK = update(gam_full, . ~ . - s(OVERALL_PICK)),
  drop_WS = update(gam_full, . ~ . - s(WS)),
  drop_G = update(gam_full, . ~ . - s(G)),
  drop_VORP = update(gam_full, . ~ . - s(VORP))
)

aics_drop <- sapply(gam_drop1, AIC)
best_drop <- names(which.min(aics_drop))

gam_backward <- gam_drop1[[best_drop]]

# --- Funzione metriche ---

get_metrics <- function(model, data, response = "NBA_SEASON") {
  pred <- predict(model, newdata = data)
  actual <- data[[response]]
  c(AIC = AIC(model),
    BIC = BIC(model),
    RMSE = rmse(actual, pred),
    MAE = mae(actual, pred))
}

# --- Costruzione tabella ---

results <- data.frame(
  Modello = c("LM", "LM", "GAM", "GAM"),
  Selezione = c("Forward", "Backward", "Forward", "Backward"),
  AIC = NA,
  BIC = NA,
  RMSE = NA,
  MAE = NA
)

results[1, 3:6] <- get_metrics(lm_forward, mvp_data_sub)
results[2, 3:6] <- get_metrics(lm_backward, mvp_data_sub)
results[3, 3:6] <- get_metrics(gam_forward, mvp_data_sub)
results[4, 3:6] <- get_metrics(gam_backward, mvp_data_sub)

print(results)

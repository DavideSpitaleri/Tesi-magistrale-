library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(xtable)
library(gam)

dati <- read_excel("M:/Tesi/Dati tesi modified.xlsx", sheet = "Foglio1")

summary(dati)
str(dati)  # Controlla la struttura del dataset
# Rinomina tutte le colonne problematiche in dati
names(dati) <- gsub(" ", "_", names(dati))
names(dati) <- gsub("%", "perc", names(dati))
names(dati) <- gsub("/", "_", names(dati))
names(dati) <- gsub("3", "tre", names(dati))
# Convertiamo le colonne che contengono percentuali o numeri come caratteri in numeri
dati$`FGperc` <- as.numeric(dati$`FGperc`)
dati$`trePperc` <- as.numeric(dati$`trePperc`)
dati$`FTperc` <- as.numeric(dati$`FTperc`)

# Rimuoviamo gli eventuali caratteri (ad esempio 'min' o altri simboli) per altre colonne
dati$`MP` <- as.numeric(gsub("min", "", dati$`MP`))  # Minuti di gioco
dati$`PTS` <- as.numeric(dati$`PTS`)  # Punti
dati$`RB` <- as.numeric(dati$`RB`)  # Rimbalzi
dati$`AST` <- as.numeric(dati$`AST`)  # Assisti
dati$`WS` <- as.numeric(dati$`WS`)  # Win Shares
dati$`WS_48` <- as.numeric(dati$`WS_48`)  # Win Shares per 48 minuti
dati$`BPM` <- as.numeric(dati$`BPM`)  # Box Plus-Minus
dati$`VORP` <- as.numeric(dati$`VORP`)  # Value Over Replacement Player
dati$PLAYER <- as.factor(dati$PLAYER)
dati$AFFILATION <- as.factor(dati$AFFILATION)
dati$TEAM <- as.factor(dati$TEAM)
dati$YEAR <- as.factor(dati$YEAR)

str(dati)  # Controlla la struttura aggiornata del dataset
summary(dati)



library(mgcv)
# ROUND_PICK
mod_lm2 <- gam(NBA_SEASON ~ ROUND_PICK, data = dati)
mod_gam2 <- gam(NBA_SEASON ~ s(ROUND_PICK, bs = "cr"), data = dati)
cat("\n=== ROUND_PICK ===\n")
cat("AIC modello lineare: ", AIC(mod_lm2), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm2)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm2)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam2))
anova(mod_lm2, mod_gam2, test = "Chisq")

# OVERALL_PICK
mod_lm3 <- gam(NBA_SEASON ~ OVERALL_PICK, data = dati)
mod_gam3 <- gam(NBA_SEASON ~ s(OVERALL_PICK, bs = "cr"), data = dati)
cat("\n=== OVERALL_PICK ===\n")
cat("AIC modello lineare: ", AIC(mod_lm3), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm3)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm3)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam3))
anova(mod_lm3, mod_gam3, test = "Chisq")

# G
mod_lm4 <- gam(NBA_SEASON ~ G, data = dati)
mod_gam4 <- gam(NBA_SEASON ~ s(G, bs = "cr"), data = dati)
cat("\n=== G ===\n")
cat("AIC modello lineare: ", AIC(mod_lm4), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm4)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm4)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam4))
anova(mod_lm4, mod_gam4, test = "Chisq")

# TMP
mod_lm5 <- gam(NBA_SEASON ~ TMP, data = dati)
mod_gam5 <- gam(NBA_SEASON ~ s(TMP, bs = "cr"), data = dati)
cat("\n=== TMP ===\n")
cat("AIC modello lineare: ", AIC(mod_lm5), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm5)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm5)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam5))
anova(mod_lm5, mod_gam5, test = "Chisq")

# TPTS
mod_lm6 <- gam(NBA_SEASON ~ TPTS, data = dati)
mod_gam6 <- gam(NBA_SEASON ~ s(TPTS, bs = "cr"), data = dati)
cat("\n=== TPTS ===\n")
cat("AIC modello lineare: ", AIC(mod_lm6), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm6)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm6)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam6))
anova(mod_lm6, mod_gam6, test = "Chisq")

# TRB
mod_lm7 <- gam(NBA_SEASON ~ TRB, data = dati)
mod_gam7 <- gam(NBA_SEASON ~ s(TRB, bs = "cr"), data = dati)
cat("\n=== TRB ===\n")
cat("AIC modello lineare: ", AIC(mod_lm7), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm7)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm7)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam7))
anova(mod_lm7, mod_gam7, test = "Chisq")

# TAST
mod_lm8 <- gam(NBA_SEASON ~ TAST, data = dati)
mod_gam8 <- gam(NBA_SEASON ~ s(TAST, bs = "cr"), data = dati)
cat("\n=== TAST ===\n")
cat("AIC modello lineare: ", AIC(mod_lm8), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm8)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm8)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam8))
anova(mod_lm8, mod_gam8, test = "Chisq")

# MP
mod_lm9 <- gam(NBA_SEASON ~ MP, data = dati)
mod_gam9 <- gam(NBA_SEASON ~ s(MP, bs = "cr"), data = dati)
cat("\n=== MP ===\n")
cat("AIC modello lineare: ", AIC(mod_lm9), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm9)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm9)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam9))
anova(mod_lm9, mod_gam9, test = "Chisq")

# PTS
mod_lm10 <- gam(NBA_SEASON ~ PTS, data = dati)
mod_gam10 <- gam(NBA_SEASON ~ s(PTS, bs = "cr"), data = dati)
cat("\n=== PTS ===\n")
cat("AIC modello lineare: ", AIC(mod_lm10), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm10)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm10)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam10))
anova(mod_lm10, mod_gam10, test = "Chisq")

# RB
mod_lm11 <- gam(NBA_SEASON ~ RB, data = dati)
mod_gam11 <- gam(NBA_SEASON ~ s(RB, bs = "cr"), data = dati)
cat("\n=== RB ===\n")
cat("AIC modello lineare: ", AIC(mod_lm11), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm11)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm11)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam11))
anova(mod_lm11, mod_gam11, test = "Chisq")

# AST
mod_lm12 <- gam(NBA_SEASON ~ AST, data = dati)
mod_gam12 <- gam(NBA_SEASON ~ s(AST, bs = "cr"), data = dati)
cat("\n=== AST ===\n")
cat("AIC modello lineare: ", AIC(mod_lm12), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm12)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm12)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam12))
anova(mod_lm12, mod_gam12, test = "Chisq")

# WS
mod_lm13 <- gam(NBA_SEASON ~ WS, data = dati)
mod_gam13 <- gam(NBA_SEASON ~ s(WS, bs = "cr"), data = dati)
cat("\n=== WS ===\n")
cat("AIC modello lineare: ", AIC(mod_lm13), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm13)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm13)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam13))
anova(mod_lm13, mod_gam13, test = "Chisq")

# WS_48
mod_lm14 <- gam(NBA_SEASON ~ WS_48, data = dati)
mod_gam14 <- gam(NBA_SEASON ~ s(WS_48, bs = "cr"), data = dati)
cat("\n=== WS_48 ===\n")
cat("AIC modello lineare: ", AIC(mod_lm14), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm14)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm14)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam14))
anova(mod_lm14, mod_gam14, test = "Chisq")

# BPM
mod_lm15 <- gam(NBA_SEASON ~ BPM, data = dati)
mod_gam15 <- gam(NBA_SEASON ~ s(BPM, bs = "cr"), data = dati)
cat("\n=== BPM ===\n")
cat("AIC modello lineare: ", AIC(mod_lm15), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm15)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm15)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam15))
anova(mod_lm15, mod_gam15, test = "Chisq")

# VORP
mod_lm16 <- gam(NBA_SEASON ~ VORP, data = dati)
mod_gam16 <- gam(NBA_SEASON ~ s(VORP, bs = "cr"), data = dati)
cat("\n=== VORP ===\n")
cat("AIC modello lineare: ", AIC(mod_lm16), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm16)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm16)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam16))
anova(mod_lm16, mod_gam16, test = "Chisq")

# FGperc
mod_lm17 <- gam(NBA_SEASON ~ FGperc, data = dati)
mod_gam17 <- gam(NBA_SEASON ~ s(FGperc, bs = "cr", k=3), data = dati)
cat("\n=== FGperc ===\n")
cat("AIC modello lineare: ", AIC(mod_lm17), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm17)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm17)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam17))
anova(mod_lm17, mod_gam17, test = "Chisq")

# treP_perc
mod_lm18 <- gam(NBA_SEASON ~ trePperc, data = dati)
mod_gam18 <- gam(NBA_SEASON ~ s(trePperc, bs = "cr"), data = dati)
cat("\n=== treperc ===\n")
cat("AIC modello lineare: ", AIC(mod_lm18), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm18)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm18)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam18))
anova(mod_lm18, mod_gam18, test = "Chisq")

# FT_perc
mod_lm19 <- gam(NBA_SEASON ~ FTperc, data = dati)
mod_gam19 <- gam(NBA_SEASON ~ s(FTperc, bs = "cr"), data = dati)
cat("\n=== FT_perc ===\n")
cat("AIC modello lineare: ", AIC(mod_lm19), "\n")
cat("Smoothness penalty criterion (sp.criterion): ", summary(mod_lm19)$sp.criterion, "\n")
cat("R squared modello lineare: ", summary(mod_lm19)$r.sq, "\n\n")
cat("Summary modello GAM:\n")
print(summary(mod_gam19))
anova(mod_lm19, mod_gam19, test = "Chisq")



library(Metrics)  # Per RMSE e MAE

# Lista delle variabili predittive analizzate
vars <- c("ROUND_PICK", "OVERALL_PICK", "G", "TMP", "TPTS", "TRB",
          "TAST", "MP", "PTS", "RB", "AST", "WS", "WS_48", "BPM", "VORP",
          "FGperc", "trePperc", "FTperc")

# Inizializza la tabella
results <- data.frame(
  Variable = character(),
  Model = character(),
  AIC = numeric(),
  BIC = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

# Loop per raccogliere le metriche per ogni variabile
for (var in vars) {
  # Modelli
  mod_lm <- gam(as.formula(paste("NBA_SEASON ~", var)), data = dati)
  mod_gam <- gam(as.formula(paste("NBA_SEASON ~ s(", var, ", bs = 'cr')")), data = dati)
  
  # Predizioni
  pred_lm <- predict(mod_lm, newdata = dati)
  pred_gam <- predict(mod_gam, newdata = dati)
  
  # Calcolo metriche
  actual <- dati$NBA_SEASON
  
  results <- rbind(
    results,
    data.frame(
      Variable = var,
      Model = "Linear",
      AIC = AIC(mod_lm),
      BIC = BIC(mod_lm),
      RMSE = rmse(actual, pred_lm),
      MAE = mae(actual, pred_lm),
      R_squared = summary(mod_lm)$r.sq
    ),
    data.frame(
      Variable = var,
      Model = "GAM",
      AIC = AIC(mod_gam),
      BIC = BIC(mod_gam),
      RMSE = rmse(actual, pred_gam),
      MAE = mae(actual, pred_gam),
      R_squared = summary(mod_gam)$r.sq
    )
  )
}

# Visualizza la tabella ordinata per variabile
print(results)
tabella_xtable <- xtable(results, caption = "Risultati dei modelli GAM e Linear", label = "tab:risultati_modelli")
print(tabella_xtable, include.rownames = FALSE, type = "latex")




set.seed(123)  # Per riproducibilità
sample_size <- floor(0.8 * nrow(dati))
train_indices <- sample(seq_len(nrow(dati)), size = sample_size)

train_data <- dati[train_indices, ]
test_data <- dati[-train_indices, ]

# Modelli allenati sul training set
mod_lm <- gam(NBA_SEASON ~ PTS, data = train_data)
mod_gam <- gam(NBA_SEASON ~ s(PTS, bs = "cr", k = 5), data = train_data)

# Predizioni sul test set
pred_lm <- predict(mod_lm, newdata = test_data)
pred_gam <- predict(mod_gam, newdata = test_data)

# Valori reali
actual <- test_data$NBA_SEASON

# Funzioni RMSE e MAE (se non usi il pacchetto Metrics)
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

# Calcola le metriche
cat("Modello Lineare:\n")
cat("RMSE:", rmse(actual, pred_lm), "\n")
cat("MAE:", mae(actual, pred_lm), "\n\n")

cat("Modello GAM:\n")
cat("RMSE:", rmse(actual, pred_gam), "\n")
cat("MAE:", mae(actual, pred_gam), "\n")

vars <- c("ROUND_PICK", "OVERALL_PICK", "G", "TMP", "TPTS", "TRB",
          "TAST", "MP", "PTS", "RB", "AST", "WS", "WS_48", "BPM", "VORP",
          "FGperc", "trePperc", "FTperc")  # Escludiamo ROUND_NUMBER

# Inizializza tabella risultati
test_results <- data.frame(
  Variable = character(),
  Model = character(),
  RMSE = numeric(),
  MAE = numeric(),
  stringsAsFactors = FALSE
)

# Loop su tutte le variabili
for (var in vars) {
  # Costruzione formula dinamica
  formula_lm <- as.formula(paste("NBA_SEASON ~", var))
  formula_gam <- as.formula(paste("NBA_SEASON ~ s(", var, ", bs='cr', k=5)"))
  
  # Modelli sul training set
  mod_lm <- gam(formula_lm, data = train_data)
  mod_gam <- gam(formula_gam, data = train_data)
  
  # Predizioni sul test
  pred_lm <- predict(mod_lm, newdata = test_data)
  pred_gam <- predict(mod_gam, newdata = test_data)
  
  # Evita errori su variabili con pochi valori unici (es. ROUND_NUMBER)
  if (any(is.na(pred_lm)) | any(is.na(pred_gam))) next
  
  # Calcolo errori
  rmse_lm <- rmse(test_data$NBA_SEASON, pred_lm)
  mae_lm <- mae(test_data$NBA_SEASON, pred_lm)
  rmse_gam <- rmse(test_data$NBA_SEASON, pred_gam)
  mae_gam <- mae(test_data$NBA_SEASON, pred_gam)
  
  # Aggiungi alla tabella
  test_results <- rbind(
    test_results,
    data.frame(Variable = var, Model = "Linear", RMSE = rmse_lm, MAE = mae_lm),
    data.frame(Variable = var, Model = "GAM", RMSE = rmse_gam, MAE = mae_gam)
  )
}

# Visualizza tabella finale
print(test_results)
tabella_test <- xtable(test_results,
                       caption = "Performance dei modelli Linear e GAM sul test set",
                       label = "tab:risultati_test")
print(tabella_test,
      include.rownames = FALSE,
      digits = c(0, 0, 0, 3, 3),  # Arrotonda RMSE e MAE a 3 decimali
      type = "latex")


library(mgcv)
# Lista variabili predittive (numeric)
vars <- c("ROUND_PICK", "OVERALL_PICK", "G", "TMP", "TPTS", "TRB",
          "TAST", "MP", "PTS", "RB", "AST", "WS", "WS_48", "BPM", "VORP",
          "FGperc", "trePperc", "FTperc")

# Costruisci formula GAM multivariata
formula_gam_all <- as.formula(
  paste("NBA_SEASON ~", paste0("s(", vars, ")", collapse = " + "))
)

# Fitting del GAM multivariato
gam_all <- gam(formula_gam_all, data = train_data, method = "REML", select = TRUE)

# Riassunto del modello
summary(gam_all)
# Predizione
pred_all_gam <- predict(gam_all, newdata = test_data)

# Errori
rmse_all_gam <- rmse(test_data$NBA_SEASON, pred_all_gam)
mae_all_gam <- mae(test_data$NBA_SEASON, pred_all_gam)

cat("GAM multivariato:\n")
cat("RMSE:", rmse_all_gam, "\n")
cat("MAE:", mae_all_gam, "\n")

# Crea tabella risultati GAM multivariato
multivar_results <- data.frame(
  Model = "GAM multivariato",
  RMSE = rmse_all_gam,
  MAE = mae_all_gam
)
xtab_multivar <- xtable(multivar_results,
                        caption = "Performance del modello GAM multivariato sul test set",
                        label = "tab:gam_multivariato")
print(xtab_multivar,
      include.rownames = FALSE,
      digits = c(0, 0, 3, 3),  # 3 decimali per RMSE e MAE
      type = "latex")




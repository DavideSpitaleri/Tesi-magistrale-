library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)

# Carica i dati
dati <- read_excel("M:/Tesi/Dati tesi modified.xlsx", sheet = "Foglio1")

# Converti le colonne percentuali da caratteri a numeri
dati$`FG%` <- as.numeric(dati$`FG%`)
dati$`3P%` <- as.numeric(dati$`3P%`)
dati$`FT%` <- as.numeric(dati$`FT%`)

# Per comodità, rinominiamo le colonne con nomi validi
dati <- dati %>%
  rename(
    FG = `FG%`,
    X3P = `3P%`
  )

# Costruisci il modello GAM
mod_gam <- gam(PTS ~ s(MP) + s(FG) + s(X3P) + s(AST), data = dati)

# Sommario
summary(mod_gam)

# Grafico degli effetti stimati
par(mfrow = c(2, 2))
plot(mod_gam, shade = TRUE, main = "Effetti stimati (GAM)")

# Aggiunta dei residui
dati$pred <- predict(mod_gam)
dati$residui <- residuals(mod_gam)

# Grafico residui vs predetti
plot(dati$pred, dati$residui,
     xlab = "Valori Predetti",
     ylab = "Residui",
     main = "Residui vs Predetti")
abline(h = 0, col = "red", lty = 2)

# Istogramma dei residui
hist(dati$residui, main = "Distribuzione dei residui", xlab = "Residui", breaks = 20)

# QQ plot dei residui
qqnorm(dati$residui)
qqline(dati$residui, col = "blue")


# Assicurati che le colonne siano numeriche
dati$`OVERALL PICK` <- as.numeric(dati$`OVERALL PICK`)
dati$`NBA SEASON` <- as.numeric(dati$`NBA SEASON`)

# Modello di regressione lineare
mod <- lm(`NBA SEASON` ~ `OVERALL PICK`, data = dati)
pred <- fitted(mod)
residui <- residuals(mod)

# Variabili per il grafico
x <- dati$`OVERALL PICK`
y <- dati$`NBA SEASON`

# Crea il grafico
plot(x, y,
     pch = 19, col = "firebrick3",
     xlab = "Posizione al Draft (OVERALL PICK)",
     ylab = "Stagioni NBA giocate",
     main = "Relazione tra Posizione al Draft e Carriera NBA",
     ylim = range(c(y, pred)))

# Aggiungi la retta di regressione
abline(mod, col = "blue", lwd = 2)

# Aggiungi le linee dei residui
for (i in 1:length(x)) {
  colore <- ifelse(residui[i] > 0, "goldenrod", "gray40")
  lines(c(x[i], x[i]), c(y[i], pred[i]), col = colore)
}

# Aggiungi griglia
grid()

# Annotazioni
text(x = min(x, na.rm=TRUE) + 5, y = max(y, na.rm=TRUE) - 1, labels = expression(d[j] > 0), col = "goldenrod", cex = 0.9)
text(x = max(x, na.rm=TRUE) - 10, y = min(y, na.rm=TRUE) + 1, labels = expression(d[j] < 0), col = "gray40", cex = 0.9)

# Carica il pacchetto
library(scatterplot3d)

# Usa un sottoinsieme del tuo dataset, ad esempio: OVERALL PICK, NBA SEASON e PTS
dati_sub <- na.omit(dati[, c("OVERALL PICK", "NBA SEASON", "PTS")])
colnames(dati_sub) <- c("X1", "X2", "Y")

# Modello di regressione lineare
mod <- lm(Y ~ X1 + X2, data = dati_sub)
pred <- predict(mod)

# Crea il grafico 3D
s3d <- scatterplot3d(dati_sub$X1, dati_sub$X2, dati_sub$Y,
                     pch = 19, color = "blue",
                     xlab = "OVERALL PICK",
                     ylab = "NBA SEASON",
                     zlab = "PTS",
                     angle = 15)

# Aggiungi il piano di regressione
s3d$plane3d(mod)

# Aggiungi le linee dei residui
for(i in 1:nrow(dati_sub)) {
  s3d$points3d(
    c(dati_sub$X1[i], dati_sub$X1[i]),
    c(dati_sub$X2[i], dati_sub$X2[i]),
    c(dati_sub$Y[i], pred[i]),
    type = "l", col = "orange"
  )
}



















# Dati di esempio
set.seed(1)
x <- 1:30
y <- 1.2 * x + rnorm(30, mean = 0, sd = 3)

# Modello di regressione lineare
mod <- lm(y ~ x)
y_pred <- fitted(mod)

# Crea il grafico
plot(x, y,
     pch = 19, col = "firebrick3",
     xlab = "grandezza X", ylab = "grandezza Y",
      
     ylim = range(c(y, y_pred)))

# Aggiungi la retta di regressione
abline(mod, col = "blue", lwd = 2)

# Aggiungi residui colorati: arancione per d_j > 0, grigio per d_j < 0
for (i in 1:length(x)) {
  residuo <- y[i] - y_pred[i]
  colore <- ifelse(residuo > 0, "goldenrod", "gray40")
  lines(c(x[i], x[i]), c(y[i], y_pred[i]), col = colore, lty = 1)
}

# Aggiungi griglia
grid()

# Annotazioni per d_j > 0 e d_j < 0
text(x = 5, y = y[5] + 5, labels = expression(d[j] > 0), col = "goldenrod", cex = 0.8)
text(x = 25, y = y[25] - 5, labels = expression(d[j] < 0), col = "gray40", cex = 0.8)

# Salva l'immagine (opzionale)
# png("retta_regressione_scarti.png", width = 800, height = 600)
# (ripeti qui il codice del grafico)
# dev.off()

install.packages("scatterplot3d")
library(scatterplot3d)

# Dataset di esempio
set.seed(123)
n <- 50
X1 <- runif(n, 1, 30)
X2 <- runif(n, 1, 30)
Y <- 5 + 2*X1 + 3*X2 + rnorm(n, 0, 5)
data <- data.frame(Y, X1, X2)

# Modello di regressione multipla
model <- lm(Y ~ X1 + X2, data=data)

# Valori predetti
pred <- predict(model)

# Scatterplot 3D con piano di regressione
s3d <- scatterplot3d(data$X1, data$X2, data$Y,
                     pch=19, color="blue",
                     xlab="X1", ylab="X2", zlab="Y",
                     main="Regressione lineare multipla con residui",
                     theta=120,  # rotazione orizzontale
                     phi=30)     # inclinazione verticale

# Aggiungiamo il piano di regressione
s3d$plane3d(model)

# Aggiungiamo i residui (linee verticali tra i punti osservati e il piano)
for(i in 1:n){
  s3d$points3d(c(data$X1[i], data$X1[i]), 
               c(data$X2[i], data$X2[i]), 
               c(data$Y[i], pred[i]), 
               type="l", col="orange")
}

library(scatterplot3d)

set.seed(123)
n <- 50
X1 <- runif(n, 1, 30)
X2 <- runif(n, 1, 30)
Y <- 5 + 2*X1 + 3*X2 + rnorm(n, 0, 5)
data <- data.frame(Y, X1, X2)

model <- lm(Y ~ X1 + X2, data=data)
pred <- predict(model)

# Cambiamo angolo con 'angle' (rotazione orizzontale)
s3d <- scatterplot3d(data$X1, data$X2, data$Y,
                     pch=19, color="blue",
                     xlab="X1", ylab="X2", zlab="Y",
                     main="Regressione lineare multipla con residui",
                     angle=15)  # angolo di rotazione orizzontale

# Aggiungiamo il piano di regressione
s3d$plane3d(model)

# Residui (linee verticali)
for(i in 1:n){
  s3d$points3d(c(data$X1[i], data$X1[i]), 
               c(data$X2[i], data$X2[i]), 
               c(data$Y[i], pred[i]), 
               type="l", col="orange")
}

library(scatterplot3d)

set.seed(123)
n <- 50
X1 <- runif(n, 1, 30)
X2 <- runif(n, 1, 30)
Y <- 5 + 2*X1 + 3*X2 + rnorm(n, 0, 5)
data <- data.frame(Y, X1, X2)

model <- lm(Y ~ X1 + X2, data=data)
pred <- predict(model)

s3d <- scatterplot3d(data$X1, data$X2, data$Y,
                     pch=19, color="blue",
                     xlab=expression(X[1]),
                     ylab=expression(X[2]),
                     zlab="Y",
                     
                     angle=15)

s3d$plane3d(model)

for(i in 1:n){
  s3d$points3d(c(data$X1[i], data$X1[i]),
               c(data$X2[i], data$X2[i]),
               c(data$Y[i], pred[i]),
               type="l", col="orange")
}

library(mgcv)
# Simuliamo dati di esempio
set.seed(123)
n <- 200
x1 <- runif(n, 0, 10)
x2 <- runif(n, 0, 5)
y <- 3 + sin(x1) + log(x2 + 1) + rnorm(n, 0, 0.5)

data <- data.frame(y, x1, x2)

# Fit del modello GAM
gam_mod <- gam(y ~ s(x1) + s(x2), data = data)

# Predizioni
data$fit <- predict(gam_mod)

# Residui grezzi
data$resid <- residuals(gam_mod, type = "response")

# Grafico dei termini smooth
par(mfrow = c(2,2))

plot(gam_mod, shade = TRUE, main = "Effetti smooth di x1 e x2")

# Grafico residui vs predetti
plot(data$fit, data$resid,
     xlab = "Valori predetti",
     ylab = "Residui grezzi",
     main = "Residui vs Valori predetti")
abline(h = 0, col = "red", lty = 2)

# Istogramma dei residui
hist(data$resid, main = "Distribuzione dei residui", xlab = "Residui", breaks = 20)

# QQ plot dei residui per normalità
qqnorm(data$resid)
qqline(data$resid, col = "blue")




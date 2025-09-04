# title: "Advanced Statistical Models - Project Work"
# subtitle: "Master in Artificial Intelligence and Data Science a.a. 2024/2025"
# author: "Francesca Ricci - Maria Rotella - Marco Longo"

# Load data
original_data <- read.csv("returns.csv")


# Preprocessing
returns <- original_data
colnames(returns) <- c("Dates",
                       "SHCOMP_CN","SPX_US","NKY_JP","HSI_HK",
                       "KOSPI_KR","STI_SG","ASX_AU","NZSE_NZ")
returns$Dates <- as.Date(returns$Dates)
any(is.na(returns))


# Analisi preliminare

library(ggplot2)
library(tidyr)
library(dplyr)

colori_indici <- c(
  "SHCOMP_CN" = "blue", 
  "SPX_US" = "red", 
  "NKY_JP" = "green", 
  "HSI_HK" = "purple", 
  "KOSPI_KR" = "orange", 
  "STI_SG" = "pink", 
  "ASX_AU" = "brown", 
  "NZSE_NZ" = "gray"
)

returns_pvt <- returns %>% pivot_longer(cols = -Dates, names_to = "Index", values_to = "Value")

returns_pvt <- returns_pvt %>%
  mutate(Index = recode(Index, 
                        "SHCOMP Index" = "SHCOMP_CN",
                        "SPX Index" = "SPX_US",
                        "NKY Index" = "NKY_JP",
                        "HSI Index" = "HSI_HK",
                        "KOSPI Index" = "KOSPI_KR",
                        "STI Index" = "STI_SG",
                        "ASX Index" = "ASX_AU",
                        "NZSE:IND" = "NZSE_NZ"
  ))

ggplot(returns_pvt, aes(x = Dates, y = Value, color = Index)) +
  geom_line(alpha = 0.5) +
  labs(title = "Rendimenti nel tempo", x = "", y = "Valori") +
  scale_color_manual(values = colori_indici) +  
  theme_minimal() +
  theme(legend.text = element_text(size = 5))


library(reshape2) 

ggplot(returns_pvt, aes(x = Dates, y = Value, color = Index)) +
  geom_line() +
  facet_wrap(~Index, scales = "free_y") +  
  labs(title = "Rendimenti degli indici nel tempo", x = "", y = "Rendimento") +
  scale_color_manual(values = colori_indici) +
  theme_minimal() +
  theme(legend.position = "none")


library(zoo)

crisis_periods <- data.frame(
  Start = as.Date(c("2008-09-01", "2011-06-01", "2015-06-01")),
  End = as.Date(c("2009-06-30", "2012-01-31", "2016-02-29"))
)

returns_vltlt <- returns_pvt %>% group_by(Index) %>%
  mutate(Volatility = rollapply(Value, width = 30, FUN = sd, fill = NA, align = "right"))

ggplot(returns_vltlt, aes(x = Dates, y = Volatility, color = Index)) +
  geom_line() +
  facet_wrap(~Index, scales = "free_y") +  
  geom_rect(data = crisis_periods, inherit.aes = FALSE,
            aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.2) +  
  labs(title = "Volatilità Mobile degli Indici con Periodi di Crisi",
       x = "", y = "Deviazione Standard (30 giorni)") +
  scale_color_manual(values = colori_indici) +  
  theme_minimal()+
  theme(legend.position = "none")


ggplot(returns_vltlt, aes(x = Index, y = Volatility, fill = Index)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Confronto della Volatilità tra gli Indici",
       x = "", y = "Deviazione Standard Mobile (30 giorni)") +
  scale_fill_manual(values = colori_indici) +  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 5)  
  )


window_size <- 252

rolling_corr <- function(series1, series2, window_size) {
  rollapplyr(1:length(series1), width = window_size, 
             FUN = function(i) cor(series1[i], series2[i], use = "complete.obs"), 
             by.column = FALSE, fill = NA)
}

index_names <- colnames(returns)[-1]
rolling_corr_matrix <- data.frame(Date = returns$Date)

for (i in index_names) {
  if (i != "SPX_US") {  
    rolling_corr_matrix[[i]] <- rolling_corr(returns[[i]], returns$SPX_US, window_size)
  }
}

rolling_corr_long <- rolling_corr_matrix %>%
  pivot_longer(cols = -Date, names_to = "Index", values_to = "Value")

ggplot(rolling_corr_long, aes(x = Date, y = Value, color = Index)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +  
  theme_minimal() +
  labs(title = "Correlazione Mobile (252 giorni) rispetto a SPX_US",
       x = "", y = "Correlazione", color = "") +
  scale_color_manual(values = colori_indici) +
  scale_y_continuous(limits = c(-0.25, 1)) +  
  theme(legend.position = "bottom")


library(tidyverse)
library(lubridate)
library(forecast)

returns_ts <- ts(returns$`SPX_US`, start = c(year(min(returns$Date))), frequency = 252)  

decomp <- stl(returns_ts, s.window = "periodic")

decomp_df <- data.frame(
  Data = as.numeric(time(returns_ts)),
  Dati = decomp$time.series[, "remainder"] + decomp$time.series[, "trend"] + decomp$time.series[, "seasonal"],
  Stagionale = decomp$time.series[, "seasonal"],
  Trend = decomp$time.series[, "trend"],
  Residuo = decomp$time.series[, "remainder"]
)

decomp_long <- pivot_longer(decomp_df, cols = -Data, names_to = "Componente", values_to = "Valore")

ggplot(decomp_long, aes(x = Data, y = Valore)) +
  geom_line(color = "blue") +
  facet_wrap(~ Componente, scales = "free_y", ncol = 1) +
  labs(title = "Decomposizione STL dei Ritorni SPX_US", x = "Anno", y = "Valore") +
  theme_minimal()


returns$Weekday <- weekdays(returns$Date)
returns$Month <- format(returns$Date, "%m")

returns <- returns %>%
  mutate(Weekday = factor(Weekday, 
                          levels = c("lunedì", "martedì", "mercoledì", "giovedì", "venerdì"), 
                          labels = c("Lun", "Mar", "Mer", "Gio", "Ven")))

ggplot(returns, aes(x = Weekday, y = SPX_US, fill = Weekday)) +
  geom_boxplot() + theme_minimal() + scale_fill_brewer(palette = "Pastel1") +  
  labs(title = "Distribuzione dei rendimenti SPX_US per giorno della settimana", x="", y="Rendimento") + 
  theme(legend.position = "none")

returns <- returns %>%
  mutate(Month = factor(Month, 
                        levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                        labels = c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic")))

ggplot(returns, aes(x = Month, y = SPX_US, fill = Month)) +
  geom_boxplot() +
  theme_minimal()  +
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Distribuzione dei rendimenti SPX_US per mese", x = "", y = "Rendimento") +
  theme(legend.position = "none") 


library(tseries)

returns_clnd <- returns[, sapply(returns, is.numeric)]
returns_clnd <- returns_clnd[complete.cases(returns_clnd), ]

adf_results <- sapply(returns_clnd, function(x) adf.test(x)$p.value)

print(adf_results)


library(rugarch)

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
garch_fit <- ugarchfit(spec = garch_spec, data = returns$SPX)
coef(garch_fit)

volatility_forecast <- sigma(garch_fit)

plot(returns$Dates, volatility_forecast, type = 'l', col = "blue", lwd = 2,
     main = "Volatilità prevista (GARCH(1,1))",
     xlab = "", ylab = "Volatilità")


### HMM (Hidden Markov Models) e HSMM (Hidden Semi-Markov Models)

library(mhsmm)
set.seed(123)
K <- 2
start.val.k2 <- hmmspec(init = rep(1/K, K), trans = matrix(c(0.8, 0.2, 0.3, 0.7), 
                                                           nrow = K, ncol = K, byrow = TRUE),
                        parms.emis = list(mu = rep(mean(returns$SPX_US), K),
                                          sigma = rep(sd(returns$SPX_US), K)),
                        dens.emis = dnorm.hsmm)

mod.hmm.k2 <- hmmfit(returns$SPX_US, start.val.k2, mstep = mstep.norm)
summary(mod.hmm.k2)

set.seed(123)
states.k2 <- mod.hmm.k2$yhat

plot(returns$Dates, returns$SPX_US, col=states.k2,
     main = "Modello HMM per SPX_US (k=2)", 
     xlab = "", ylab = "Valore dell'Indice", xaxt = "n")  

axis(1, at = seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), 
     labels = format(
       seq(from=min(returns$Dates), to=max(returns$Dates), by="year"), "%Y")
)


set.seed(123)

cols <- c("SHCOMP_CN", "SPX_US", "NKY_JP", "HSI_HK", "KOSPI_KR", "STI_SG", "ASX_AU", "NZSE_NZ")

plot_list <- list()
K <- 2

par(mfrow = c(8, 1), mar = c(3, 2, 2, 1))

for (i in seq_along(cols)) {
  colname <- cols[i]
  series <- returns[[colname]]
  dates <- returns$Dates
  
  start.val <- hmmspec(
    init = rep(1/K, K),
    trans = matrix(c(0.8, 0.2, 0.3, 0.7), nrow = K, byrow = TRUE),
    parms.emis = list(mu = rep(mean(series), K), sigma = rep(sd(series), K)),
    dens.emis = dnorm.hsmm
  )
  
  # Fit del modello
  fit <- hmmfit(series, start.val, mstep = mstep.norm)
  states <- fit$yhat 
  
  plot(dates, series, col = states,
       main = paste("HMM per", colname),
       xlab = "", ylab = "", xaxt = "n")
  
  axis(1, at = seq(from = min(dates), to = max(dates), by = "year"),
       labels = format(seq(from = min(dates), to = max(dates), by = "year"), "%Y"))
}


set.seed(123)
K <- 3

trans_mat <- matrix(c(0.8, 0.2, 0.3, 0.7), nrow = K, ncol = K, byrow = TRUE)
trans_mat <- trans_mat / rowSums(trans_mat)

series <- list(x = matrix(returns$SPX_US, ncol = 1), N = length(returns$SPX_US))

start.val.k3 <- hmmspec(
  init = rep(1/K, K),
  trans = trans_mat,
  parms.emis = list(mu = rep(mean(returns$SPX_US), K),
                    sigma = rep(sd(returns$SPX_US), K)),
  dens.emis = dnorm.hsmm
)

mod.hmm.k3 <- hmmfit(returns$SPX_US, start.val.k3, mstep = mstep.norm)
summary(mod.hmm.k3)

states.hmm.k3 <- mod.hmm.k3$yhat

plot(returns$Dates, returns$SPX_US, col=states.hmm.k3,
     main = "Modello HMM per SPX_US (k=3)", 
     xlab = "", ylab = "Valore dell'Indice", xaxt = "n")  

axis(1, at = seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), 
     labels = format(seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), "%Y"))



set.seed(123)

plot_list <- list()
K <- 3

cols <- c("SPX_US", "NKY_JP", "HSI_HK", "KOSPI_KR", "STI_SG", "ASX_AU", "NZSE_NZ")

trans_mat <- matrix(1/K, nrow = K, ncol = K)
diag(trans_mat) <- 0.8
trans_mat <- trans_mat / rowSums(trans_mat)

par(mfrow = c(7, 1), mar = c(3, 2, 2, 1))

# Ciclo su ogni indice
for (i in seq_along(cols)) {
  colname <- cols[i]
  series_data <- returns[[colname]]
  dates <- returns$Dates
  
  # Pulisci i dati
  valid_idx <- !is.na(series_data) & is.finite(series_data)
  series_data <- series_data[valid_idx]
  dates <- dates[valid_idx]
  
  # Prepara serie per hmmfit
  series <- list(x = matrix(series_data, ncol = 1), N = length(series_data))
  
  # Inizializzazione robusta dei parametri
  quantiles <- quantile(series_data, probs = seq(0.1, 0.9, length.out = K))
  mu_init <- as.numeric(quantiles)
  sigma_init <- rep(sd(series_data, na.rm = TRUE) + 1e-6, K)
  
  start.val <- hmmspec(
    init = rep(1/K, K),
    trans = trans_mat,
    parms.emis = list(mu = mu_init, sigma = sigma_init),
    dens.emis = dnorm.hsmm
  )
  
  fit <- hmmfit(series, start.val, mstep = mstep.norm)
  states <- fit$yhat
  
  plot(dates, series_data, col = states, pch = 16, cex = 0.5,
       main = paste("HMM con", K, "stati per", colname),
       xlab = "", ylab = "Return", xaxt = "n")
  
  axis(1, at = seq(from = min(dates), to = max(dates), by = "year"),
       labels = format(seq(from = min(dates), to = max(dates), by = "year"), "%Y"))
}


min_len <- min(length(mod.hmm.k2$loglik), length(mod.hmm.k3$loglik))

plot(mod.hmm.k2$loglik[1:min_len], type = "b", col = "blue", 
     ylab = "Log-likelihood", xlab = "Iterazioni", 
     main = "Confronto Log-likelihood per K=2 e K=3", 
     ylim = range(c(mod.hmm.k2$loglik, mod.hmm.k3$loglik)))

lines(mod.hmm.k3$loglik[1:min_len], type = "b", col = "red")

legend("bottomright", legend = c("K=2", "K=3"), col = c("blue", "red"), lty = 1, pch = 1)



n_obs <- length(returns$SPX_US)

# calcolo dei parametri per HMM con k=2
loglik.hmm.k2 <- tail(mod.hmm.k2$loglik, 1) 

n_params.k2 <- length(unlist(mod.hmm.k2$model$parms.emission)) + 
  length(mod.hmm.k2$model$init) + 
  length(mod.hmm.k2$model$transition) 

aic.hmm.k2 <- -2 * loglik.hmm.k2 + 2 * n_params.k2
bic.hmm.k2 <- -2 * loglik.hmm.k2 + log(n_obs) * n_params.k2


# calcolo dei parametri per HMM con k=3
loglik.hmm.k3 <- tail(mod.hmm.k3$loglik, 1) 

n_params.k3 <- length(unlist(mod.hmm.k3$model$parms.emission)) + 
  length(mod.hmm.k3$model$init) + 
  length(mod.hmm.k3$model$transition) 

aic.hmm.k3 <- -2 * loglik.hmm.k3 + 2 * n_params.k3
bic.hmm.k3 <- -2 * loglik.hmm.k3 + log(n_obs) * n_params.k3

table <- data.frame(
  HMM.2 = c(loglik.hmm.k2, aic.hmm.k2, bic.hmm.k2),
  HMM.3 = c(loglik.hmm.k3, aic.hmm.k3, bic.hmm.k3)
)

rownames(table) <- c("Log-likelihood", "AIC", "BIC")
print(table)


set.seed(123)

K=3

init_probs <- rep(1/K, K)

# Matrice di transizione (diagonale a zero, nessuna autotransizione)
trans_matrix <- matrix(c(0,.8,.2,.3,0,.7,.5,.5,0), nrow = K, byrow = TRUE)

parms.emis = list(mu = rnorm(K, mean = mean(returns$SPX_US), sd = sd(returns$SPX_US)),
                  sigma = rep(sd(returns$SPX_US)^2, K))

# Distribuzione di durata (Poisson) per ciascuno stato
sojourn <- list(lambda = c(15, 15, 15), shift = c(1, 1, 1), type = 'poisson') 

start.val.hsmm <- hsmmspec(
  init = init_probs,
  trans = trans_matrix,
  parms.emis = parms.emis,
  sojourn = sojourn,
  dens.emission = dnorm.hsmm
)
train.data <- list(x = returns$SPX_US, N = length(returns$SPX_US))

mod.hsmm.k3 <- hsmmfit(train.data, start.val.hsmm, 
                       mstep = mstep.norm, M = 50)

summary(mod.hsmm.k3)


states.hsmm.k3 <- mod.hsmm.k3$yhat

plot(returns$Dates, returns$SPX_US, col=states.hsmm.k3,
     main = "Modello HSMM per SPX_US (k=3)", 
     xlab = "", ylab = "Valore dell'Indice", xaxt = "n")  

axis(1, at = seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), 
     labels = format(seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), "%Y"))


n_obs <- length(returns$SPX_US)

loglik.hsmm.k3 <- tail(mod.hsmm.k3$loglik, 1) 

n_params.k3 <- length(unlist(mod.hsmm.k3$model$parms.emission)) + 
  length(mod.hsmm.k3$model$init) + 
  length(mod.hsmm.k3$model$transition) 

aic.hsmm.k3 <- -2 * loglik.hsmm.k3 + 2 * n_params.k3
bic.hsmm.k3 <- -2 * loglik.hsmm.k3 + log(n_obs) * n_params.k3

table <- data.frame(
  HMM.2 = c(loglik.hmm.k2, aic.hmm.k2, bic.hmm.k2),
  HMM.3 = c(loglik.hmm.k3, aic.hmm.k3, bic.hmm.k3),
  HSMM.3 = c(loglik.hsmm.k3, aic.hsmm.k3, bic.hsmm.k3)
)

rownames(table) <- c("Log-likelihood", "AIC", "BIC")
print(table)


library(gamlss.mx)
set.seed(123)
mod.fmm.k2 <- gamlssMXfits(n=5, returns_clnd$SPX_US ~ 1, data = returns_clnd, family="NO", K=2)


mod.fmm.k2$prob


states.mixt.k2 <- apply(mod.fmm.k2$post.prob, 1, which.max)

plot(returns$Dates, returns$SPX_US, col=states.mixt.k2, 
     main = "Indice SPX_US - Misture gaussiane (k=2)", xlab = "", 
     ylab = "Valore dell'Indice", xaxt = "n")  

axis(1, at = seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), 
     labels = format(seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), "%Y"))


set.seed(123)
mod.fmm.k3 <- gamlssMXfits(n=5, returns_clnd$SPX_US ~ 1, data = returns_clnd, family="NO", K=3)


mod.fmm.k3$prob


states.mixt.k3 <- apply(mod.fmm.k3$post.prob, 1, which.max)

plot(returns$Dates, returns$SPX_US, col=states.mixt.k3,
     main = "Indice SPX_US - Misture gaussiane (k=3)", 
     xlab = "", ylab = "Valore dell'Indice", xaxt = "n")  

axis(1, at = seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), 
     labels = format(seq(from = min(returns$Dates), to = max(returns$Dates), by = "year"), "%Y"))



table <- data.frame(
  FMM.2 = c(mod.fmm.k2$G.deviance, mod.fmm.k2$aic, mod.fmm.k2$sbc),
  FMM.3 = c(mod.fmm.k3$G.deviance, mod.fmm.k3$aic, mod.fmm.k3$sbc)
)

rownames(table) <- c("Deviance", "AIC", "SBC")
print(table)


library(MSGARCH)
set.seed(123)

ms2.gjr.s <- CreateSpec(
  variance.spec = list(model = "gjrGARCH"),
  distribution.spec = list(distribution = "std"),
  switch.spec = list(K = 2),
  constraint.spec = list(regime.const = "nu")
)

fit.ml <- FitML(ms2.gjr.s, data = returns$SPX_US)
summary(fit.ml) 


sqrt(250) * sapply(ExtractStateFit(fit.ml), UncVol)



library(scales)
library(gridExtra)

# Estrai le probabilità smussate per ciascun stato
smooth.prob1 <- State(fit.ml)$SmoothProb[, 1, 1]  # Probabilità per stato 1
smooth.prob2 <- State(fit.ml)$SmoothProb[, 1, 2]  # Probabilità per stato 2

# Scala le probabilità per ciascun stato
smooth_scaled1 <- rescale(smooth.prob1[-1], to = c(-2, 2))
smooth_scaled2 <- rescale(smooth.prob2[-1], to = c(-2, 2))

# Creazione dei grafici per ciascun stato
p1 <- ggplot(returns, aes(x = Dates)) +
  geom_point(aes(y = returns$SPX_US), color = "black", alpha = 0.5) + 
  geom_line(aes(y = smooth_scaled1), color = "lightgreen", size = 1) + 
  labs(title = "Rendimenti Giornalieri e Probabilità Smussata del Regime 1 (SPX_US)",
       x = "Data",
       y = "Valore") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  
    axis.title = element_text(size = 10),  
    axis.text = element_text(size = 8),  
    plot.margin = margin(8, 8, 8, 8)  
  )

p2 <- ggplot(returns, aes(x = Dates)) +
  geom_point(aes(y = returns$SPX_US), color = "black", alpha = 0.5) + 
  geom_line(aes(y = smooth_scaled2), color = "orange", size = 1) + 
  labs(title = "Rendimenti Giornalieri e Probabilità Smussata del Regime 2 (SPX_US)",
       x = "Data",
       y = "Valore") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  
    axis.title = element_text(size = 10),  
    axis.text = element_text(size = 8),  
    plot.margin = margin(8, 8, 8, 8)  
  )

grid.arrange(p1, p2, ncol = 1)


cols <- c("SHCOMP_CN", "SPX_US", "NKY_JP", "HSI_HK", "KOSPI_KR", "STI_SG", "ASX_AU", "NZSE_NZ")

plot_list <- list()

for (i in seq_along(cols)) {
  colname <- cols[i]
  series <- returns[[colname]]
  names(series) <- NULL 
  
  fit <- FitML(ms2.gjr.s, data = series)
  smooth_prob <- State(fit)$SmoothProb[, 1, 2, drop = TRUE] # Probabilità per stato 2
  smooth_scaled <- rescale(smooth_prob[-1], to = c(-2, 2))
  
  df_plot <- data.frame(Dates = returns$Dates, Return = series,Smooth = smooth_scaled)
  
  p <- ggplot(df_plot, aes(x = Dates)) +
    geom_point(aes(y = Return), color = "black", alpha = 0.5) + 
    geom_line(aes(y = Smooth), color = "orange", size = 1) +
    labs(title = paste("Rendimenti e Prob. Smussata del regime 2 -", colname), x = "", y = "") +
    theme_minimal() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme(plot.margin = margin(6, 6, 6, 6),axis.text = element_text(size = 8),
          axis.title = element_text(size = 8), plot.title = element_text(size = 10))
  
  plot_list[[i]] <- p
}

grid.arrange(grobs = plot_list, ncol = 1)



set.seed(123)

ms2.gjr.s3 <- CreateSpec(
  variance.spec = list(model = "gjrGARCH"),
  distribution.spec = list(distribution = "std"),
  switch.spec = list(K = 3),
  constraint.spec = list(regime.const = "nu")
)

fit.ml3 <- FitML(ms2.gjr.s3, data = returns$SPX_US)
summary(fit.ml3) 


smooth.prob1 <- State(fit.ml3)$SmoothProb[, 1, 1]  # Probabilità per stato 1
smooth.prob2 <- State(fit.ml3)$SmoothProb[, 1, 2]  # Probabilità per stato 2
smooth.prob3 <- State(fit.ml3)$SmoothProb[, 1, 3]  # Probabilità per stato 3

smooth_scaled1 <- rescale(smooth.prob1[-1], to = c(-2, 2))
smooth_scaled2 <- rescale(smooth.prob2[-1], to = c(-2, 2))
smooth_scaled3 <- rescale(smooth.prob3[-1], to = c(-2, 2))

p1 <- ggplot(returns, aes(x = Dates)) +
  geom_point(aes(y = returns$SPX_US), color = "black", alpha = 0.5) + 
  geom_line(aes(y = smooth_scaled1), color = "lightgreen", size = 1) + 
  labs(title = "Rendimenti Giornalieri e Probabilità Smussata del Regime 1 (SPX_US)",
       x = "",   y = "Valore") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  
    axis.title = element_text(size = 10),  
    axis.text = element_text(size = 8),  
    plot.margin = margin(8, 8, 8, 8)  
  )

p2 <- ggplot(returns, aes(x = Dates)) +
  geom_point(aes(y = returns$SPX_US), color = "black", alpha = 0.5) + 
  geom_line(aes(y = smooth_scaled2), color = "orange", size = 1) + 
  labs(title = "Rendimenti Giornalieri e Probabilità Smussata del Regime 2 (SPX_US)",
       x = "",   y = "Valore") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  
    axis.title = element_text(size = 10),  
    axis.text = element_text(size = 8),  
    plot.margin = margin(8, 8, 8, 8)  
  )

p3 <- ggplot(returns, aes(x = Dates)) +
  geom_point(aes(y = returns$SPX_US), color = "black", alpha = 0.5) + 
  geom_line(aes(y = smooth_scaled3), color = "indianred", size = 1) + 
  labs(title = "Rendimenti Giornalieri e Probabilità Smussata del Regime 3 (SPX_US)",
       x = "",   y = "Valore") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  
    axis.title = element_text(size = 10),  
    axis.text = element_text(size = 8),  
    plot.margin = margin(8, 8, 8, 8)  
  )

# Visualizza i tre grafici uno sotto l'altro
grid.arrange(p1, p2, p3, ncol = 1)



cols <- c("SPX_US", "NKY_JP", "HSI_HK", "KOSPI_KR", "STI_SG", "ASX_AU", "NZSE_NZ")

plot_list <- list()

for (i in seq_along(cols)) {
  colname <- cols[i]
  series <- returns[[colname]]
  names(series) <- NULL 
  
  fit <- FitML(ms2.gjr.s3, data = series)
  smooth_prob <- State(fit)$SmoothProb[, 1, 3, drop = TRUE] # Probabilità per stato 3
  smooth_scaled <- rescale(smooth_prob[-1], to = c(-2, 2))
  
  df_plot <- data.frame(Dates = returns$Dates, Return = series,Smooth = smooth_scaled)
  
  p <- ggplot(df_plot, aes(x = Dates)) +
    geom_point(aes(y = Return), color = "black", alpha = 0.5) + 
    geom_line(aes(y = Smooth), color = "indianred", size = 1) +
    labs(title = paste("Rendimenti e Prob. Smussata del regime 3 -", colname), x = "", y = "") +
    theme_minimal() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme(plot.margin = margin(6, 6, 6, 6),axis.text = element_text(size = 8),
          axis.title = element_text(size = 8), plot.title = element_text(size = 10))
  
  plot_list[[i]] <- p
}

grid.arrange(grobs = plot_list, ncol = 1)



table <- data.frame(
  GARCH.k2 = c(fit.ml$loglik, AIC(fit.ml), BIC(fit.ml)),
  GARCH.k3 = c(fit.ml3$loglik, AIC(fit.ml3), BIC(fit.ml3))
)

rownames(table) <- c("Log-Likelihood", "AIC", "BIC")
print(table)


table <- data.frame(
  ll = c(loglik.hmm.k2, loglik.hmm.k3, loglik.hsmm.k3, NA, NA, fit.ml$loglik, fit.ml$loglik),
  AIC = c(aic.hmm.k2, aic.hmm.k3, aic.hsmm.k3, mod.fmm.k2$aic, mod.fmm.k3$aic, AIC(fit.ml), AIC(fit.ml3)),
  BIC = c(bic.hmm.k2, bic.hmm.k3, bic.hsmm.k3, mod.fmm.k2$sbc, mod.fmm.k3$sbc, BIC(fit.ml), BIC(fit.ml3))
)

rownames(table) <- c("HMM (k=2)", "HMM (k=3)", "HSMM (k=3)", "FMM (k=2)", "FMM (k=3)", "GARCH (k=2)", "GARCH (k=3)")
print(table)


library(FitUltD)
library(modes)
library(tidyverse)
library(plotly)
library(mclust)
library(distr)
libraries()
ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)
data <- read.csv(paste0(getwd(), "/creditcard.csv"))
cond_fraude <- data[["Class"]] == 1
data_Fraude <- data[cond_fraude, ]
data_No_Fraude <- data[!cond_fraude, ]
summary(data[, c("Time", "Amount")])
monto <- data$Amount
tiempo <- data$Time
monto_1 <- data[data$Class == 1, ]$Amount
monto_0 <- data[data$Class == 0, ]$Amount
tiempo_1 <- data[data$Class == 1, ]$Time
tiempo_0 <- data[data$Class == 0, ]$Time
clas <- data$Class
par(mfrow = c(1, 2))
hist(tiempo, main = "Tiempo")
hist(monto, main = "Monto")
library(ggplot2)
plot.density <- function(X1, cat, title, xlb) {
    Clase <- rbind(data.frame(Fraude = as.factor(cat), DT = X1))
    p <- ggplot(Clase, aes(x = Clase$DT, fill = Clase$Fraude)) + geom_density(alpha = 0.4) + ggtitle(title) + xlab(xlb)
    p
}
plot.bar <- function(X1, cat, title, xlb) {
    Clase <- rbind(data.frame(Fraude = as.factor(cat), DT = X1))
    p <- ggplot(Clase, aes(x = Fraude, y = DT)) + geom_boxplot(alpha = 0.4) + ggtitle(title) + xlab(xlb)
    p
}
pt <- plot.density(X1 = tiempo, cat = as.factor(clas), title = "Densidad Tiempo", xlb = "Tiempo")
pm <- plot.density(X1 = monto, cat = as.factor(clas), title = "Densidad Monto", xlb = "Monto")
m1 <- plot.density(l_monto, cat = as.factor(clas), title = "Densidad log(Monto)", xlb = "log(Monto)")
outl_monto <- monto < quantile(monto, 0.975)
m2 <- plot.density(monto[outl_monto], cat = as.factor(clas[outl_monto]), title = "Densidad Monto percentil 99%", xlb = "Monto")
library(cowplot)
cowplot::plot_grid(pt, pm, nrow = 2)
cowplot::plot_grid(m1, m2, nrow = 2)
ks.test(monto[clas == 1], monto[clas == 0])
ks.test(tiempo[clas == 1], tiempo[clas == 0])
epsilon <- sort(unique(monto))[2]/10
l_monto <- log(monto + epsilon)
hist(tiempo, main = "Tiempo")
hist(l_monto, main = "Log(Monto)")
plot.bar(X1 = tiempo, cat = as.factor(clas), title = "Tiempo", xlb = "Fraude")
plot.bar(X1 = l_monto, cat = as.factor(clas), title = "log(Monto)", xlb = "Fraude")
plot.bar(X1 = monto[outl_monto], cat = as.factor(clas[outl_monto]), title = "Monto percentil 97.5%", xlb = "Fraude")
IR <- quantile(tiempo)
IR_diff <- IR[4] - IR[2]
Inter_Q <- as.numeric(c(IR[2] - IR_diff, IR[4] + IR_diff))
Inter_Q
modes::kurtosis(tiempo, T)
modes::bimodality_coefficient(tiempo) > 5/9
Ajuste_tiempo <- FDist(tiempo, plot = T)
Ajuste_monto <- FDist(monto, plot = T)
A <- data.frame(seed = 0, ks = 0)[-1, ]
for (seed in 1:1460) {
    set.seed(seed)
    muestra_t <- tiempo[sample(1:length(tiempo), floor(length(tiempo) * 0.05))]
    A <- rbind(A, data.frame(seed = seed, ks = ks.test(tiempo, muestra_t)$statistic))
}
set.seed(31109)
set.seed(A[A$ks == min(A[A$ks != 0, ]$ks), 1])
ind <- sample(1:length(tiempo), floor(length(tiempo) * 0.05))
clas[ind]
muestra_tiempo <- tiempo[ind]
muestra_tiempo_0 <- muestra_tiempo[clas[ind] == 0]
smt <- summary(muestra_tiempo)
st <- summary(tiempo)
par(mfrow = c(1, 1))
graphics::plot(density(tiempo), main = "Tiempo vs Muestra Tiempo")
lines(density(muestra_tiempo), col = "red")
Ajuste_tiempo_Full[[1]]
Ajuste_tiempo_Full[[2]]
plot(density(tiempo))
lines(density(Ajuste_tiempo_Full[[2]]), col = "blue")
ks.test(Ajuste_tiempo_Full[[2]], jitter(tiempo, amount = 1))
set.seed(31109)
Ajuste_tiempo_Full <- FDistUlt(X = muestra_tiempo_0, plot = T, p.val_min = 0.01, n.obs = length(muestra_tiempo))
set.seed(31109)
Ajuste_tiempo_Fraud <- FDistUlt(X = tiempo[clas == 1], plot = T, p.val_min = 0.01, n.obs = length(muestra_tiempo))
length(Ajuste_tiempo_Full[[1]])
length(Ajuste_tiempo_Fraud[[1]])
ks.test(Ajuste_tiempo_Full[[2]], jitter(tiempo, amount = 1))
ks.test(Ajuste_tiempo_Fraud[[2]], jitter(tiempo[clas == 1], amount = 1))
summary(Ajuste_tiempo_Fraud[[2]])
Ajuste_tiempo_Full[[4]]
Ajuste_tiempo_Fraud[[4]]
View(Ajuste_tiempo_Fraud[[3]])
Nombres = c("exp", "pois", "beta", "gamma", "lnorm", "norm", "weibull", "nbinom", "hyper", "binom", "unif", "t")
set.seed(31109)
Ajuste_tiempo_Fraud_No_Cauchy <- FDistUlt(X = tiempo[clas == 1], plot = T, p.val_min = 0.01, n.obs = length(muestra_tiempo))
rm("Nombres")
summary(Ajuste_tiempo_Fraud_No_Cauchy[[2]])
summary(Ajuste_tiempo_Full[[2]])
summary(muestra_tiempo_0)
Ajuste_tiempo_Fraud_No_Cauchy[[4]]
View(Ajuste_tiempo_Fraud_No_Cauchy[[3]])
ks.test(Ajuste_tiempo_Fraud_No_Cauchy[[2]], jitter(tiempo[clas == 1], amount = 1))
summary(Ajuste_tiempo_Fraud_No_Cauchy[[2]])
purrr::map(Ajuste_tiempo_Fraud_No_Cauchy[[6]], ~.x$Lim_inf)
View(Ajuste_tiempo_Fraud_No_Cauchy[[6]][[1]])
Ajuste_tiempo_Fraud_No_Cauchy[[6]][[7]]
P.value <- M_candidato <- list()
p_val <- c(0.05, 0.025, 0.015, 0.005, 0.0025, 0.001, 5e-04, 0.00025, 1e-04, 5e-05, 2.5e-05, 1e-05)
n <- 0
Tiempo <- system.time({
    for (i in 1:length(p_val)) {
        set.seed(31109)
        M_candidato[[i]] <- FDistUlt(X = jitter(muestra_tiempo_0, amount = 1), plot = T, crt = 1, p.val_min = p_val[i], n.obs = length(muestra_tiempo_0) * 20)
        for (k in 1:10) {
            n <- n + 1
            P.value[[n]] <- ks.test(sample(M_candidato[[i]][[2]], length(muestra_tiempo_0)), jitter(tiempo, amount = 1))
            print(P.value[[n]]$p.value)
        }
    }
})
P.value_F <- M_candidato_F <- list()
n <- 0
Nombres = c("exp", "pois", "beta", "gamma", "lnorm", "norm", "weibull", "nbinom", "hyper", "binom", "unif", "t")
Tiempo_F <- system.time({
    for (i in 1:length(p_val)) {
        set.seed(31109)
        M_candidato_F[[i]] <- FDistUlt(X = tiempo[clas == 1], plot = T, crt = 1, p.val_min = p_val[i], n.obs = length(tiempo[clas == 1]))
        for (k in 1:10) {
            n <- n + 1
            P.value_F[[n]] <- ks.test(M_candidato_F[[i]][[2]], jitter(tiempo[clas == 1], amount = 1))
            print(P.value_F[[n]]$p.value)
        }
    }
})
rm("Nombres")
purrr::map(M_candidato_F, ~dim(.x[[3]]))
Fit4 <- M_candidato_F[[4]]
summary(Fit4[[2]])
Fit4[[4]]
Fit4[[3]]
Fit4[[6]][[1]]
summary(rweibull(1000, 2.778059, 29809.33))
qqq <- rweibull
formals(qqq)[1] <- length(Fit4[[1]][[1]]())
formals(qqq)[2] <- 2.778059
formals(qqq)[3] <- 29809.33
Fit4[[1]][[1]] <- qqq
semilla <- 31109:31129
k.s <- c()
for (i in semilla) {
    set.seed(i)
    Muestras <- purrr::map(Fit4[[1]], ~.x())
    limits <- purrr::map(Muestras, ~c(min(.x), max(.x)))
    Muestra_completa <- do.call(c, Muestras)
    k.s <- c(k.s, ks.test(Muestra_completa, tiempo[clas == 1])$p.value)
}
summary(Muestra_completa)
summary(tiempo[clas == 1])
set.seed(31109)
Nombres = c("exp", "pois", "beta", "gamma", "lnorm", "weibull", "nbinom", "hyper", "binom", "unif", "t")
Prueba_2 <- FDistUlt(X = tiempo[clas == 1], plot = T, crt = 1, p.val_min = 0.01, n.obs = length(tiempo[clas == 1]), ref = 2)
rm("Nombres")
qqplot(Prueba_2[[2]], tiempo[clas == 1])
summary(Prueba_2[[2]])
ks.test(jitter(tiempo[clas == 1], amount = 1), Prueba_2[[2]])
Prueba_2[[4]]
write.csv(Prueba_2[[3]], "pruebas2.csv")
Tiempo
print(paste0("Horas d e ejecución: ", Tiempo[3]/3600))
M_candidato[[12]][[4]]
DF <- data.frame(p_val_min = rep(p_val, each = 10), NROW = rep(purrr::map_int(M_candidato, ~nrow(.x[[3]])), each = 10), P.VAL = purrr::map_dbl(P.value, ~.x[["p.value"]]))
Mean <- reshape2::dcast(data = DF, formula = p_val_min ~ ., mean, value.var = "P.VAL")
Median <- reshape2::dcast(data = DF, formula = p_val_min ~ ., median, value.var = "P.VAL")
Mean[[1]] <- as.factor(Mean[[1]])
Median[[1]] <- as.factor(Median[[1]])
plot(Mean)
plot(Median)
library(plotly)
plot_ly(x = DF[[1]], y = DF[[2]], z = DF[[3]], type = "scatter3d", mode = "markers", color = DF[[3]])
M_candidato[[12]][[4]]
Estudio <- M_candidato[[12]][[3]]
Funciones_generadoras <- M_candidato[[12]][[1]]
semilla <- 31109:31129
k.s <- c()
for (i in semilla) {
    set.seed(i)
    Muestras <- purrr::map(Funciones_generadoras, ~.x())
    Muestra_completa <- do.call(c, Muestras)
    k.s <- c(k.s, ks.test(Muestra_completa, jitter(tiempo, 1))$p.value)
}
plot(density(k.s))
min(k.s)
mean(k.s)
len <- purrr::map_int(Muestras, length)
len/sum(len) - Estudio$Dist_Prop
qqplot(Muestra_completa, tiempo)
`%c%` <- function(e1, e2) as.numeric(e1) >= as.numeric(e2[1]) & as.numeric(e1) <= as.numeric(e2[2])
muestra_tiempo[muestra_tiempo %c% M_candidato[[12]][[3]][1, c(13, 14)]]
modelo <- 11
Funciones_generadoras <- M_candidato[[modelo]][[1]]
semilla <- 31109:31129
k.s <- c()
for (i in semilla) {
    set.seed(i)
    Muestras <- purrr::map(Funciones_generadoras, ~.x())
    limits <- purrr::map(Muestras, ~c(min(.x), max(.x)))
    Muestra_completa <- do.call(c, Muestras)
    k.s <- c(k.s, ks.test(Muestra_completa, tiempo)$p.value)
}
summary(Muestra_completa)
M_candidato[[7]][[4]]
summary(M_candidato[[7]][[2]])
plot(density(k.s))
M_candidato[[modelo]][[3]]
DF_12 <- M_candidato[[modelo]][[3]]
DF_12 <- A_X[[3]]
sum(DF_12[DF_12$AD_p.v < 0.05 & DF_12$KS_p.v < 0.05, "Dist_Prop"])
sum(DF_12[DF_12$AD_p.v < 0.05 | DF_12$KS_p.v < 0.05, "Dist_Prop"])
sum(DF_12[DF_12$AD_p.v < 0.01 & DF_12$KS_p.v < 0.01, "Dist_Prop"])
sum(DF_12[DF_12$AD_p.v < 0.01 | DF_12$KS_p.v < 0.01, "Dist_Prop"])
plot(density(tiempo))
lines(density(Muestra_completa), col = "red")
M_candidato[[12]][[4]][[1]]
get_dist_args <- function(Dist) {
    firstup <- function(x) {
        FL <- toupper(substr(x, 1, 1))
        if (FL == "G") {
            x <- gsub("gamma", "gammad", x)
        }
        if (FL == "T") {
            x <- gsub("t", "td", x)
        }
        substr(x, 1, 1) <- FL
        x
    }
    text_eval <- function(a) {
        if (class(a) != "character") {
            return()
        }
        eval(parse(text = a))
    }
    purrr::map(c(paste0("distr::", firstup(Dist)), paste0("stats::r", Dist)), ~formalArgs(text_eval(.x)))
}
Args <- purrr::map(c("exp", "pois", "beta", "gamma", "lnorm", "norm", "weibull", "nbinom", "hyper", "cauchy", "binom", "unif", "t"), ~get_dist_args(.x))
get_dist("gamma(364.537, 0.009)")
get_dist <- function(Dist) {
    text_eval <- function(a) {
        if (class(a) != "character") {
            return()
        }
        eval(parse(text = a))
    }
    firstup <- function(x) {
        FL <- toupper(substr(x, 1, 1))
        if (FL == "G") {
            x <- gsub("gamma", "gammad", x)
            param_split <- strsplit(x, ",")[[1]]
            param2 <- param_split[2]
            x <- paste0(param_split[1], ",", 1/text_eval(substr(param2, 1, nchar(param2) - 1)), ")")
        }
        if (FL == "T") {
            x <- gsub("t", "td", x)
        }
        substr(x, 1, 1) <- FL
        x
    }
    text_eval(paste0("distr::", firstup(Dist)))
}
library(distr)
Dists <- purrr::map(as.character(DF_12[, 1]), get_dist)
Tott <- list()
for (i in 1:length(Dists)) {
    Tott[[i]] <- r(Dists[[i]])(DF_12[, "Obs"][[i]])
}
Distss <- do.call("c", Tott)
Distss <- purrr::map2(Dists, DF_12[, "Obs"], ~r(.x)(.y))
limits <- purrr::map(Distss, ~c(min(.x), max(.x)))
max(purrr::map_dbl(limits, max))
plot(density(tiempo))
lines(density(Distss), col = "red")
modelo <- 10
DF_12 <- M_candidato[[modelo]][[3]]
table(DF_12$Dist)
A <- M_candidato[[modelo]][[3]][, 1]
B <- M_candidato[[modelo]][[3]][, "Dist_Prop"] * length(M_candidato[[modelo]][[2]])
Dists <- purrr::map(as.character(A), get_dist)
Distss <- purrr::map2(Dists, B, ~r(.x)(.y))
plot(density(tiempo))
m.a. <- do.call(c, Distss)
lines(density(m.a.), col = "blue")
summary(m.a.)
summary(tiempo)
qtl_diff <- quantile(m.a., 1:100/100) - quantile(tiempo, 1:100/100)
quant <- names(qtl_diff[abs(qtl_diff) == max(abs(qtl_diff))])
quant
quant_n <- 0.18
cond_df <- DF_12[, 13] < quantile(m.a., quant_n) & DF_12[, 14] > quantile(m.a., quant_n)
DF_12[cond_df, ]
ks.test(jitter(tiempo, amount = 1), m.a.)
qqplot(jitter(tiempo, amount = 1), m.a.)
plot(ecdf(tiempo))
lines(ecdf(m.a.), col = "red")
Dist <- UnivarMixingDistribution(Dlist = Dists, mixCoeff = DF_12$Dist_Prop)
plot(Dist)
par(mfrow = c(1, 2))
hist(tiempo, breaks = 500)
hist(m.a., breaks = 500)
par(mfrow = c(1, 1))
muestra <- r(Dist)(25000)
summary(muestra)
summary(tiempo)
ks.test(muestra, tiempo)
ks.test(m.a.[m.a. < quantile(m.a., 0.995) & m.a. > quantile(m.a., 0.005)], tiempo)
plot(sum_D)
pdistodaro <- p(Dist)
library(ADGofTest)
ADGofTest::ad.test(muestra, pdistodaro)
ADGofTest::ad.test(muestra_tiempo, pdistodaro)
plot(density(muestra_tiempo))
lines(density(muestra), col = "blue")
summary(muestra)
summary(muestra_tiempo)
quantile_critic <- function(x, y) {
    qtl_diff <<- quantile(x, 1:100/100) - quantile(y, 1:100/100)
    cond <- abs(qtl_diff) == max(abs(qtl_diff))
    quant <- (1:100)[cond]/100
    diff <- qtl_diff[cond]
    return(c(quant, quantile(x, quant), quantile(y, quant), diff))
}
quantile_critic(muestra, muestra_tiempo)
qtl_diff
KS <- ks.test(jitter(muestra, amount = 1), jitter(muestra_tiempo, amount = 1))
KS
ADGofTest::ad.test(tiempo, pdistodaro)
ks.test(muestra_tiempo, tiempo)
ks.test(muestra_tiempo, muestra)
tiempo
ks.test(M_candidato[[7]][[2]], tiempo)
ks.test(muestra, tiempo)
summary(M_candidato[[7]][[2]])
M_candidato[[7]][[3]]
M_candidato
c(p_val, as.vector(P.value))
Ajuste_tiempo_Full <- FDistUlt(tiempo, plot = T, subplot = T)
Ajuste_monto_Full <- FDistUlt(monto, plot = T, subplot = T)
Ajuste_l_monto_Full <- FDistUlt(l_monto, plot = T, subplot = T)
save.image("~/Lib/Tesis/tesis.RData")
U <- unique(df[["CL"]])
par(mfrow = c(3, 3))
purrr::map(U, ~plot(density(df[df[["CL"]] == .x, 2])))
par(mfrow = c(1, 1))
X <- sort(X)
X[c(F, diff(mod1) != 0)]

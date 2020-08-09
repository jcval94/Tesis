cll<-function () 
{
  ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)
  if (!inherits(ctx, "try-error")) {
    contenido <- ctx[["contents"]]
    file_w <- file(paste0(getwd(),"/new.R"))
    writeLines(((contenido)), file_w)
    writeLines(as.character(parse("new.R")), file_w)
    utils::file.edit(paste0(getwd(),"/new.R"))
  }
  return(invisible())
}



x <- c(rnorm(1000),rnorm(1000,3))
y <- density(x)
Y = c(y[['y']])
X = c(y[['x']])

#  crr <- min(y[['x']])
#  X <- c(y[['x']]-crr)
#  X <- c(X, X + max(X))

p <- TSA::periodogram(Y, plot = F)
dds <- data.frame(freq = 1/p$freq, spec = p$spec, orden = 1:length(p$spec))
dds <- head(dds[order(-dds$spec), ], 10)

periodos <- round(dds[['freq']],0)

for (p in periodos) {
  print(X[p])
}

library(TSA)
library(forecast)
y.dc <- decompose(ts(Y, frequency = 171))
plot(y.dc)

frequency(Y)

plot(y)



N = length(Y)
I = abs(fft(Y)/sqrt(N))^2
P = (4/N) * I
f = (0:floor(N/2))/N
plot(f, I[1:((N/2) + 1)], type = "h", xlab = "frequency", ylab = "", main = "Periodogram of CO2 data after trend removal", col = "blue")


D_F = data.frame(Freq = f, Spec = I[1:((N/2) + 1)])
1/head(D_F[order(-D_F$Spec),'Freq'])

###########################


periodicidad <- function(ts, place = 10) {
  ddT <- data.frame(freq = c(), spec = c(), orden = c())
  ords <- floor(length(ts) * 0.7):length(ts)
  for (lu in ords) {
    p <- TSA::periodogram(ts[1:lu], plot = F)
    dds <- data.frame(freq = 1/p$freq, spec = p$spec, orden = 1:length(p$spec))
    dds <- head(dds[order(-dds$spec), ], place)
    ddT <- rbind(ddT, dds)
  }
  ddT <- ddT[order(-ddT$spec), ]
  Maxi <- max(ddT$spec)
  ddT <- head(ddT[ddT$orden > 2, ], 15)
  ddT$Freq_Orden <- paste0(ddT$freq, "_", ddT$orden)
  ddT <- suppressWarnings(reshape2::dcast(ddT, Freq_Orden ~ ., max, value.var = "spec"))
  ddT$. <- ddT$./Maxi
  ddT <- ddT[order(-ddT$.), ]
  return(list(unique(as.numeric(do.call("rbind", strsplit(ddT$Freq_Orden, "_"))[, 1])), ddT))
}
library(timeSeries)
data(LPP2005REC)
df <- LPP2005REC
periodicidad(df, place = 10)
length(df)
#------------------------------------------------------
# Siguiente intento: entrenar un modelo que obtenga
# los parámetros de una bimodal
# https://www.youtube.com/watch?v=nktiUUd6X_U
# muy bueno
# https://tinyheero.github.io/2016/01/03/gmm-em.html


library("ggplot2")
library("dplyr")
library("reshape2")

options(scipen = 999)
set.seed(1)

comp1.vals <- data_frame(comp = "A", 
                         vals = rnorm(50, mean = 1, sd = 0.5))
comp2.vals <- data_frame(comp = "B", 
                         vals = rnorm(50, mean = 1.5, sd = 0.5))

vals.df <- bind_rows(comp1.vals, comp2.vals)

vals.df %>%
  ggplot(aes(x = vals, y = "A", color = factor(comp))) +
  geom_point(alpha = 0.4) +
  scale_color_discrete(name = "Source of Data") +
  xlab("Values") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top")



wait <- c(rnorm(103,0,4),rnorm(130,7,6))
plot(density(wait))

wait.kmeans <- kmeans(wait, 2)
wait.kmeans.cluster <- wait.kmeans$cluster

wait.df <- data_frame(x = wait, cluster = wait.kmeans.cluster)

wait.summary.df <- wait.df %>%
  group_by(cluster) %>%
  summarize(mu = mean(x), variance = var(x), std = sd(x), size = n())

#################

wait.summary.df <- wait.summary.df %>%
  mutate(alpha = size / sum(size))

#################################################

#' Expectation Step of the EM Algorithm
#'
#' Calculate the posterior probabilities (soft labels) that each component
#' has to each data point.
#'
#' @param sd.vector Vector containing the standard deviations of each component
#' @param sd.vector Vector containing the mean of each component
#' @param alpha.vector Vector containing the mixing weights  of each component
#' @return Named list containing the loglik and posterior.df
e_step <- function(x, mu.vector, sd.vector, alpha.vector) {
  comp1.prod <- dnorm(x, mu.vector[1], sd.vector[1]) * alpha.vector[1]
  comp2.prod <- dnorm(x, mu.vector[2], sd.vector[2]) * alpha.vector[2]
  sum.of.comps <- comp1.prod + comp2.prod
  comp1.post <- comp1.prod / sum.of.comps
  comp2.post <- comp2.prod / sum.of.comps
  
  sum.of.comps.ln <- log(sum.of.comps, base = exp(1))
  sum.of.comps.ln.sum <- sum(sum.of.comps.ln)
  
  list("loglik" = sum.of.comps.ln.sum,
       "posterior.df" = cbind(comp1.post, comp2.post))
}

#' Maximization Step of the EM Algorithm
#'
#' Update the Component Parameters
#'
#' @param x Input data.
#' @param posterior.df Posterior probability data.frame.
#' @return Named list containing the mean (mu), variance (var), and mixing
#'   weights (alpha) for each component.
m_step <- function(x, posterior.df) {
  comp1.n <- sum(posterior.df[, 1])
  comp2.n <- sum(posterior.df[, 2])
  
  comp1.mu <- 1/comp1.n * sum(posterior.df[, 1] * x)
  comp2.mu <- 1/comp2.n * sum(posterior.df[, 2] * x)
  
  comp1.var <- sum(posterior.df[, 1] * (x - comp1.mu)^2) * 1/comp1.n
  comp2.var <- sum(posterior.df[, 2] * (x - comp2.mu)^2) * 1/comp2.n
  
  comp1.alpha <- comp1.n / length(x)
  comp2.alpha <- comp2.n / length(x)
  
  list("mu" = c(comp1.mu, comp2.mu),
       "var" = c(comp1.var, comp2.var),
       "alpha" = c(comp1.alpha, comp2.alpha))
}



for (i in 1:50) {
  if (i == 1) {
    # Initialization
    e.step <- e_step(wait, wait.summary.df[["mu"]], wait.summary.df[["std"]],
                     wait.summary.df[["alpha"]])
    m.step <- m_step(wait, e.step[["posterior.df"]])
    cur.loglik <- e.step[["loglik"]]
    loglik.vector <- e.step[["loglik"]]
  } else {
    # Repeat E and M steps till convergence
    e.step <- e_step(wait, m.step[["mu"]], sqrt(m.step[["var"]]), 
                     m.step[["alpha"]])
    m.step <- m_step(wait, e.step[["posterior.df"]])
    loglik.vector <- c(loglik.vector, e.step[["loglik"]])
    
    loglik.diff <- abs((cur.loglik - e.step[["loglik"]]))
    if(loglik.diff < 1e-6) {
      break
    } else {
      cur.loglik <- e.step[["loglik"]]
    }
  }
}
loglik.vector






plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

data.frame(x = wait) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(m.step$mu[1], sqrt(m.step$var[1]), 
                            lam = m.step$alpha[1]),
                colour = "red", lwd = 1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(m.step$mu[2], sqrt(m.step$var[2]), 
                            lam = m.step$alpha[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density") +
  xlab("Values") +
  ggtitle("Final GMM Fit")







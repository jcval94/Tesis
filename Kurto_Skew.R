set.seed(31109)
n.sample <- rnorm(n = 10000, mean = 55, sd = 4.5)
library(modes)
sk<-skewness(n.sample)
ku<-kurtosis(n.sample,T)

#Histogram
library(ggplot2)
library(modes)
datasim <- data.frame(n.sample)
ggplot(datasim, aes(x = n.sample), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(colour = 'red') + xlab(expression(bold('Simulación 10,000 obs. Normal(0,1)'))) + 
  ylab(expression(bold('Densidad')))+ 
  annotate("text", x = 65, y = .085, label = paste0("Coef. Bimodalidad = ",round(bimodality_coefficient(n.sample),3)))+ 
  annotate("text", x = 65, y = .075, label = paste0("Kurtosis = ",round(ku,3)))+ 
  annotate("text", x = 65, y = .065, label = paste0("Asimet. = ",round(sk,3)))


set.seed(31109)
n.sample <- rexp(n = 10000,rate = 1)
sk<-skewness(n.sample)
ku<-kurtosis(n.sample,T)

#Histogram
datasim <- data.frame(n.sample)
ggplot(datasim, aes(x = n.sample), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(colour = 'red') + xlab(expression(bold('Simulación 10,000 obs. Exp(1)'))) + 
  ylab(expression(bold('Densidad')))+ 
  annotate("text", x = 7.5, y = .7, label = paste0("Coef. Bimodalidad = ",round(bimodality_coefficient(n.sample),3)))+ 
  annotate("text", x = 7.5, y = .65, label = paste0("Kurtosis = ",round(ku,3)))+ 
  annotate("text", x = 7.5, y = .6, label = paste0("Asimet. = ",round(sk,3)))




set.seed(31109)
n.sample <- c(rnorm(n = 5000),rnorm(n = 5000,6))
sk<-skewness(n.sample)
ku<-kurtosis(n.sample,T)

#Histogram
datasim <- data.frame(n.sample)
ggplot(datasim, aes(x = n.sample), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(colour = 'red') + xlab(expression(bold('Simulación 10,000 obs. Normal-Normal'))) + 
  ylab(expression(bold('Densidad')))+ 
  annotate("text", x = 8.5, y = .2, label = paste0("Coef. Bimodalidad = ",round(bimodality_coefficient(n.sample),3)))+ 
  annotate("text", x = 9, y = .18, label = paste0("Kurtosis = ",round(ku,3)))+ 
  annotate("text", x = 9, y = .16, label = paste0("Asimet. = ",round(sk,3)))





set.seed(31109)
n.sample <- c(rnorm(n = 5000),rgamma(n = 5000,10,2))
sk<-skewness(n.sample)
ku<-kurtosis(n.sample,T)

#Histogram
datasim <- data.frame(n.sample)
ggplot(datasim, aes(x = n.sample), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(colour = 'red') + xlab(expression(bold('Simulación 10,000 obs. Normal-Gamma'))) + 
  ylab(expression(bold('Densidad')))+ 
  annotate("text", x = 8.5, y = .2, label = paste0("Coef. Bimodalidad = ",round(bimodality_coefficient(n.sample),3)))+ 
  annotate("text", x = 9, y = .18, label = paste0("Kurtosis = ",round(ku,3)))+ 
  annotate("text", x = 9, y = .16, label = paste0("Asimet. = ",round(sk,3)))

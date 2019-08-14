set.seed(31109)
n.sample <- rnorm(n = 10000, mean = 55, sd = 4.5)
library(modes)
sk<-skewness(n.sample)
ku<-kurtosis(n.sample)

#Histogram
library(ggplot2)
datasim <- data.frame(n.sample)
ggplot(datasim, aes(x = n.sample), binwidth = 2) + 
       geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
       geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples'))) + 
       ylab(expression(bold('Density')))+ annotate("text", x = 70, y = .075, label = paste0("Kurtosis = ",ku," , Asimet. = ",sk))

library(data.table)
library(ggplot2)

ggplot(faithful, aes(waiting)) + geom_density() + 
  geom_vline(xintercept = density(faithful$waiting)$x[which.max(density(faithful$waiting)$y)])+
  geom_vline(xintercept = density(faithful$waiting)$x[which(density(faithful$waiting)$y == max(density(faithful$waiting)$y[density(faithful$waiting)$x < 65]))])+
  theme_minimal()+ylab("")+xlab("")


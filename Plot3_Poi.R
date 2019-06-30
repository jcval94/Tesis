library(ggplot2)
ggplot(transform(data.frame(x=c(0:10)), y=dpois(x, 1)), aes(x, y)) + 
    geom_bar(stat="identity")

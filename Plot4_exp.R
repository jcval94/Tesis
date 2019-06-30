library(ggplot2)
df <- data.frame(x=seq(0,10,by=0.1))
ggplot(df) + stat_function(aes(x),fun=dexp)

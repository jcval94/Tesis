
library(ggplot2)

pcos<-ggplot(data.frame( x = c(-2.5,2.5)), aes( x = x)) + 
  stat_function(fun = function(x) ifelse(x!=0, cos(pi*x), 0), 
                geom = "line", n=5000, col='red')+
  stat_function(fun = function(x) ifelse(x< -1 | x>1, cos(pi*x), 1), 
                geom = "line", n=5000, col='blue')+
  stat_function(fun = function(x) ifelse(x< -1 | x>1, cos(pi*x), -1), 
                geom = "line", n=5000, col='blue')
pcos



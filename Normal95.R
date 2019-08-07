library(ggplot2)
ggplot(NULL, aes(c(-3,3))) +
  geom_area(stat = "function", fun = dnorm, fill = "#b82200", xlim = c(-3, 1.65)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey80", xlim = c(1.65, 3)) +
  ggtitle("Fn. de Densidad Normal Estandar (95%)") + labs(x = "z", y = "") + 
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = 1.65)

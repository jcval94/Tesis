library(dplyr)

norm_10 <- data.frame(mean = 0:10) %>%
    mutate(likelihood = dnorm(x = 15, mean)) 

ggplot(norm_10, aes(x=mean, y=likelihood)) +
    geom_point()

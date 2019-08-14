set.seed(31109)
muestra <- c(rnorm(n = 5000),rgamma(n = 5000,10,2))
datasim <- data.frame(muestra)
ggplot(datasim, aes(x = muestra))+
  geom_density(color="black",linetype="dotted",adjust = 1/50)+
  geom_density(color="black",linetype="dotted",adjust = 1/40)+
  geom_density(color="black",linetype="dotted",adjust = 1/30)+
  geom_density(color="black",linetype="dotted",adjust = 1/20)+
  geom_density(color="black",linetype="dotted",adjust = 1)+
  geom_density(color="black",linetype="dotted",adjust = 2)+
  geom_density(color="black",adjust = 3)+ylab("")+ggtitle("Ancho de banda")

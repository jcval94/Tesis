Emp<-rnorm(1000)
Real<-rnorm(1000, 2, 2)

group <- c(rep("Emp", length(Emp)), rep("Real", length(Real)))
dat <- data.frame(KSD = c(Emp,Real), group = group)
cdf1 <- ecdf(Emp) 
cdf2 <- ecdf(Real) 

minMax <- seq(min(Emp, Real), max(Emp, Real), length.out=length(Emp)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 


ggplot(dat, aes(x = KSD, group = group, colour = group, linetype=group))+
  stat_ecdf(size=1) +
  xlab("mm") +
  ylab("Función de distribución acumulada") +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=1) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=1) +
  ggtitle("K-S Test: Dist. Empírica / Dist. Real")

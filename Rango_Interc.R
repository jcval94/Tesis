
set.seed (31109);rnorm(30 ,0 ,1)->A

Q1<-quantile(A)
RI<-Q1[4]-Q1[2]
R<-c(Q1[2]-1.5*RI,Q1[4]+RI)
Normal<-data.frame(Normal=A,B="Distribución normal")
p10 <- ggplot(Normal, aes(B,Normal)) +
  geom_boxplot()+xlab("")

p11 <- ggplot(Normal, aes(Normal))+geom_density(fill="red",alpha=.4)+ylab("")

CP<-cowplot::plot_grid(p10,p11)
CP

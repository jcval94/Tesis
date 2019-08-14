a<-rnorm(200,0,1) 
b<-rnorm(200,5,2) 
c<-rnorm(200,8,3) 
d<-c(a,b,c) 
df<-data.frame(d,id=rep(c(1,2,3),each=200)) 

require(ggplot2)

ggplot(df) +
  stat_density(aes(x = d,  linetype = as.factor(id)), position = "stack", geom = "line", show.legend = F) +
  stat_density(aes(x = d,  linetype = as.factor(id)), position = "identity", geom = "line", color = "grey", show.legend = F)+
  ylab("")+xlab("Distribución compuesta por tres Kernels Normales")+ theme_minimal()


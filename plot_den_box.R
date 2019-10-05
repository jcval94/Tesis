library(ggplot2)
plot.density<-function(X1,cat,title,xlb){
  Clase<-rbind(data.frame(Fraude=as.factor(cat),DT=X1))
  p <- ggplot(Clase,aes(x=Clase$DT,fill=Clase$Fraude)) + geom_density(alpha=0.4) +ggtitle(title)+xlab(xlb)
  p
}
plot.box<-function(X1,cat,title,xlb){
  Clase<-rbind(data.frame(Fraude=as.factor(cat),DT=X1))
  p <- ggplot(Clase,aes(x=Fraude,y=DT)) + geom_boxplot(alpha=0.4) +ggtitle(title)+xlab(xlb)
  p
}
pt<-plot.density(X1 = tiempo,cat = as.factor(clas),title="Densidad Tiempo",xlb = "Tiempo")
# plot.density(X1 = exp(tiempo/10000),cat = as.factor(clas),title="Densidad Tiempo",xlb = "Tiempo")

pm<-plot.density(X1 = monto,cat = as.factor(clas),title="Densidad Monto",xlb = "Monto")
m1<-plot.density(l_monto,cat = as.factor(clas),title="Densidad log(Monto)",xlb = "log(Monto)")
outl_monto<-monto < quantile(monto,.975)
m2<-plot.density(monto[outl_monto],cat = as.factor(clas[outl_monto]),title="Densidad Monto percentil 99%",xlb = "Monto")

library(cowplot)

cowplot::plot_grid(pt,pm,nrow = 2)
cowplot::plot_grid(m1,m2,nrow = 2)

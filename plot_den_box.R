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

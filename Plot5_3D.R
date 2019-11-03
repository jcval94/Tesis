Res_P3<-data.frame()
n<-0
clus<-2:5
Prueba_3_<-list()
for(i in 1:length(p_val)){
  for(k in 1:length(clus)){
    n<-n+1
    Nombres=c("exp","pois","beta","gamma","lnorm","weibull","nbinom","hyper","binom","unif","t")
    set.seed(31109)
    PP<-Prueba_3_[[n]]<-try(FDistUlt(X = tiempo[clas==1],plot = T,crt=2,p.val_min = p_val[i],n.obs=length(tiempo[clas==1]),ref = clus[k]))
    if(assertthat::is.error(Prueba_3_[[n]][[3]])){next()}
    rm("Nombres")
    k.s<-0
    for(g in 1:10){
      Muestras<-purrr::map(PP[[1]],~.x())
      limits<-purrr::map(Muestras,~c(min(.x),max(.x)))
      Muestra_completa<-do.call(c,Muestras)
      k.s<-c(k.s,ks.test(Muestra_completa,tiempo[clas==1])$p.value)
    }
    Res_P3<-rbind(Res_P3,data.frame(i=p_val[i],
                                    k=clus[k],
                                    n_vars=nrow(PP[[3]]),
                                    mean=mean(k.s)))
    
  }
}

Cs<-Res_P3

names(Cs)[4]<-"P.Valor.Final"
names(Cs)[3]<-"n_vars"
names(Cs)[2]<-"Clusters"
names(Cs)[1]<-"P.Valor.Inicial"

plot_ly(Cs,x=~Clusters, y=~P.Valor.Inicial, z=~P.Valor.Final, type="scatter3d", mode="markers", color=~n_vars)

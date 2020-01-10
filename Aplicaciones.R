#librerías
library(FitUltD)
library(modes)
library(tidyverse)
library(plotly)
library(mclust)
library(distr)

#Tratamiento de datos
libraries()
ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)


#Leer
data<-read.csv(paste0(getwd(),"/creditcard.csv"))

#Separar fraude/no fraude
cond_fraude<-data[["Class"]]==1
data_Fraude<-data[cond_fraude,]
data_No_Fraude<-data[!cond_fraude,]

 #Breve resumen
summary(data[,c("Time","Amount")])

# Time            Amount        
# Min.   :     0   Min.   :    0.00  
# 1st Qu.: 54202   1st Qu.:    5.60  
# Median : 84692   Median :   22.00  
# Mean   : 94814   Mean   :   88.35  
# 3rd Qu.:139321   3rd Qu.:   77.17  
# Max.   :172792   Max.   :25691.16  

# Como podemos observar, la variable tiempo muestra una diferencia entre la mediana y la moda y dado el volumen de datos, podemos descartar la distribución normal
# Por otro lado, todos los cuantiles presentan diferencias similares, por lo que al hacer la prueba de outliers con rango intercuantílico seguramente veremos pocos

# Respecto a la variable monto, hay una clara diferencia entre el tercer quantil y el máximo, y entonces, podemos esperar la presencia de outliers en nuestra distribución, de igual forma, vemos una notable diferencia entre mediana y media, por lo que esperamos que la distribución esté cargada hacia la izquierda, pues la media es incluso mayor al tercer quantil

# También vemos que ambas variables tienen su mínimo en el valor 0, por lo que no será necesario realizar transformaciones para ajustar distribuciones que se encuentren definidas en R+, como la exponencial por ejemplo.

#Variables de estudio
monto<-data$Amount
tiempo<-data$Time

monto_1<-data[data$Class==1,]$Amount
monto_0<-data[data$Class==0,]$Amount
tiempo_1<-data[data$Class==1,]$Time
tiempo_0<-data[data$Class==0,]$Time
clas<-data$Class
#Histograma y distribución mediante el kernel
par(mfrow=c(1,2))
hist(tiempo,main="Tiempo")
hist(monto,main="Monto")
library(ggplot2)
plot.density<-function(X1,cat,title,xlb){
  Clase<-rbind(data.frame(Fraude=as.factor(cat),DT=X1))
  p <- ggplot(Clase,aes(x=Clase$DT,fill=Clase$Fraude)) + geom_density(alpha=0.4) +ggtitle(title)+xlab(xlb)
  p
}
plot.bar<-function(X1,cat,title,xlb){
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


ks.test(monto[clas==1],monto[clas==0])

ks.test(tiempo[clas==1],tiempo[clas==0])


#aplicaremos la función logaitmo a la variable monto para tener una mejor visualización de los datos
#No obstante, agregaremos un épsilon para no tener -Inf en la nueva variable
epsilon<-sort(unique(monto))[2]/10
l_monto<-log(monto+epsilon)
hist(tiempo,main="Tiempo")
hist(l_monto,main="Log(Monto)")

#Con esto tenemos los siguientes resultados

# La variable tiempo muestra claramente una distribución bimodal o incluso multimodal

# El logaritmo de la variable monto parecería tener una distribución normal asociada, por lo que podríamos atribuirle una distribución Log-Normal, no obstante, no descartamos que pueda ser Log-Weibull o Log-Gamma


#BoxPlot ()
plot.bar(X1 = tiempo,cat = as.factor(clas),title="Tiempo",xlb = "Fraude")
plot.bar(X1 = l_monto,cat = as.factor(clas),title="log(Monto)",xlb = "Fraude")

plot.bar(X1 = monto[outl_monto],cat = as.factor(clas[outl_monto]),title="Monto percentil 97.5%",xlb = "Fraude")

#Rango intercuantílico
IR<-quantile(tiempo)
IR_diff<-IR[4]-IR[2]
Inter_Q<-as.numeric(c(IR[2]-IR_diff,IR[4]+IR_diff))
Inter_Q

#Kurtosis
modes::kurtosis(tiempo,T)

#Bimodalidad
modes::bimodality_coefficient(tiempo)>5/9

#Ajuste
Ajuste_tiempo<-FDist(tiempo,plot = T)
Ajuste_monto<-FDist(monto,plot = T)

#No hubo ajuste con las distribuciones usuales como era de esperar
#Ajustaremos también la transformación de la variable monto

#Ajuste_l_monto<-FDist(l_monto,plot = T)

A<-data.frame(seed=0,ks=0)[-1,]

for (seed in 1:1460) {
  set.seed(seed)
  muestra_t<-tiempo[sample(1:length(tiempo),floor(length(tiempo)*.05))]
  A<-rbind(A,data.frame(seed=seed,ks=ks.test(tiempo,muestra_t)$statistic))
}

#Ahora revisemos los resultados y apliquemos nuestro procedimiento completo
set.seed(31109)
#muestra_tiempo<-dplyr::sample_frac(data.frame(tiempo),.05)[[1]]
#Best seed
set.seed(A[A$ks==min(A[A$ks!=0,]$ks),1])
#
ind<-sample(1:length(tiempo),floor(length(tiempo)*.05))
clas[ind]
muestra_tiempo<-tiempo[ind]

muestra_tiempo_0<-muestra_tiempo[clas[ind]==0]

#Una pequeña comparación de estadísticos entre la muestra y la realidad

smt<-summary(muestra_tiempo)
st<-summary(tiempo)

par(mfrow=c(1,1))
graphics::plot(density(tiempo),main="Tiempo vs Muestra Tiempo")
lines(density(muestra_tiempo),col="red")
#mod1 <- mclust::Mclust(tiempo)$classification

# set.seed(31109)
# 3-5 minutos aprox
# Ajuste_tiempo_Full<-FDistUlt(X = muestra_tiempo,crt = 1,plot = T,p.val_min = 0.01,n.obs=length(muestra_tiempo))


Ajuste_tiempo_Full[[1]]
Ajuste_tiempo_Full[[2]]

plot(density(tiempo))
lines(density(Ajuste_tiempo_Full[[2]]),col="blue")

#Prueba de fuego:
#Pertenecen a la misma distribución?
#Dado que hay números repetidos para una variable que conceptualmente es contínua
#añadiremos un poco de ruido a la misma 

ks.test(Ajuste_tiempo_Full[[2]],jitter(tiempo,amount = 1))
#########
#Si pasó la prueba (si funciona jaja)
#########


#Qué pasa si disminuímos el p.valor?
set.seed(31109)
Ajuste_tiempo_Full<-FDistUlt(X = muestra_tiempo_0,plot = T,p.val_min = 0.01,n.obs=length(muestra_tiempo))
set.seed(31109)
Ajuste_tiempo_Fraud<-FDistUlt(X = tiempo[clas==1],plot = T,p.val_min = 0.01,n.obs=length(muestra_tiempo))

#Deberíamos tener un modelo con menos variables aleatorias, pero y KS?
length(Ajuste_tiempo_Full[[1]])
length(Ajuste_tiempo_Fraud[[1]])

#Aún pasa la prueba de k.s y reducimos conciderablemente el número de variables aleatorias
ks.test(Ajuste_tiempo_Full[[2]],jitter(tiempo,amount = 1))
ks.test(Ajuste_tiempo_Fraud[[2]],jitter(tiempo[clas==1],amount = 1))

summary(Ajuste_tiempo_Fraud[[2]])

Ajuste_tiempo_Full[[4]]
Ajuste_tiempo_Fraud[[4]]

View(Ajuste_tiempo_Fraud[[3]])


Nombres=c("exp","pois","beta","gamma","lnorm","norm","weibull","nbinom","hyper","binom","unif","t")
set.seed(31109)
Ajuste_tiempo_Fraud_No_Cauchy<-FDistUlt(X = tiempo[clas==1],plot = T,p.val_min = 0.01,n.obs=length(muestra_tiempo))
rm("Nombres")

summary(Ajuste_tiempo_Fraud_No_Cauchy[[2]])

summary(Ajuste_tiempo_Full[[2]])
summary(muestra_tiempo_0)

Ajuste_tiempo_Fraud_No_Cauchy[[4]]

View(Ajuste_tiempo_Fraud_No_Cauchy[[3]])

ks.test(Ajuste_tiempo_Fraud_No_Cauchy[[2]],jitter(tiempo[clas==1],amount = 1))

summary(Ajuste_tiempo_Fraud_No_Cauchy[[2]])


purrr::map(Ajuste_tiempo_Fraud_No_Cauchy[[6]],~.x$Lim_inf)

View(Ajuste_tiempo_Fraud_No_Cauchy[[6]][[1]])
Ajuste_tiempo_Fraud_No_Cauchy[[6]][[7]]

#A partir de qué p.valor mínimo aceptable se sigue cumpliendo la prueba de k.s y se seguirán reduciendo el número de v.a's?
#Esta idea se asemeja al concepto de bosque aleatorio, en donde empleamos una gran cantidad de predictores para llegar a la predicción final

P.value<-M_candidato<-list()
p_val<-c(.05,.025,0.015,0.005,0.0025,0.001,0.0005,0.00025,0.0001,0.00005,0.000025,0.00001)
n<-0
Tiempo<-system.time({
for(i in 1:length(p_val)){
  set.seed(31109)
  M_candidato[[i]]<-FDistUlt(X = jitter(muestra_tiempo_0,amount = 1),plot = T,crt=1,p.val_min = p_val[i],n.obs=length(muestra_tiempo_0)*20)
  for(k in 1:10){
    n<-n+1
    P.value[[n]]<-ks.test(sample(M_candidato[[i]][[2]],length(muestra_tiempo_0)),jitter(tiempo,amount = 1))
    print(P.value[[n]]$p.value)
  }
}
})

###############################

P.value_F<-M_candidato_F<-list()
n<-0
Nombres=c("exp","pois","beta","gamma","lnorm","norm","weibull","nbinom","hyper","binom","unif","t")
Tiempo_F<-system.time({
  for(i in 1:length(p_val)){
    set.seed(31109)
    M_candidato_F[[i]]<- FDistUlt(X = tiempo[clas==1],plot = T,crt=1,p.val_min = p_val[i],n.obs=length(tiempo[clas==1]))
    for(k in 1:10){
      n<-n+1
      P.value_F[[n]]<-ks.test(M_candidato_F[[i]][[2]],jitter(tiempo[clas==1],amount = 1))
      print(P.value_F[[n]]$p.value)
    } 
  }
})
rm("Nombres")


purrr::map(M_candidato_F,~dim(.x[[3]]))

Fit4<-M_candidato_F[[4]]

summary(Fit4[[2]])

Fit4[[4]]

Fit4[[3]]

#sustitución normal x weibull
#summary(rnorm(10000,24794.587, 9796.326))
Fit4[[6]][[1]]
summary(rweibull(1000,2.778059,2.980933e+04))
#

qqq<-rweibull
formals(qqq)[1]<-length(Fit4[[1]][[1]]())
formals(qqq)[2]<-2.778059
formals(qqq)[3]<-2.980933e+04

Fit4[[1]][[1]]<-qqq

semilla<-31109:31129
k.s<-c()
for (i in semilla) {
  set.seed(i)
  Muestras<-purrr::map(Fit4[[1]],~.x())
  limits<-purrr::map(Muestras,~c(min(.x),max(.x)))
  Muestra_completa<-do.call(c,Muestras)
  
  k.s<-c(k.s,ks.test(Muestra_completa,tiempo[clas==1])$p.value)
}
summary(Muestra_completa)
summary(tiempo[clas==1])

 #Después de ver el análisis, vemos que el número de separaciones cambia muy drásticamente, por lo que fijaremos el número de k centroides en dos, debido al número de modas en la distribución
set.seed(31109)
#ejecutar 2 veces
#revisar el error en la primera corrida
Nombres=c("exp","pois","beta","gamma","lnorm","weibull","nbinom","hyper","binom","unif","t")
Prueba_2<-FDistUlt(X = tiempo[clas==1],plot = T,crt=1,p.val_min = .01,n.obs=length(tiempo[clas==1]),ref = 2)
rm("Nombres")


qqplot(Prueba_2[[2]],tiempo[clas==1])
summary(Prueba_2[[2]])
ks.test(jitter(tiempo[clas==1],amount=1),Prueba_2[[2]])
Prueba_2[[4]]
write.csv(Prueba_2[[3]],"pruebas2.csv")

#Ahora vemos una mejora muy significativa, pasando de 10 distribuciones distintas a solamente tres

#Aproximadaente 611.1 segundos por cada 28K obs
Tiempo

print(paste0("Horas d e ejecución: ",Tiempo[3]/3600))
#save.image("~/Lib/Tesis/.RData")

#Uno de los resultdos nos arroja valores muy negativos, lo que significa que el p.valor no debe ser tan cercano a cero y que además, debemos aregar la posibilidad de ajustar distribuciones con colas pesadas

#Veamos cada uno de los resultados

M_candidato[[12]][[4]]

DF<-data.frame(p_val_min=rep(p_val,each=10),
NROW=rep(purrr::map_int(M_candidato,~nrow(.x[[3]])),each=10),
P.VAL=purrr::map_dbl(P.value,~.x[["p.value"]]))
Mean<-reshape2::dcast(data = DF,formula = p_val_min~.,mean,value.var = "P.VAL")
Median<-reshape2::dcast(data = DF,formula = p_val_min~.,median,value.var = "P.VAL")

Mean[[1]]<-as.factor(Mean[[1]])
Median[[1]]<-as.factor(Median[[1]])
plot(Mean)
plot(Median)

library(plotly)
plot_ly(x=DF[[1]], y=DF[[2]], z=DF[[3]], type="scatter3d", mode="markers", color=DF[[3]])

#Observamos dos cosas distintas:
#La cantidad de componentes que conforman la función de distribución final disminuye si el p_valor_min lo hace
#Es decir, obtenemos un modelo más simple con un menor número de predictores.

#El p.valor final obtenido empieza a variar una vez que el p.valor inicial es demasiado bajo (5.0e-05)

#No obstante, parece que para algunos valores menores (1.0e-05) no se rechaza la hipótesis nula

#Más aún, para estos valores pequeños el p.valor final no necesariamente se ve afectado.

#Vemos que, por ejemplo, el primer modelo 5.0e-02  112 0.266475546 ofrece resultados más complejos y con un mayor tiempo de cómputo y que sin embargo, no se refleja en el p.valor final
#Este, podríamos llamarlo un modelo sobreajustado

#Por otro lado, el doceavo modelo ofrece la siguiente respuesta: 1.0e-05   34 0.306709586, este, ocupa una menor cantidad de componentes y pasa la hipótesis nula.

#Veamos entonces el contenido de este modelo

M_candidato[[12]][[4]]

Estudio<-M_candidato[[12]][[3]]

#Primero realicemos vallidación cruzada para saber que el resultado no fue un evento atípico

Funciones_generadoras<-M_candidato[[12]][[1]]
semilla<-31109:31129
k.s<-c()
for (i in semilla) {
  set.seed(i)
  Muestras<-purrr::map(Funciones_generadoras,~.x())
  Muestra_completa<-do.call(c,Muestras)
  
  k.s<-c(k.s,ks.test(Muestra_completa,jitter(tiempo,1))$p.value)
}
#Distribución de los p.valores
plot(density(k.s))
#El mínimo valor de los p.valores fue 0.17023 y la media de estos: 0.32465
min(k.s)
mean(k.s)
#Entonces, podemos decir con mayor certidumbre que no rechazamos la hipótesis nula con este modelo

#Comprobemos que la muestra concuerde con las proporciones
len<-purrr::map_int(Muestras,length)
#Las proporciones son perfectas
len/sum(len)-Estudio$Dist_Prop


#El gráfico quantil-quantil para ver si hay anomalias
qqplot(Muestra_completa,tiempo)
#El modelo encaja en cada uno de sus percentiles respecto a la muestra original
`%c%` <- function(e1, e2) as.numeric(e1)>=as.numeric(e2[1]) & as.numeric(e1)<=as.numeric(e2[2])

muestra_tiempo[muestra_tiempo %c% M_candidato[[12]][[3]][1,c(13,14)]]


####Análisis más profundo:
#Por qué el onceavo modelo fue rechazado?
#Realicemos nuevamente validación cruzada:

modelo<-11

Funciones_generadoras<-M_candidato[[modelo]][[1]]
#Funciones_generadoras<-A_X[[1]]
semilla<-31109:31129
k.s<-c()
for (i in semilla) {
  set.seed(i)
  Muestras<-purrr::map(Funciones_generadoras,~.x())
  limits<-purrr::map(Muestras,~c(min(.x),max(.x)))
  Muestra_completa<-do.call(c,Muestras)
  
  k.s<-c(k.s,ks.test(Muestra_completa,tiempo)$p.value)
}
summary(Muestra_completa)


# S_MC<-summary(Muestra_completa)
# S_T<-summary(tiempo)
# (S_MC-S_T)/S_MC
################

M_candidato[[7]][[4]]

summary(M_candidato[[7]][[2]])


################

# all_lim<-do.call(c,limits)
# Z<-c(runif(A_X[[3]]$Dist_Prop[1]*length(X),limits[[1]][1],limits[[1]][2]),
# runif(A_X[[3]]$Dist_Prop[2]*length(X),limits[[2]][1],limits[[2]][2]),
# runif(A_X[[3]]$Dist_Prop[3]*length(X),limits[[3]][1],limits[[3]][2]))
# hist(Z)
#Distribución de los p.valores
plot(density(k.s))
#Resultó ser un valor atípico, pues recordemos que la prueba K.S es más sensible a outliers

#Volviendo a nuestra tabla de resultados
M_candidato[[modelo]][[3]]
#Qué proporción de la distribución no pasó la hpiótesis nula?
DF_12<-M_candidato[[modelo]][[3]]
DF_12<-A_X[[3]]
sum(DF_12[DF_12$AD_p.v<.05 & DF_12$KS_p.v<.05,"Dist_Prop"])
sum(DF_12[DF_12$AD_p.v<.05 | DF_12$KS_p.v<.05,"Dist_Prop"])

sum(DF_12[DF_12$AD_p.v<.01 & DF_12$KS_p.v<.01,"Dist_Prop"])
sum(DF_12[DF_12$AD_p.v<.01 | DF_12$KS_p.v<.01,"Dist_Prop"])

plot(density(tiempo))
lines(density(Muestra_completa),col="red")
#lines(density(Muestra_completa2),col="blue")
#Por qué un conjunto de distribuciones que por separado no pasan la hip.nula en su conjunto forman un modelo que si lo logra?

##########################################################
##########################################################
##########################################################
#Aquí es donde todo se derrumbó dentro de mi, dentro de mi
##########################################################
##########################################################
##########################################################


#Distribución generada por el modelo 12
#12 es el modelo, 4 es la parte de las gráficas, 1 es la primera gráfica
M_candidato[[12]][[4]][[1]]


#Cuál es la distribución del tiempo?
#Es el momento de asignar formalmente una distribución

#Veremos primero si los parámetros de las distribuciones no generarán conflicto al cambiar de paquetería
#Entonces vamos a 
#######################
#stats vs distr

get_dist_args<-function(Dist){
  
  firstup <- function(x) {
    FL<-toupper(substr(x, 1, 1))
    #si es una gamma agrego la d para que pueda ser leida por "distr"
    if(FL=="G"){
      x<-gsub("gamma","gammad",x)
    }
    if(FL=="T"){
      x<-gsub("t","td",x)
    }
    substr(x, 1, 1) <- FL
    
    x
  }
  text_eval<-function(a){
    if(class(a)!="character"){return()}
    eval(parse(text = a))
  }
  purrr::map(c(paste0("distr::",firstup(Dist)),
               paste0("stats::r",Dist)),~formalArgs(text_eval(.x)))
}

Args<-purrr::map(c("exp","pois","beta","gamma","lnorm","norm","weibull",
                      "nbinom","hyper","cauchy","binom","unif","t"),~get_dist_args(.x))

get_dist("gamma(364.537, 0.009)")
#warnings: sólo la dist. gamma
#

#######################

get_dist<-function(Dist){
  text_eval<-function(a){
    if(class(a)!="character"){return()}
    eval(parse(text = a))
  }
  firstup <- function(x) {
    FL<-toupper(substr(x, 1, 1))
    #si es una gamma agrego la d para que pueda ser leida por "distr"
    if(FL=="G"){
      x<-gsub("gamma","gammad",x)
      param_split<-strsplit(x,",")[[1]]
      param2<-param_split[2]
      x<-paste0(param_split[1],",",1/text_eval(substr(param2,1,nchar(param2)-1)),")")
    }
    if(FL=="T"){
      x<-gsub("t","td",x)
    }
    substr(x, 1, 1) <- FL
    x
  }
  
  text_eval(paste0("distr::",firstup(Dist)))
}
library(distr)
#Transformo los nobres de las distribuciones en funciones en R
Dists<-purrr::map(as.character(DF_12[,1]),get_dist)

#Utilizo rfunc
Tott<-list()
for(i in 1:length(Dists)){
  Tott[[i]]<-r(Dists[[i]])(DF_12[,"Obs"][[i]])
}
Distss<-do.call("c",Tott)

Distss<-purrr::map2(Dists,DF_12[,"Obs"],~r(.x)(.y))
limits<-purrr::map(Distss,~c(min(.x),max(.x)))

max(purrr::map_dbl(limits,max))
#Hasta aquí todo parec bien (salvo las proporciones de las muestras)
plot(density(tiempo))
lines(density(Distss),col="red")

####################Prueba modelo 2
#########################
modelo<-10

DF_12<-M_candidato[[modelo]][[3]]
table(DF_12$Dist)
A<-M_candidato[[modelo]][[3]][,1]
B<-M_candidato[[modelo]][[3]][,"Dist_Prop"]*length(M_candidato[[modelo]][[2]])

Dists<-purrr::map(as.character(A),get_dist)
Distss<-purrr::map2(Dists,B,~r(.x)(.y))

plot(density(tiempo))
m.a.<-do.call(c,Distss)
lines(density(m.a.),col="blue")
summary(m.a.)
summary(tiempo)

qtl_diff<-quantile(m.a.,1:100/100)-quantile(tiempo,1:100/100)
quant<-names(qtl_diff[abs(qtl_diff)==max(abs(qtl_diff))])
quant
quant_n<-.18
cond_df<-DF_12[,13] < quantile(m.a.,quant_n) & DF_12[,14] > quantile(m.a.,quant_n)
DF_12[cond_df,]


ks.test(jitter(tiempo,amount = 1),m.a.)
qqplot(jitter(tiempo,amount = 1),m.a.)
plot(ecdf(tiempo));lines(ecdf(m.a.),col="red")
############################
#Me interesa saber también si hay huecos entre distribuciones?
Dist<-UnivarMixingDistribution(Dlist = Dists,mixCoeff = DF_12$Dist_Prop)
#por separado
plot(Dist)

par(mfrow=c(1,2))
hist(tiempo,breaks = 500)
hist(m.a.,breaks = 500)
par(mfrow=c(1,1))

muestra<-r(Dist)(25000)
summary(muestra)
summary(tiempo)

ks.test(muestra,tiempo)

ks.test(m.a.[m.a.<quantile(m.a.,.995) & m.a. > quantile(m.a.,.005)],tiempo)

plot(sum_D)
#Podríamos obtener un modelo mejor?

#Los resultdos indican que si, en caso de que reduzcamos aún más el p.valor mínimo
pdistodaro<-p(Dist)
library(ADGofTest)
ADGofTest::ad.test(muestra,pdistodaro)
ADGofTest::ad.test(muestra_tiempo,pdistodaro)

plot(density(muestra_tiempo))
lines(density(muestra),col="blue")
summary(muestra)
summary(muestra_tiempo)

quantile_critic<-function(x,y){
  qtl_diff<<-quantile(x,1:100/100)-quantile(y,1:100/100)
  cond<-abs(qtl_diff)==max(abs(qtl_diff))
  quant<-(1:100)[cond]/100
  diff<-qtl_diff[cond]
  return(c(quant,quantile(x,quant),quantile(y,quant),diff))
}

quantile_critic(muestra,muestra_tiempo)
qtl_diff
KS<-ks.test(jitter(muestra,amount = 1),
            jitter(muestra_tiempo,amount = 1))

KS

ADGofTest::ad.test(tiempo,pdistodaro)

#Por qué la muestra si y el total no?
#Dos opciones posibles:
#La muestra no es representativa o la prueba

#Es sufuciente K.S. para saber si la muestra es rep.?
ks.test(muestra_tiempo,tiempo)
ks.test(muestra_tiempo,muestra)
tiempo
ks.test(M_candidato[[7]][[2]],tiempo)

ks.test(muestra,tiempo)
#Que es lo que hace distinto el test ks respecto a AD?

#El estadístico de KS (La diferencia máxima de las F(x)) 
#Resulta ser suficiente como para pasar la prueba con un p.valor de .6

#Al buscar la máxima de estas diferencias, encontramos "huecos" en la distribución
#Dichos huecos pueden deberse a la existencia de un corte "fijo"

#Entonces, definiremos el alcance de la distribución

#Recordemos que hay distribuciones cuyos límites están buen definidos (unif y beta por ejemplo)
#Y recordemos que también las hay de colas pesadas, lo que implica la aparición de outliers que no hacen sentido (cmo tiempos negativos)
#Entonces, debemos resringir el tipo de distribución que estamos empleando al momento del cálculo



#Quien es responsable de los outliers?
summary(M_candidato[[7]][[2]])
M_candidato[[7]][[3]]




#Cuáles son las ventajas sobre la ditribución empírica

#La distribución empírcia sólo es capaz de generar valores antes observados y no es posible generar nuevos outliers
#Un modelo paramérico ofrece una mejor explicación de como está conformada una distribución


#Era de esperar que para un valor de 0 ajustara cualquie distribución

#save.image("~/Lib/Tesis/g.RData")
#Entonces observamos los siguientes resultados:

M_candidato

c(p_val,as.vector(P.value))

#Después de anaizar los resultados es necesario observar la importancia de hacer el corte en el Punto adecuado, pues dicho factor diferencia el total de lOs subconjuntos obtenidos que se ajustan a la distribución, la velocidad de ejecución y la exactitud del modelo
#Si vemos los resultados 

Ajuste_tiempo_Full<-FDistUlt(tiempo,plot = T,subplot = T)
Ajuste_monto_Full<-FDistUlt(monto,plot = T,subplot = T)
Ajuste_l_monto_Full<-FDistUlt(l_monto,plot = T,subplot = T)

save.image("~/Lib/Tesis/tesis.RData")
#system("shutdown -s")




##
U<-unique(df[["CL"]])
par(mfrow=c(3,3))
purrr::map(U,~plot(density(df[df[["CL"]]==.x,2])))
par(mfrow=c(1,1))



#Los cortes están bien definidos, pero qué pasa si no doy un intervalo
#definido sino una zona "aleatoria"

X<-sort(X)
X[c(F,diff(mod1)!=0)]




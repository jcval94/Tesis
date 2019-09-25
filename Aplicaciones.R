#librerías
library(FitUltD)
library(modes)
library(tidyverse)
#Tratamiento de datos

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
# Por otro lado, todos los quantiles presentan diferencias similares, por lo que al hacer la prueba de outliers con rango intercuantílico seguramente veremos pocos

# Respecto a la variable monto, hay una clara diferencia entre el tercer quantil y el máximo, y entonces, podemos esperar la presencia de outliers en nuestra distribución, de igual forma, vemos una notable diferencia entre mediana y moda, por lo que esperamos que la distribución esté cargada hacia la izquierda, pues la media es incluso mayor al tercer quantil

# También vemos que ambas muestras tienen su mínimo en el valor 0, por lo que no será necesario realizar transformaciones para ajustar distribuciones que se encuentren definidas en R+, como la exponencial por ejemplo.

#Variables de estudio
monto<-data$Amount
tiempo<-data$Time

#Histograma y distribución mediante el kernel
par(mfrow=c(1,2))
hist(tiempo,main="Tiempo")
hist(monto,main="Monto")

#aplicaremos la función logaitmo a la variable monto para tener una mejor visualización de los datos
#No obstante, agregaremos un épsilon para no tener -Inf en la nueva variable
epsilon<-sort(unique(monto))[2]/1000
l_monto<-log(monto+epsilon)
hist(tiempo,main="Tiempo")
hist(l_monto,main="Log(Monto)")

#Con esto tenemos los siguientes resultados

# La variable tiempo muestra claramente una distribución bimodal o incluso multimodal

# El logaritmo de la variable monto parecería tener una distribución normal asociada, por lo que podríamos atribuirle una distribución Log-Normal, no obstante, no descartamos que pueda ser Log-Weibull o Log-Gamma


#BoxPlot ()
boxplot(l_monto)
boxplot(tiempo)

#Rango intercuantílico



#Kurtosis




#Ajuste
Ajuste_tiempo<-FDist(tiempo,plot = T)
Ajuste_monto<-FDist(monto,plot = T)

#Ajustaremos también la transformación de la variable monto

Ajuste_l_monto<-FDist(l_monto,plot = T)


#Ahora revisemos los resultados y apliquemos nuestro procedimiento completo

set.seed(31109)
muestra_tiempo<-dplyr::sample_frac(data.frame(tiempo),.05)[[1]]
#Una pequeña comparación de estadísticos entre la muestra y la realidad

smt<-summary(muestra_tiempo)
st<-summary(tiempo)

plot(density(tiempo))
lines(density(muestra_tiempo),col="red")


mod1 <- mclust::Mclust(tiempo)$classification

set.seed(31109)
Ajuste_tiempo_Full<-FDistUlt(X = muestra_tiempo,plot = T,p.val_min = 0.01,n.obs=length(muestra_tiempo))


Ajuste_tiempo_Full[[1]]

Ajuste_tiempo_Full[[2]]

plot(density(tiempo))
lines(density(Ajuste_tiempo_Full[[2]]),col="blue")

#Prueba de fuego:
#Pertenecen a la misma distribución?
ks.test(Ajuste_tiempo_Full[[2]],tiempo)



set.seed(31109)
Ajuste_tiempo_Full<-FDistUlt(X = muestra_tiempo,plot = T,p.val_min = 0.001,n.obs=length(muestra_tiempo))
#Qué pasa si disminuímos el p.valor

#Deberíaos tener un odelo con menos variables aleatorias, pero y KS?
length(Ajuste_tiempo_Full[[1]])

#Aún pasa la prueba de k.s y reducimos conciderablemente el número de variables aleatorias
ks.test(Ajuste_tiempo_Full[[2]],tiempo)

plot(density(tiempo))
lines(density(Ajuste_tiempo_Full[[2]]),col="blue")


#A partir de qué p.valor mínimo aceptable se sigue cumpliendo la prueba de k.s y se seguirán reduciendo el número de v.a's?
#Esta idea se asemeja al concepto de bosque aleatorio, en donde empleamos una gran cantidad de predictores para llegar a la predicción final

P.value<-M_candidato<-list()
p_val<-c(.05,.025,0.01,0.005,0.0025,0.001,0.0005,0.00025,0.0001,0.00005,0.000025,0.00001,0)
n<-0
Tiempo<-system.time({
for(i in 1:length(p_val)){
  set.seed(31109)
  M_candidato[[i]]<-FDistUlt(X = muestra_tiempo,plot = T,p.val_min = p_val[i],n.obs=length(muestra_tiempo)*10)
  for(k in i:10){
    n<-n+1
    P.value[[n]]<-ks.test(sample(M_candidato[[i]][[2]],length(muestra_tiempo)),tiempo)
  }
}
})
#Aproximadaente 611.1 segundos por cada 28K obs
Tiempo

print(paste0("Horas de ejecución: ",Tiempo[3]/3600))

#Uno de los resultdos nos arroja valores muy negativos, lo que significa que el p.valor no debe ser tan cercano a cero y que además, debemos aregar la posibilidad de ajustar distribuciones con colas pesadas

#Veamos cada uno de los resultados


M_candidato[[12]][[4]]

DF<-data.frame(p_val_min=p_val,
NROW=purrr::map_int(M_candidato,~nrow(.x[[3]])),
P.VAL=purrr::map_dbl(P.value,~.x[["p.value"]]))


plot(DF)

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
  
  k.s<-c(k.s,ks.test(Muestra_completa,tiempo)$p.value)
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


####Análisis más profundo:
#Por qué el onceavo modelo fue rechazado?
#Realicemos nuevamente validación cruzada:

Funciones_generadoras<-M_candidato[[11]][[1]]
semilla<-31109:31129
k.s<-c()
for (i in semilla) {
  set.seed(i)
  Muestras<-purrr::map(Funciones_generadoras,~.x())
  Muestra_completa<-do.call(c,Muestras)
  
  k.s<-c(k.s,ks.test(Muestra_completa,tiempo)$p.value)
}
#Distribución de los p.valores
plot(density(k.s))
#Resultó ser un valor atípico, pues recordemos que la prueba K.S es más sensible a outliers

#Volviendo a nuestra tabla de resultados
M_candidato[[12]][[3]]
#Qué proporción de la distribución no pasó la hpiótesis nula?
DF_12<-M_candidato[[12]][[3]]
sum(DF_12[DF_12$AD_p.v<.05 & DF_12$KS_p.v<.05,"Dist_Prop"])
sum(DF_12[DF_12$AD_p.v<.05 | DF_12$KS_p.v<.05,"Dist_Prop"])


sum(DF_12[DF_12$AD_p.v<.01 & DF_12$KS_p.v<.01,"Dist_Prop"])
sum(DF_12[DF_12$AD_p.v<.01 | DF_12$KS_p.v<.01,"Dist_Prop"])

#Por qué un conjunto de distribuciones que por separado no pasan la hip.nula en su conjunto forman un modelo que si lo logra.

#Esta pregunta ha sido antes abordada en el estudio del modelo estadístico Random Forest.
#Conceptualmente




#Cuál es la distribución del tiempo?
#Es el momento de asignar formalmente una distribución
library(distr)
v1<-as.character(DF_12[1,1])

get_dist<-function(Dist){
  firstup <- function(x) {
    FL<-toupper(substr(x, 1, 1))
    #si es una gamma agrego la d para que pueda ser leida por "distr"
    if(FL=="G"){
      x<-gsub("gamma","gammad",x)
    }
    substr(x, 1, 1) <- FL
    
    x
  }
  text_eval<-function(a){
    if(class(a)!="character"){return()}
    eval(parse(text = a))
  }
  text_eval(paste0("distr::",firstup(Dist)))
}

Dists<-purrr::map(as.character(DF_12[,1]),get_dist)

Dist<-UnivarMixingDistribution(Dlist = Dists,mixCoeff = DF_12$Dist_Prop)


#Podríamos obtener un modelo mejor?

#Los resultdos indican que si, en caso de que reduzcamos aún más el p.valor mínimo


#Cuáles son las ventajas sobre la ditribución empírica

#La distribución empírcia sólo es capaz de generar valores antes observados y no es posible generar nuevos outliers
#Un modelo paramérico ofrece una mejor explicación de como está conformada una distribución


#Era de esperar que para un valor de 0 ajustara cualquie distribución

#save.image("~/Lib/Tesis/g.RData")
#Entonces observamos los siguientes resultados:

M_candidato

c(p_val,as.vector(P.value))

#Después de anaizar los resultados es necesario observar la importancia de hacer el corte en el munto adecuado, pues dicho factor diferencia el total de las subconjuntos obtenidos que se ajustan a la distribución, la velocidad de ejecución y la exactitud del modelo
#Si vemos los resultados 

Ajuste_tiempo_Full<-FDistUlt(tiempo,plot = T,subplot = T)
Ajuste_monto_Full<-FDistUlt(monto,plot = T,subplot = T)
Ajuste_l_monto_Full<-FDistUlt(l_monto,plot = T,subplot = T)

save.image("~/Lib/Tesis/tesis.RData")
#system("shutdown -s")

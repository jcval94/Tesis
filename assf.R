#Ejercico del dia de la crisis

cambio<-read.csv("C:/Users/CUBO/Downloads/Datos históricos USD_MXN (1).csv")
crudo<-read.csv("C:/Users/CUBO/Downloads/Datos históricos Futuros petróleo crudo WTI (2).csv")
cambio<-cambio[-261,]


#cambio<-cambio[c(15:100),]
#crudo<-crudo[c(15:100),]

cor(crudo[[2]],cambio[[2]])
cor(crudo[[3]],cambio[[3]])
cor(crudo[[4]],cambio[[4]])
cor(crudo[[5]],cambio[[5]])

cr_max<-crudo[[3]]/max(crudo[[3]])
ca_max<-cambio[[5]]/max(cambio[[5]])

par(mfrow = c(1,2))
plot.ts(cr_max,col=2)
plot.ts(ca_max)

plot(cr_max,ca_max)

cor(cr_max,ca_max)

#Cross validation

cierr<-aper<-mini<-maxi<-c()
for (semanas in (1:16)) {
  cierr<-c(cierr,cor(crudo[[2]][(1:100)+((semanas-1)*10)],cambio[[2]][(1:100)+((semanas-1)*10)]))
  aper<-c(aper,cor(crudo[[3]][(1:100)+((semanas-1)*10)],cambio[[3]][(1:100)+((semanas-1)*10)]))
  maxi<-c(maxi,cor(crudo[[4]][(1:100)+((semanas-1)*10)],cambio[[4]][(1:100)+((semanas-1)*10)]))
  mini<-c(mini,cor(crudo[[5]][(1:100)+((semanas-1)*10)],cambio[[5]][(1:100)+((semanas-1)*10)]))
}
df1<-data.frame(cierr=cierr,aper=aper,maxi=maxi,mini=mini)


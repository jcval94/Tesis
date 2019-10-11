library(distr)

D1<-Gammad(shape = 489.72892,1/3.180806e-03)
D2<-Norm(153888.40799,	6.934835e+03)
D3<-Lnorm(11.94369,4.525698e-02)

xs <- seq(175000,185000,len=400)
plot(xs,d(D1)(xs),type="l",lwd=2,main="Colas Gamma, Norm & Lnorm",xlab="tiempo",ylab="fX(x)")
lines(xs,d(D2)(xs),type="l",lwd=2,col="blue")
lines(xs,d(D3)(xs),type="l",lwd=2,col="red")
legend("topright",legend=c("Gamma","Normal", "LogNormal"), fill=c("black","blue","red"))

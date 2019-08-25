x11();par(mfrow=c(2,3))
set.seed(31109);X<-rnorm(8)
plot(density(X,kernel = "epanechnikov"),main="Epanechnikov")
plot(density(X,kernel = "rectangular"),main="Rectangular")
plot(density(X,kernel = "triangular"),main="Triangular")

plot(density(X,kernel = "biweight"),main="Biweight")
plot(density(X,kernel = "cosine"),main="Cosine")
plot(density(X,kernel = "optcosine"),main="Optcosine")

library(ggplot2)
data(iris)
cl<- kmeans(iris[,3:4],3,nstart = 20)$cluster
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= cl)) + geom_point()

data(iris)
library(ade4)
tp<-triangle.plot(iris[,c(1,2,4)],cpoint=0,show.position=F)
points(tp, pch=c(1,2,8)[iris[,5]])

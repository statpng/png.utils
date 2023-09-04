# data(iris)
# rownames(iris)<-paste(toupper(substr(iris[,5],1,2)),
# rownames(iris), sep="")
# bb<-hclust(dist(iris[,1:4]), method="ave")
# dend<-as.dendrogram(bb)
# 
# local({
# 	colLab <<- function(n) {
#     if(is.leaf(n)) {
#       a <- attributes(n)
#       i <<- i+1
#       attr(n, "nodePar") <-
#         c(a$nodePar, list(lab.col = mycols[i],
#         pch=mysymbols[i],col=mycols[i],
#         lab.cex=0.5, cex=0.5 ))}
#         n }
# mycols <- c("blue","red","green")[as.factor
#            (substr(labels(dend),1,2))]
# mysymbols<-c(15,17,1)[as.factor(substr(labels(dend), 1,2))]
# i <- 0
# })
# b <- dendrapply(dend, colLab)
# plot(b, main="UPGMA on the iris data set")
# 
# 
# library(ade4)
# kk<-hclust2phylog(bb, FALSE)
# radial.phylog(kk,clabel.l=0.5,cleaves=0,circle=1.7)
# points(0,0,pch=21,cex=2,bg="grey")
# 
# 

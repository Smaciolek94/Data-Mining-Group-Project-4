library(cluster)

#creating some sample data
x <- rep(0,1000)
dim(x) <- c(1000,10)
x <- as.data.frame(x)
for (i in 1:10){
  x[,i] <- rnorm(100)
}

#number of clusters here:
n<-6

#kmeans clustering:
k <- kmeans(x,n)

############################GAP STATISTIC
#kmeans
gapstatkmeans <- clusGap(x,FUN = kmeans,K.max = 20,B = 10) # last argument is the total number of clusters
#PAM
gapstatpam <- clusGap(x,FUN = pam,K.max = 20,B = 10)
#hierachical
#a seperate function is needed for hierachical 
hclust1 <- function(x,k){
  list(cluster=cutree(hclust(dist(x),method="ward.D"),k))
}
      #as an aside, dist is the distance matrix of x, cutree cuts the cluster tree at k clusters
        #similar to proc cluster and then proc tree in SAS
gapstathier <- clusGap(x,FUN = hclust1,K.max = 20, B=10)

#plotting
plot(gapstatkmeans)
plot(gapstatpam)
plot(gapstathier)

#silhouette statistic:
#kmeans needs its own function defined first
kmfunc <- kmeans(x,n)
silhouette(kmfunc$cluster, dist(x)) #this one works now too
silhouette(pam(x,n)) #this one works
silhouette(hclust1, dist(x))

#second derivative:
sdkmeans <- sapply(1:20, FUN = kmeans(x,7))
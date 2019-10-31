library(cluster)

#creating some sample data
x <- rep(0,1000)
dim(x) <- c(1000,10)
x <- as.data.frame(x)
for (i in 1:10){
  x[,i] <- rnorm(100)
}

############################GAP STATISTIC
#the clusGap function calculates the gap statistic for each number of clusters specified by K.max =
#no need to loop it, but it will take some time to run

#kmeans
gapstatkmeans <- clusGap(x,FUN = kmeans,K.max = 20,B = 10) # last argument is the total number of clusters

#PAM
gapstatpam <- clusGap(x,FUN = pam,K.max = 20,B = 10)

#hierachical
#for hierarchical, the function needs to be defined first
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

print(min(gapstatpam$Tab[,3])) #the third column is the gap stat
print(min(gapstatkmeans$Tab[,3])) #the third column is the gap stat
print(min(gapstathier$Tab[,3])) #the third column is the gap stat

################silhouette statistic:
#the silhoutte function finds the silhoutte statistic for each point for a given number of clusters and a given 
#clustering method.  I had to define a function that ran it for each clustering method, and then averaged
#all of the output silhoutte values to find the silhoutte statistic for that method for that number of clusters
#it needs to be looped in order to find the minumum

silh <- function(x,n){

  kmfunc <- kmeans(x,n)
  silkm <- silhouette(kmfunc$cluster, dist(x)) 
  
  pamfunc <- pam(x,n)
  silpam <- silhouette(pamfunc$clustering, dist(x)) 
  
  outhier <- hclust1(x,n)
  silhier <- silhouette(as.numeric(outhier$cluster), dist(x)) 
    
 #finding the averages and returning just them
  avesilkm <- mean(silkm[,3])
  avesilpam <- mean(silpam[,3])
  avesilhier <- mean(silhier[,3])
  out <- c(avesilkm,avesilpam,avesilhier)
  return(out)
}
#running the function in a loop for each possible number of clusters

p <- silh(x,6) # it works when run once, but not in a cluster 
#im still getting an error with the number of dimensions when trying to loop, but the function works when run by itself
out <- rep(0,60)
dim(out) <- c(20,3)
for (n in 1:20){
  out[n,1:3] <- silh(x,n)
}

#second derivative:
sdkmeans <- sapply(1:20, FUN = kmeans(x,7))
sdpam <- sapply(1:20, FUN = pam(x,n))
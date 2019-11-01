library(cluster)

#creating some sample data
x <- rep(0,1000)
dim(x) <- c(100,10)
x <- as.data.frame(x)
for (i in 1:10){
  x[,i] <- rnorm(100)
}

############################GAP STATISTIC
#the clusGap function calculates the gap statistic for each number of clusters from 1 to K.max =
#no need to loop it, but it will take some time to run

#kmeans
gapstatkmeans <- clusGap(x,FUN = kmeans,K.max = 20,B = 10) # last argument is the total number of bootstrap samples

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
#this is not necessary for the final product, it's just so you can see a visual of the output
#this will not work if you ran the part below that converts the output to a dataframe
#plot(gapstatkmeans)
#plot(gapstatpam)
#plot(gapstathier)

#this section returns the observation number with the lowest gap stat, which corresponds to the number of 
#clusters with the lowest gap statisitc for each method
gapstatpam$Tab <- as.data.frame(gapstatpam$Tab)
minpamgap <- which(gapstatpam$Tab$gap==min(gapstatpam$Tab$gap))

gapstatkmeans$Tab <- as.data.frame(gapstatkmeans$Tab)
minkmeansgap <- which(gapstatkmeans$Tab$gap==min(gapstatkmeans$Tab$gap))

gapstathier$Tab <- as.data.frame(gapstathier$Tab)
minhiergap <- which(gapstathier$Tab$gap==min(gapstathier$Tab$gap))

gapstatfinal <- data.frame(minpamgap,minkmeansgap,minhiergap)
colnames(gapstatfinal) <- c("PAM minimum", "kmeans minimum", "hierachical minimum")

#the final output is printed here
print(gapstatfinal)

################silhouette statistic:
#the silhoutte function finds the silhoutte statistic for each point for a given number of clusters and a given 
#clustering method.  I had to define a function that ran it for each clustering method, and then averaged
#all of the output silhoutte values to find the silhoutte statistic for that method for that number of clusters
#it needs to be looped in order to find the minumum
#this function for some reason does not like when you start with 1 cluster, so the loop starts with 2 clusters

#defining the functions for each method that will need to be looped

silhkm <- function(x,n){
  kmfunc <- kmeans(x,n)
  silkm <- silhouette(kmfunc$cluster, dist(x)) 
  avesilkm <- mean(silkm[,3])
  return(avesilkm)
}

silhpam <- function(x,n) {
  pamfunc <- pam(x,n)
  silpam <- silhouette(pamfunc$clustering, dist(x)) 
  avesilpam <- mean(silpam[,3])
  return(avesilpam)
}
  
silhhier <- function(x,n) {
  outhier <- hclust1(x,n)
  silhier <- silhouette(as.numeric(outhier$cluster), dist(x)) 
  avesilhier <- mean(silhier[,3])
  return(avesilhier)
}

#looping
outkmsil <- rep(0,19)
outpamsil<- rep(0,19)
outhiersil <- rep(0,19)

for (i in 2:20){
  n <- i
  n <- as.numeric(n)
  outkmsil[i] <- silhkm(x,n)
  outpamsil[i] <- silhpam(x,n)
  outhiersil[i] <- silhhier(x,n)
}

#finding minima and adding 1, since we started at 2 clusters 
minoutkmsil <- which(outkmsil==min(outkmsil))
minoutkmsil <- minoutkmsil + 1

minoutpamsil <- which(outpamsil==min(outpamsil))
minoutpamsil <- minoutpamsil + 1

minouthiersil <- which(outhiersil==min(outhiersil))
minouthiersil <- minouthiersil + 1

silhouettefinal <- data.frame(minoutkmsil,minoutpamsil,minouthiersil)
colnames(silhouettefinal) <- c("kmeans minimum","PAM minimum","hierarchial minimum")

print(silhouettefinal)

#second derivative:

clust_sel = function(x,y,jrange=2:20,dd=2) {
  ## x is an array, ## jrange n of clusters to be checked
  ## y is an hclust object ## dd number of differences
  wss4 = function(x,y,w = rep(1, length(y))) {sum(lm(x~factor(y),weights =
                                                    w)$resid^2*w)}
  ### wss4 calculates within cluster sum of squares
  sm1 = NULL
  for(i in jrange) sm1[i] = wss4(x,cutree(y,i))
  sm1=sm1[jrange]
  k = if(dd==1) sm1[-1] else -diff(sm1)
  plot(jrange[-length(k)+1:0], -diff(k)/k[-length(k)]*100)
  jrange [sort.list(diff(k)/k[-length(k)]*100)[1:4]]
}

clust_sel(x,hclust1(x,dist(x)))
clust_sel(x,pam)

kmfunc <- kmeans(x,)

#if we're using the random normal data, it'd make sense for there to be 1 or 2 clusters as the best solution
print(gapstatfinal)
print(silhouettefinal)


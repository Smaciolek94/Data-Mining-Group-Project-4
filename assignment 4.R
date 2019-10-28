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

#calculating the gap statistic for each
gapstatkmeans <- clusGap(x,FUN = kmeans,K.max = 20,B = 10) # last argument is the total number of clusters
gapstatpam <- clusGap(x,FUN = pam,K.max = 20,B = 10)
gapstathier <- clusGap(x,FUN = hclust,K.max = 20, B=10)

plot(gapstatkmeans)
plot(gapstatpam)
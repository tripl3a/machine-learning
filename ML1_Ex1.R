
##################
### Exercise 1 ###
##################

df <- data.frame(
  Obs=1:6,
  X1=c(1,1,0,5,6,4),
  X2=c(4,3,4,1,2,0),
  Init=c(1,2,1,2,1,2)
)

df$clust <- df$Init
# initial plot:
plot(df$X1, df$X2, col=df$clust)

# centroid and distance calculation:
centroid1 <- c(mean(df$X1[df$clust==1]),
               mean(df$X2[df$clust==1]))
centroid2 <- c(mean(df$X1[df$clust==2]),
               mean(df$X2[df$clust==2]))

points(x=centroid1[1], y=centroid1[2], pch=23, col=1)
points(x=centroid2[1], y=centroid2[2], pch=23, col=2)

df$dist1 <- (df$X1 - centroid1[1])^2 + (df$X2 - centroid1[2])^2
df$dist2 <- (df$X1 - centroid2[1])^2 + (df$X2 - centroid2[2])^2

sum(df$dist1[df$clust==1])
sum(df$dist1[df$clust==2])

sum(df$dist2[df$clust==1])
sum(df$dist2[df$clust==2])

# assign new cluster based on lower distance:
df$clust <- ifelse(df$dist1 < df$dist2, 1, 2)

plot(df$X1, df$X2, col=df$clust)
points(x=centroid1[1], y=centroid1[2], pch=23, col=1)
points(x=centroid2[1], y=centroid2[2], pch=23, col=2)

###############################################
### Exercise 3 K-Means in R: Simulated Data ###
###############################################

# Simulate a matrix with two columns representing the x and y coordinates of 5 points. 
# The first 25 points will be shifted in a South-East direction to create two clusters.
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]<-x[1:25,1]+2
x[1:25,2]<-x[1:25,2]-2
plot(x,pch=16)

# Execute the following code one line at a time:
km.out<-kmeans(x,centers=2,nstart=1) #run with two clusters
km.out$cluster #a vector specifying which cluster each row belongs to
names(km.out) #all the different outputs from kmeans
km.out$totss #the sum of squares without clustering
km.out$tot.withinss #the sum of squares with this clustering
km.out$withinss #the sum of squares within each cluster
km.out$centers #Matrix with the center coordinates
plot(x,col=km.out$cluster+1,pch=16) #plot the poiunts colorfed by cluster
points(km.out$centers,col=2:3,pch=3) #add the cluster centers

# The kmeans command has an argument called nstart. This indicates how many times the algorithm
# is to be repeated using different random starting configurations. nstart=1 is too small, 
# you might be unlucky and have found a poor local minimum.

# Repeat this using 20 repeats
km.out<-kmeans(x,centers=2,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
points(km.out$centers,col=2:3,pch=3)
km.out$tot.withinss
# Ok it seems that the first attempt was a good (or the best) solution.

# We can now see what happens when we try to force more than two clusters to the data.
set.seed(4)
km.out<-kmeans(x,centers=3,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
km.out<-kmeans(x,centers=4,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
km.out$tot.withinss
# The within cluster sum of squares W is now much lower even though there are only really two clusters
# in our data matrix. W almost always decreases with increasing number of clusters 
# =>The best number of clusters to fit cannot be found by simply by minimising W.

# We will now work through a similar example where the data has 3 clusters.
x<-matrix(rnorm(50*3),ncol=2)
x[1:25,1]<-x[1:25,1]+2
x[1:25,2]<-x[1:25,2]-2
x[50+1:25,1]<-x[50+1:25,1]+2
x[50+1:25,2]<-x[50+1:25,2]+2
km.out<-kmeans(x,3,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
points(km.out$centers,col=2:4,pch=3)


############################################
### Exercise 4 Clustering City Locations ###
############################################

library(dplyr)
#install.packages("mdsr")
# instaling "mdsr" didn't work at first, the following two commands helped:
# 1. sudo apt-get install libxml2 libxml2-dev
# 2. sudo apt-get install libmysqlclient-dev
library(mdsr)

# Lets take a first look at the structure of the data frame.
names(WorldCities)
str(WorldCities)

# How many cities are listed in this data set?
dim(WorldCities)[1]

# We will restrict the data to just those with a population of at least 100 000 inhabitants using the filter
# command in the dplyr package, and then keep just two variables longitude and lattitude.
BigCities<-filter(WorldCities,population >= 100000)
BigCities<-select(BigCities,longitude, latitude)
# How many cities have at least one hundred thousand inhabitants?
dim(BigCities)[1]

# We will now run K-Means using 6 clusters, and plot the clusters.
set.seed(15)
city.km<-kmeans(BigCities,centers = 6)
with(BigCities,plot(longitude,latitude,col=city.km$cluster,pch=16,cex=0.6))
# What do you notice?

# It is important to realise that there is nothing in the data defining continent, 
# but the clustering method can easily and surprisingly effectively identify the continents.
# Try using different numbers of clusters from 2 upwards. Also notice that the borders of each cluster can
# differ depending on the seed.

par(mfrow=c(2,2))

# try different cluster sizes:
for (k in 3:6) {
  city.km<-kmeans(BigCities,centers = k)
  with(BigCities,plot(longitude,latitude,col=city.km$cluster,pch=16,cex=0.6
                      , main=paste( paste("k=",k,sep=""), paste("tot.withinss=",round(city.km$tot.withinss,0),sep="") ,sep=", " )))
}

# try multiple times with the same cluster size and w/o a seed (=> randomness):
for (i in 2:5) {
  k <- 6
  city.km<-kmeans(BigCities,centers = k)
  with(BigCities,plot(longitude,latitude,col=city.km$cluster,pch=16,cex=0.6
                      , main=paste( paste("k=",k,sep=""), paste("tot.withinss=",round(city.km$tot.withinss,0),sep="") ,sep=", " )))
}

par(mfrow=c(1,1)) # reset layout

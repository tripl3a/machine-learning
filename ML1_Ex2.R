##################
### Exercise 1 ###
##################

# As a similar exercise to Exercise 3 in Workshop 1, 
# we will simulate a three dimensional matrix with threeclusters.

# prerequisite: 
# 1. sudo apt-get install libglu1-mesa-dev
# 2. http://ubuntuhandbook.org/index.php/2017/06/install-freetype-2-8-in-ubuntu-16-04-17-04/)
#    sudo add-apt-repository ppa:glasen/freetype2
#    sudo apt update && sudo apt install freetype2-demos
# 3. sudo apt-get install libmagick++-dev

#install.packages("rgl", dependencies = TRUE)
library(rgl)

set.seed=(10)
x=matrix(rnorm(75*3),ncol=3)
x[1:25,1]=x[1:25,1]+5
x[51:75,2]=x[1:25,2]-6
truth<-rep(1:3,c(25,25,25))

pairs(x,col=truth+1)

# Scatter Plot & Correlation
library(psych)
pairs.panels(x
             , gap=0
             , bg=c("red","yellow","blue")[truth]
             , pch=21)

plot3d(x, size=5)

# (a) Make sure you understand the given code.

# (b) Rotate and play about with the 3D plot. Try to find a viewing angle that differentiates the clusters
# well. Notice that the clusters are well enough separated that with the correct viewing angle the
# colours are not required.

# (c) Run the kmeans function on this data with 3 clusters.

km.out3<-kmeans(x,centers=3,nstart=1) #run with three clusters

# (d) Compare the output clusters with the known clusters.

str(km.out3)
comp3 <- data.frame(truth=truth, km.clust=km.out3$cluster)
library(dplyr)
comp3 %>%
  group_by(truth,km.clust) %>%
  summarise(n = n())
count(comp3, truth, km.clust) # simplified

# (e) Colour the clusters and add the cluster means.
plot3d(x,col=km.out3$cluster+1, size=5)
plot3d(km.out3$centers,add=TRUE,col=2:4,type="s")

# (f) Repeat the above using just two clusters. How can you describe the two clusters in one sentence?

km.out2<-kmeans(x,centers=2,nstart=1) #run with two clusters
comp2 <- data.frame(truth=truth, km.clust=km.out2$cluster)
count(comp2, truth, km.clust)
plot3d(x,col=km.out2$cluster+1, size=5)
plot3d(km.out2$centers,add=TRUE,col=2:4,type="s")

# => There's one bigger cluster and one smaller cluster now and they are nicely separated
# when you look from the x1-x2-perspective, the x3-axis played a less important role 
# as the variance there seems to be smaller.

# Principal Component Analysis
library(psych)
pc <- prcomp(x,
             center = TRUE,
             scale. = TRUE)
summary(pc)  # here we can see how much of the variance is covered by each PC

# (g) Generate a new matrix called y with 10 columns instead of 3, define the clusters in exactly the
# same way as above.

set.seed=(123)
x=matrix(rnorm(75*10),ncol=10)
x[1:25,1]=x[1:25,1]+5
x[51:75,2]=x[1:25,2]-6
# shifted the same way as in (a) => the added variables become just noise
truth<-rep(1:3,c(25,25,25))

# Run kmeans with 3 centres on x and on y.
km.out10<-kmeans(x, centers=3)

# Use the R function table to compare how many rows have been correctly and incorrectly assigned
table(truth, km.out10$cluster)

#######################################################
### Exercise 2 USA arrests data: PCA and clustering ###
#######################################################

# (a) Obtain names of the four variables in USArrests.
names(USArrests)
str(USArrests)

# (b) For each variable obtain the mean and the standard deviation.
summary(USArrests)

# (c) Use prcomp to get the principal components for this dataset.

# Principal Component Analysis
pr.out <- prcomp(USArrests,
             center = TRUE,
             scale. = TRUE)
attributes(pr.out)
print(pr.out) 

# (d) What is the difference between pr.out$scale and pr.out$sd?
pr.out$center
mean(USArrests$Murder)

pr.out$scale
sd(USArrests$Murder)

pr.out$sdev # different from sd of original values
# prcomp$sdev (from R help): 
# the standard deviations of the principal components (i.e., the square 
# roots of the eigenvalues of the covariance/correlation matrix, though the 
# calculation is actually done with the singular values of the data matrix).

# (e) Create the scree plot and cumulative variance plot plots as in James. The first two Principal
# components explain 87% of the variance, so we will use just the first two.

pc.sum <- summary(pr.out)
print(pc.sum$importance)

plot(pc.sum$importance["Proportion of Variance",], 
     type="b", 
     main="USArrests PCA", 
     xlab="Principal Component",
     ylab="Prop. Variance Explained")

plot(pc.sum$importance["Cumulative Proportion",], 
     type="b", 
     main="USArrests PCA", 
     xlab="Principal Component", 
     ylab="Cumulative Porp. Variance Explained",
     ylim = c(0,1))

# (f) Obtain the biplot using

biplot(pr.out,xlabs=state.abb)
# state.abb contains the standard US state abbreviations in the correct order, giving a neater
# presentation, than that given in James et al.

# (g) Run K-means on the PCA data with 2 clusters and plot the results. Note that the function biplot
# does not allow the points to be coloured. Make sure you understand the two plotting commands
# below.

km.out<-kmeans(pr.out$x,centers=2,nstart=20)
plot(pr.out$x[,1:2],type="n")
text(pr.out$x[,1],pr.out$x[,2],labels=state.abb,col=km.out$cluster)

# Nicer Bi-Plot, that allows for colouring
#library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(pr.out,
              obs.scale = 1,
              var.scale = 1,
              groups = paste("Cluster",km.out$cluster),
              labels = state.abb,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68) # this % of data (variability) will be captured by ellipses
# add more layers to g
g <- g + scale_color_discrete(name = "")
g <- g + theme(legend.direction = "horizontal",
               legend.position = "top")
print(g)

# (h) Adapt the code below to obtain the within sum of squares using 1 to 10 clusters plotting the results.
# At which value of k is there an „Elbow”?

wss.vec<-rep(NA,10)
for(k in 1:10){
  km.out<-kmeans(USArrests,centers=k,nstart=20)
  wss.vec[k]<-km.out$tot.withinss
}
plot(wss.vec,type="b",xlab="k (# of clusters)",ylab="total within sum of squares")

# (i) Using the “best” number plot the principal components coloured by cluster. With just 4 variables a
# pairs plot is also worthwhile.

# Scatter Plot & Correlation
library(psych)
pairs.panels(USArrests, gap=0)

# (j) Repeat parts g, h and i clustering on the original USArrests data frame, and compare the results.

#############################################################
### Exercise 3 Hamburg decathlon data, PCA and clustering ###
#############################################################

setwd("~/git-reps/machine-learning")

decathlon <- read.csv2("Zehnkampf2017Hamburg.csv", 
                       stringsAsFactors = FALSE)[-c(1:3,seq(10,28,2))]
## transform times to seconds
hilf <- decathlon$Zeit.400m
decathlon$Zeit.400m <- 60*as.numeric(substr(hilf,1,2)) +
  as.numeric(gsub(",",".",substr(hilf,4,nchar(hilf))))
hilf <- decathlon$Zeit.1500m
decathlon$Zeit.1500m <- 60*as.numeric(substr(hilf,1,2)) +
  as.numeric(gsub(",",".",substr(hilf,4,nchar(hilf))))
colnames(decathlon) <- c("YearOfBirth", "Class", "Points",
                         "Day1", "Day2", "Time.100m", "LongJump", "ShotPut",
                         "HighJump", "Time.400m", "Hurdles", "Discus", "PoleVault",
                         "JavelinThrow", "Time.1500m")
#remove any competitors without complete data, and select just the event results
temp <- apply(decathlon,1,function(x) sum(is.na(x)))
clustermat<-decathlon[temp==0,6:15]
pairs(clustermat)

#================= TODO =================

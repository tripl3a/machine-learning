################################
### Exercise 1 EM clustering ###
################################

library(EMCluster)

head(USArrests)

par(mfrow=c(1,2))

# Run a K-means clustering with 4 clusters and plot the clusters for first 2 principal comonents
km.out <- kmeans(USArrests, 4)
pr.out <- prcomp(USArrests,scale=TRUE)
plot(pr.out$x[,1:2], type="n", asp=1)
text(pr.out$x[,1:2], labels=state.abb, col=km.out$cluster)

# The approach in the EMCluster package is to set up an initial solution using init.EM and then run the
# EM algorithm to get the optimal solution using the emcluster function. In the third step we obtain the
# probabilities of the optimal solution by running one more e.step.
emobj <- init.EM(USArrests, nclass = 4)
emclobj <- emcluster(USArrests, emobj, assign.class = TRUE)
emprobs <- round(e.step(USArrests, emobj = emclobj)$Gamma,3)

# When the data have more than two variables, the default plot for an EM-object (with class emret) is a
# parallel coordinates plot: This is rarely helpful.
plotem(emobj,USArrests,lwd=2)

# Instead plot the 1st 2 principal components again and compare this with the K-Means plot.
plot(pr.out$x[,1:2],type="n", asp=1)
text(pr.out$x[,1:2],labels=state.abb,col=emclobj$class)

par(mfrow=c(1,1))

# We can look at each K-means cluster in more detail by returning the EM-algorithm probabilities.
round(emprobs[km.out$cluster==1,],3)
round(emprobs[km.out$cluster==2,],3)
round(emprobs[km.out$cluster==3,],3)
round(emprobs[km.out$cluster==4,],3)

df <- data.frame(emprobs)
df[,1]+df[,2]+df[,3]+df[,4] # probabilities sum up to 1

##############################
### Exercise 2 PIOMAS data ###
##############################

setwd("~/git-reps/machine-learning/")

##data
PIOMAS<-read.table("data/PIOMAS.txt",header=FALSE,row.names=1)
names(PIOMAS)<-month.abb
dim(PIOMAS)
PIOMAS<-PIOMAS[-40,] # removes 2018
year<-as.numeric(row.names(PIOMAS))
matplot(t(PIOMAS),type="l",xlab="Month",ylab="Sea Ice Volume", main="PIOMAS measures for each year") # One line for each year 

# plot the mean values for each year
yave<-apply(PIOMAS, 1, mean)
plot(year, yave, ylab="Sea Ice Volume")

# K-means clustering
km.out <- kmeans(PIOMAS, centers=5)
plot(year, yave, ylab="Sea Ice Volume", col=km.out$cluster)
matplot(t(PIOMAS), type="l", xlab="Month", ylab="Sea Ice Volume", col=km.out$cluster)

# Scatter Plot & Correlation
library(psych)
pairs.panels(PIOMAS[,1:4] # just Jan-Apr
             , gap=0
             , pch=21)

# PCA
pc <- prcomp(PIOMAS, center=TRUE) # TODO: investigate center=TRUE
summary(pc)

# Bi-Plot
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = km.out$cluster,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68) # this % of data (variability) will be captured by ellipses
g <- g + scale_color_continuous(name = "")
g <- g + theme(legend.direction = "horizontal",
               legend.position = "top")
print(g)

# What do you notice about the variance in the first few PCs?
# PC1 and PC2 capture > 99% of the variance. PC1 alone captures 98%.
# There is a very high correlation between all the variables.
# IMPORTANT: 
# All arrows in the bi-plot point in the same directions and have rouglhy the same length
# => the yearly mean vector can be used (more or less) euqivalent to the measures for each month per year 

# These data form a time series and are very highly autocorrelated, the value of one value is 
# closely related to the value of the previous one. Most of this autocorrelation can be removed 
# from the data by considering the month on month differences in the sea ice volume. Notice that
# the first variable in each row is the difference February minus January, hence the odd variable names.

#read in the original data and convert to a vector
tt<-c(t(read.table("data/PIOMAS.txt",header=FALSE,row.names=1)))
tt[1:24]
#calculate the differences for the first 39 years of data
tt<-diff(tt[1:(39*12+1)]) #lagged diffs (compared to previous value)
tt[1:24]
#put back into data frame format
PIOdiff<-as.data.frame(matrix(tt,byrow=T,ncol=12))
#variable names
names(PIOdiff)<-month.abb[c(2:12,1)]
#row names
row.names(PIOdiff)<-row.names(PIOMAS)
matplot(t(PIOdiff),type="l")
# Large negative values imply a large amount of sea ice melting in that month.


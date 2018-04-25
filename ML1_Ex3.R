library("cluster")

#################################
### Exercise 1 PAM Clustering ###
#################################

row.names(USArrests)<-state.abb

library(psych)
pairs.panels(USArrests, gap=0, pch=21) # Scatter Plot & Correlation

# PAM clustering
pam.out<-pam(USArrests,k=4)
clusplot(pam.out,labels=3)

# silhouette plot
sp<-silhouette(pam.out)
plot(sp,col=1:4)
mean(sp[,"sil_width"])
abline(v=mean(sp[,"sil_width"]))
# A rough guide is a silhouette width over 0.4 is good. Negative silhouette widths
# suggest that maybe that element would be better assigned to the neighbour cluster. 

# The mean silhouette width can be used to assess which is the best number of clusters.
avesw.vec<-rep(NA,7)
for(k in 2:7)
  avesw.vec[k]<-mean(silhouette(pam(USArrests,k=k))[,"sil_width"])
plot(1:7,avesw.vec,type="b",ylim=c(0,0.6), xlab="number of clusters", ylab="mean silhouette width")
# Which number of clusters gives the largest mean silhouette width?

# Rerun the PAM algorithm with the optimal number of clusters
k_opt <- 2
pam.out<-pam(USArrests,k=k_opt)
clusplot(pam.out,labels=3)
sp<-silhouette(pam.out)
plot(sp,col=1:k_opt)
abline(v=mean(sp[,"sil_width"]))
# Compare the optimal PAM clustering with the K-Means result.
km.out<-kmeans(USArrests,centers=k_opt,nstart=20)
table(km.out$cluster,pam.out$clustering)
# There is no difference between the two. 
# Both assigned the same number of elements to each cluster.

# If you want to use PAM clustering using the Manhattan distance metric 
# there is an argument to pam called metric="manhattan".
pam.out<-pam(USArrests,k=k_opt,metric = "manhattan")
clusplot(pam.out,labels=3)
sp<-silhouette(pam.out)
plot(sp,col=1:k_opt)
abline(v=mean(sp[,"sil_width"]))
# Average silhoutte width euclidean was 0.59
# Average silhoutte width manhattan is 0.55
# => Manhattan performs worse than Euclidean (default)

# Silhouette plot for K-means
# The function sihouette was written for objects created by functions in the cluster package.
# kmeans is a function in the stats package loaded with the “base R” installation. As a result
# silhouette(km.out) doesn’t work, but with a little work we can coerce the data into the correct
# format. The first argument to silhouette should be the cluster vector km.out$cluster. The second
# argument should be a distance matrix dist(USArrests), a matrix containing the pairwise euclidean
# distance between each pair of points.
sp<-silhouette(km.out$cluster, dist(USArrests))
plot(sp,col=1:2)


##########################################
### Exercise 3 Hierarchical Clustering ###
##########################################

df <- data.frame (X1=c(4,5,6,10,14,15),
                  X2=c(2,5,11,2,7,9))

dd <- dist(df, method="euclidean"); dd
rank(dd)

par(mfrow=c(1,2))
plt = plot(df)
text(df, row.names(df), pos = 4)
hc<-hclust(dd, method="complete")
plot(hc)
par(mfrow=c(1,1))


# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))
install.packages("RGtk2", depen=T)
install.packages("flexclust")
library(flexclust)
library(cluster)
library(RGtk2)
library(rattle)
library(NbClust)


# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wines <- wine[,2:14]
wines<- scale(wines)
test<- wine[-1]

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wines)

# Exercise 2:
#   * How many clusters does this method suggest?

#       This method suggests using ~3 clusters

#   * Why does this method work? What's the intuition behind it?

#       This method works because it identifies the number of clusters that reduces the sum 
#       of squares the most. By reducing the SSE within the clusters, we are 
#       grouping the data into the least number of clusters where the group
#       of points in the cluster are most similar (lowest level of variation). 

#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wines, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

#     The method suggests using 3 clusters since it meets the highest number of criteria.  

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

set.seed(1234)
fit.km <- kmeans(wines, 3, nstart = 25)   #what is nstart and how does it effect the output? 
str(fit.km)
clusters <- fit.km$cluster
centers <- fit.km$centers


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

comparison <- table(wine$Type, fit.km$cluster)
randIndex(comparison)   # ARI = 0.897

#output
#   1  2  3
#1 59  0  0
#2  3 65  3
#3  0  0 48


# Yes this is a fairly good clustering - it's nearly identical to the types given in the
# original dataset.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wines, fit.km$cluster, color = TRUE, shade = TRUE)


# Visually this is a very good clustering. Three clusters 
# seem to characterize the dataset into distinct groups. 


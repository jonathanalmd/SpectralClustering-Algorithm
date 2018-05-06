# Unsupervised Learning

# It is typically used for scenarios like understanding the population demomgraphics, market segmentation, 
# social media trends, anomaly detection, etc.where the clusters are unknown to begin with.

# In training phase of K-Means, K observations are arbitrarily selected (known as centroids). 
# Each point in the vector space is assigned to a cluster represented by nearest (euclidean distance) centroid. 
# Once the clusters are formed, for each cluster the centroid is updated to the mean of all cluster members.
# And the cluster formation restarts with new centroids. This repeats until the centroids themselves become mean of clusters, i.e., 
# when updating centroids to mean doesn’t change them. The prediction of a test observation is done based on nearest centroid.

library(kknn)

data(iris)
data[is.na(data)] <- 0

train <- as.matrix(iris[,-5]) # using all features
#train <- as.matrix(iris[,c(3,4)]) # using 2 features

km <- kkmeans(train, centers=3)
K = kernelf(km)

plot(train, col=km)


# 'predict': closest kernel
# black, red, green

k <- kernelf(km)
c <- centers(km)
test <- c(5.0, 3.4, 1.2, 0.6)

k1 <- c(k(test, c[1,]))
k2 <- c(k(test, c[2,]))
k3 <- c(k(test, c[3,]))

which.max(c(k1,k2,k3))


library(tidyverse) 
df <- data.frame(train)
class <- km[1:150]
class <- replace(class, class == 1, 'a')
class <- replace(class, class == 2, 'b')
class <- replace(class, class == 3, 'c')
df["class"] <- class
ggplot(data = df, aes(x = Sepal.Length, y = Sepal.Width, colour = class)) + 
  geom_point()





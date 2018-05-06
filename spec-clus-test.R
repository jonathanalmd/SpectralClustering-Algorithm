# sspec documentation: https://www.rdocumentation.org/packages/kernlab/versions/0.9-26/topics/specc
# install.packages("kernlab")

library(kernlab)

data(iris)

train <- as.matrix(iris[,c(3,4)])

sc <- specc(train, centers=3)

sc
centers(sc)
size(sc)
withinss(sc)
train

plot(train, col=sc)


# 'predict': closest kernel
# black, red, green

k <- kernelf(sc)
c <- centers(sc)
test <- c(1.2,0.6)

k1 <- c(k(test, c[1,]))
k2 <- c(k(test, c[2,]))
k3 <- c(k(test, c[3,]))

which.max(c(k1,k2,k3))
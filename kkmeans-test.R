library(kknn)

data(iris)
train <- as.matrix(iris[,-5]) # using all features
#train <- as.matrix(iris[,c(3,4)]) # using 2 features

sc <- kkmeans(train, centers=3)
K = kernelf(sc)

plot(train, col=sc)


# 'predict': closest kernel
# black, red, green

k <- kernelf(sc)
c <- centers(sc)
test <- c(5.0, 3.4, 1.2, 0.6)

k1 <- c(k(test, c[1,]))
k2 <- c(k(test, c[2,]))
k3 <- c(k(test, c[3,]))

which.max(c(k1,k2,k3))


library(tidyverse) 
df <- data.frame(train)
ggplot(data = df, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()





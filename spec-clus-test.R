# sspec documentation: https://www.rdocumentation.org/packages/kernlab/versions/0.9-26/topics/specc
# install.packages("kernlab")
# Unsupervised learning

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


library(tidyverse) 
df <- data.frame(train)
class <- sc[1:150]
class <- replace(class, class == 1, 'a')
class <- replace(class, class == 2, 'b')
class <- replace(class, class == 3, 'c')
df["class"] <- class
ggplot(data = df, aes(x = Petal.Length, y = Petal.Width, colour = class)) + geom_point()



# 'predict': closest kernel
# black, red, green

k <- kernelf(sc)
c <- centers(sc)
test <- c(1.2,0.6)

k1 <- c(k(test, c[1,]))
k2 <- c(k(test, c[2,]))
k3 <- c(k(test, c[3,]))

which.max(c(k1,k2,k3))
# kmeans pseudocode
# http://www.devmedia.com.br/data-mining-na-pratica-algoritmo-k-means/4584

# similar algorithm
#http://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html

# ========================= Init =========================

source("set_wdir.r")
set_wdir()
input_data = paste(getwd(),"iris.data.txt",sep="/")
input_data
Dados <- read.csv(input_data, header=FALSE)
head(Dados)

#my.data <- as.matrix(Dados[,c(1,2)])
my.data <- as.matrix(Dados[,c(3,4)])

n <- nrow(my.data)
S <- my.data
A <- matrix(rep(0,n^2) ,nrow = n ,ncol=n)
sigma2 <- 5 # between 0.05 to 10
D <- diag(n)


# ========================= ALGORITHM =========================

# ========================== STEP 1 ==========================
for (i in 1:n){
  for(j in 1:n){
    if (i != j){
      # S[i,] -> row i and all columns 
      A[i,j] <- exp( - sqrt(sum((S[i,]-S[j,])^2)) / 2*sigma2)
      #A[i,j] <- exp(- norm(as.matrix(S[i,]-S[j,]), type="F"))
    }
  }  
}
# set precision (5)
round(A[1:8,1:8],5)

# ========================== STEP 2 ==========================
# sum of each row from A to build a diagonal matrix
for (i in 1:n){ 
  D[i,i] <- sum (A[i,])
}
round(D[1:8,1:8],5)

# sqrt of each element from matrix
raiz.D     <- sqrt (D)       #obs1 : raiz.D %*% raiz.D = D
                                  # raiz.D x raiz.D = D (mul matrix)
# get matrix inverse (solve())
Inv.raiz.D <- solve(raiz.D)  #obs2 : solve inversa da raiz = raiz da inversa  
                             # sqrt (solve (D)) = solve (sqrt (D))  
# compute L
L <- Inv.raiz.D %*% A %*% Inv.raiz.D

round(L[1:8,1:8],5)


# =========================== STEP 3 ==============================

# get eigenvector
autovet <- eigen (L)$vectors
#autoval <- eigen (L)$values 

# K = number of classes
k <- 3

# get the 3 first eigenvectors 
X <- autovet[,(1 : k)]

# go back to L matrix (matrix decomposition)
#dec.espec <- autovet %*% diag(autoval) %*% t(autovet)



# =========================== STEP 4 ===============================
# Init Y matrix 
Y <- matrix (0,nrow=n,ncol=k)

# for each element from X matrix: div sqrt of each row element (squared)
for(i in 1:n){
  for(j in 1:k){
    Y[i,j] <- X[i,j] / (sqrt (sum(X[i,j])^2))
  }
}
# Y: normalized matrix (only -1 and 1)

# =========================== STEP 5 ================================
# 3 classes (supervised learning)
# using R library
# km <- kmeans (Y,3,nstart=20)  
#km <- kmeans (X,3,nstart=20) 
# plot(S, col=km$cluster)


Y
xnew <- Y
# K-MEANS 
obs <- as.numeric()

# set centroids
#center1<- sample(seq(-1,1,by=0.1),3,replace=T)
#center2<- sample(seq(-1,1,by=0.1),3,replace=T)
#center3<- sample(seq(-1,1,by=0.1),3,replace=T)

#center1 <- xnew[1,]
#center2 <- xnew[70,]
#center3 <- xnew[149,]

#for(n in 1:150){
#  for(m in 1:3){
#    if (xnew[n,m] == -1){
#      xnew[n,m] = 0
#    }
#  }
#}

flag <- TRUE
while(flag){
  center1 <- xnew[sample(1:150,1),]
  center2 <- xnew[sample(1:150,1),]
  center3 <- xnew[sample(1:150,1),]
  if (!(all(center1 == center2) || all(center1 == center3) || all(center2 == center3)) ){
    icenter1 <- center1
    icenter2 <- center2
    icenter3 <- center3
    flag <- FALSE
  }
}

# Find centroids (different values between them)

for(n in 1:30000){ # update centroids (repeating)
  for(i in 1:150){ # 150 instances
    dist1<- sum((xnew[i,]-center1)^2)
    dist2<- sum((xnew[i,]-center2)^2)
    dist3<- sum((xnew[i,]-center3)^2)
    
    if(dist1<=dist2 && dist1<=dist3){
      obs[i]<-1 
    }
    else if(dist2<=dist1 && dist2<=dist3){
      obs[i]<-2
    }  
    else{
      obs[i]<-3
    }
  }
  
  grupo1<-xnew[(obs == 1),]
  grupo2<-xnew[(obs == 2),]
  grupo3<-xnew[(obs == 3),]
  
  d1 <- dim(grupo1)[1]
  d2 <- dim(grupo2)[1]
  d3 <- dim(grupo3)[1]
  
  # update centroids (classify to correct cluster)
  # if dim(cluster) = 0 -> do not update
  if (d1 != 0){
    center1<-c(mean(grupo1[,1]),mean(grupo1[,2]),mean(grupo1[,3]))
  }
  
  if (d2 != 0){
    center2<-c(mean(grupo2[,1]),mean(grupo2[,2]),mean(grupo2[,3]))
  }
  
  if (d3 != 0){
    center3<-c(mean(grupo3[,1]),mean(grupo3[,2]),mean(grupo3[,3]))
  }
  
}
km <- kmeans (Y,3,nstart=20)   
#km <- kmeans (X,3,nstart=20) 
plot(S, col=km$cluster)
km <- kmeans (Y,3,nstart=20)  
#km <- kmeans (X,3,nstart=20) 
plot(S, col=km$cluster, xlab = 'Petal Width', ylab = 'Petal Length')
# plot classified data
# S = data, obs = clusters
plot(S, col=obs, xlab = 'Petal Width', ylab = 'Petal Length')

xnew
center1
center2
center3

icenter1
icenter2
icenter3

# pseudocodigo kmeans
# http://www.devmedia.com.br/data-mining-na-pratica-algoritmo-k-means/4584

#http://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html
# usa knn junto no passo 1 e nao executa passo 4 do algoritmo do artigo
# meio diferente mas funciona

# ========================= INICIO =========================

# Dados <- read.csv("/Users/jalmeida/Documents/UnB/1_2017/Grafos/Listas/Prático/Projeto/proc-iris.data.txt", header=FALSE)
# Dados <- read.csv("/Users/jalmeida/Documents/UnB/1_2017/Grafos/Listas/Prático/Projeto/iris.data.txt", header=FALSE)
source("set_wdir.r")
set_wdir()
input_data = paste(getwd(),"iris.data.txt",sep="/")
input_data
Dados <- read.csv(input_data, header=FALSE)
head(Dados)
# printar inicio da tabela (6 primeiras linhas)

#my.data <- as.matrix(Dados[,c(1,2)])
my.data <- as.matrix(Dados[,c(3,4)])

n <- nrow(my.data)
S <- my.data
A <- matrix(rep(0,n^2) ,nrow = n ,ncol=n)
# sigma2 <- sum ((S - mean(S))^2) / (n)  #var pop
sigma2 <- 5 # variar de 0.05 a 10
              # 0.05 a 2.3 nao ajuda, começa a mudar a partir de 2.4 e não muda até 10
D <- diag(n)


# ========================= ALGORITMO =========================

# ========================== PASSO 1 ==========================
for (i in 1:n){
  for(j in 1:n){
    # matriz inicial é toda de 0
    if (i != j){
      # S[i,] -> pega linha i e todas as colunas
      A[i,j] <- exp( - sqrt(sum((S[i,]-S[j,])^2)) / 2*sigma2)
      #A[i,j] <- exp(- norm(as.matrix(S[i,]-S[j,]), type="F"))
    }
  }  
}
# definir 5 casas decimais
round(A[1:8,1:8],5)

# ========================== PASSO 2 ==========================
for (i in 1:n){ # pega a soma de cada linha do A e colocando na matriz diagonal
  D[i,i] <- sum (A[i,])
}
# definir 5 casas decimais
round(D[1:8,1:8],5)

# raiz de cada elemento da matriz
raiz.D     <- sqrt (D)       #obs1 : raiz.D %*% raiz.D = D
                                  # raiz.D x raiz.D = D (multiplicar matrizes)
# solve() para pegar inversa
Inv.raiz.D <- solve(raiz.D)  #obs2 : calcular inversa da raiz = raiz da inversa  
                             # sqrt (solve (D)) = solve (sqrt (D))  
# calcula o L
L <- Inv.raiz.D %*% A %*% Inv.raiz.D

# definir 5 casas decimais
round(L[1:8,1:8],5)


# =========================== PASSO 3 ==============================

# pega autovetor
autovet <- eigen (L)$vectors
#autoval <- eigen (L)$values 

# K = quantidade de classes
k <- 3

# pega os 3 (k) primeiros autovetores (de acordo com os K primeiros autovalores)
X <- autovet[,(1 : k)]

# se fizer isso da pra voltar para a matriz L (matriz decomposta)
#dec.espec <- autovet %*% diag(autoval) %*% t(autovet)



# =========================== PASSO 4 ===============================
# Inicializa matriz Y com 0 (n linhas e k colunas)
Y <- matrix (0,nrow=n,ncol=k)

# Pra cada elemento de X divide ele pelo sqrt de cada elemento da linha ao quadrado
for(i in 1:n){
  for(j in 1:k){
    Y[i,j] <- X[i,j] / (sqrt (sum(X[i,j])^2))
  }
}
# normalizacao resulta em matriz Y normalizada que só tem valores 1 e -1


# =========================== PASSO 5 ================================
# 3 classes (aprendizado supervisionado)
# km <- kmeans (Y,3,nstart=20)   # com y da ruim (nao da mais!)
#km <- kmeans (X,3,nstart=20) 
# plot(S, col=km$cluster)
Y
xnew <- Y
# K-MEANS 
# observações; cada instancia que vai ser classificada em uma das k (no caso 3) classes
obs <- as.numeric()

# seta centroides iniciais 
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

for(n in 1:40000){ # rodar varias vezes para ir aprimorando os centroides
  for(i in 1:150){ # 150 instancias 
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
  
  # atualiza centroide só se alguem tiver sido classificado nesse grupo, se dim(grupo) = 0 nao atualiza
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
km <- kmeans (Y,4,nstart=20)   # com y da ruim (nao da mais!)
#km <- kmeans (X,3,nstart=20) 
plot(S, col=km$cluster)
km <- kmeans (Y,3,nstart=20)   # com y da ruim (nao da mais!)
#km <- kmeans (X,3,nstart=20) 
plot(S, col=km$cluster)
# plota os pontos com a classificacao (lembrando: S = dados e obs = classificacao dos dados)
plot(S, col=obs)

xnew
center1
center2
center3

icenter1
icenter2
icenter3


# SEPAL LENGTH/WIDTH DATA

my.data <- as.matrix(Dados[,c(1,2)])

n <- nrow(my.data) # n = quantidade de colunas dos dados
S <- my.data # coloca os dados em S
A <- matrix(rep(0,n^2) ,nrow = n ,ncol=n) # declara matriz nxn toda zerada
# sigma2 <- sum ((S - mean(S))^2) / (n)  #var pop
sigma2 <- 7 # variar de 0.05 a 10
# 0.05 a 2.3 nao ajuda, começa a mudar a partir de 2.4 e não muda até 10
D <- diag(n) # declara matriz diagonal com 1 tamanho n

for (i in 1:n){
  for(j in 1:n){
    # matriz inicial é toda de 0
    if (i != j){
      # S[i,] -> pega linha i e todas as colunas
      A[i,j] <- exp( - sqrt(sum((S[i,]-S[j,])^2)) / 2*sigma2)
      #A[i,j] <- exp(- norm(as.matrix(S[i,]-S[j,]), type="F"))
    }
  }  
}
# definir 5 casas decimais
round(A[1:8,1:8],5)

for (i in 1:n){ # pega a soma de cada linha do A e colocando na matriz diagonal
  D[i,i] <- sum (A[i,])
}
# definir 5 casas decimais
round(D[1:8,1:8],5)

# tirar raiz de cada elemento da matriz
raiz.D     <- sqrt (D)       #obs1 : raiz.D %*% raiz.D = D
# raiz.D x raiz.D = D (multiplicar matrizes)
# solve() para pegar inversa
Inv.raiz.D <- solve(raiz.D)  #obs2 : calcular inversa da raiz = raiz da inversa  
# sqrt (solve (D)) = solve (sqrt (D))  
# calcula o L
L <- Inv.raiz.D %*% A %*% Inv.raiz.D

# definir 5 casas decimais
round(L[1:8,1:8],5)

# pega autovetor
autovet <- eigen (L)$vectors
#autoval <- eigen (L)$values 

# K = quantidade de classes
k <- 3

# pega os 3 (k) primeiros autovetores (de acordo com os K primeiros autovalores)
X <- autovet[,(1 : k)]
X
# se fizer isso da pra voltar para a matriz L (matriz decomposta)
#dec.espec <- autovet %*% diag(autoval) %*% t(autovet)

# Inicializa matriz Y com 0 (n linhas e k colunas)
Y <- matrix (0,nrow=n,ncol=k)

# Pra cada elemento de X divide ele pelo sqrt de cada elemento da linha ao quadrado
for(i in 1:n){
  for(j in 1:k){
    Y[i,j] <- X[i,j] / (sqrt (sum(X[i,j])^2))
  }
}
# normalizacao resulta em matriz Y normalizada que só tem valores 1 e -1
Y

# 3 classes (aprendizado supervisionado)
# Y
xnew <- Y
# K-MEANS 
# observações; cada instancia que vai ser classificada em uma das k (no caso 3) classes
obs <- as.numeric()

# seta centroides iniciais (criando combinacao de -1 e 1) -> RUIM
#center1<- sample(seq(-1,1,by=0.1),3,replace=T)
#center2<- sample(seq(-1,1,by=0.1),3,replace=T)
#center3<- sample(seq(-1,1,by=0.1),3,replace=T)

# seta centroides iniciais com heuristica (sabendo que esses 3 pontos sao disitntos)
#center1 <- xnew[1,] 
#center2 <- xnew[70,]
#center3 <- xnew[149,]

# trocar -1 por 0 para ficar tudo com valores 0 ou 1
#for(n in 1:150){
#  for(m in 1:3){
#    if (xnew[n,m] == -1){
#      xnew[n,m] = 0
#    }
#  }
#}

# setar centroides iniciais fazendo loop até que os aleatórios sejam centroides distintos
# fazer isso para conseguir bons resultados com kmeans
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


for(n in 1:20000){ # rodar 50 vezes para ir aprimorando os centroides
  for(i in 1:150){ # 150 instancias 
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
  
  # atualiza centroide só se alguem tiver sido classificado nesse grupo, se dim(grupo) = 0 nao atualiza
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
km <- kmeans (Y,4,nstart=20)   # com y da ruim (nao da mais!)
#km <- kmeans (X,3,nstart=20) 
plot(S, col=km$cluster)
km <- kmeans (Y,3,nstart=20)   # com y da ruim (nao da mais!)
#km <- kmeans (X,3,nstart=20) 
plot(S, col=km$cluster)
# plota os pontos com a classificacao (lembrando: S = dados e obs = classificacao dos dados)
plot(S, col=obs)

center1
center2
center3

icenter1
icenter2
icenter3
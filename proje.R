setwd("E:/all")
getwd()
d<-read.csv("train.csv")

norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

sigmoid<-function(z){
  g<-1/(1+exp(-z))
  return(g)
}

#Cost Function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}

#cikisimiz ya 0 yada 1 olacak sekilde yapiyoruz
Y<-d[1:200,81]
md<-median(Y)
for(i in 1:200){
  if(Y[i]>md){
    Y[i]<-1
  }
  else
    Y[i]<-0
}
x1<-norm(d[1:200,78])
x2<-norm(d[1:200,77])
x3<-norm(d[1:200,52])
x0<-c(1:200)
a2_0<-c(1:200)
for (i in 1:200) {
  x0[i]<-1
  a2_0[i]<-1
}

#data setimizi yeniden tasaraladik 
n_Data_set<-matrix(c(Y,x0,x1,x2,x3),nrow = 200,ncol=5)
dimnames(n_Data_set)[[2]] <- c("House_Priece", "x0", "YrSold", "MoSold","BedroomAbvGr")
X<-matrix(c(x0,x1,x2,x3),nrow = 200,ncol=4)

#layer2 matrixi hesapliyoruz
teta_A<-matrix(c(0,0,0,0,0,0,0,0,0,0,0,0),nrow = 3,ncol=4)
teta_A1<-t(teta_A)
a2<-sigmoid(X%*%teta_A1)
A2<-matrix(c(a2,a2_0),nrow = 200,ncol = 4)
A2_1<-matrix(c(A2[1:200]),nrow = 200,ncol = 1)
A2_2<-matrix(c(A2[201:400]),nrow = 200,ncol = 1)
A2_3<-matrix(c(A2[401:600]),nrow = 200,ncol = 1)
#layer3 matrixi hisablayalim
teta_2<-matrix(c(0,0,0,0),nrow = 1,ncol = 4)
teta_layer3<-t(teta_2)
A3<-sigmoid(A2%*%teta_layer3)

#simdi gradient descent algorithm yapalim
alpha <- 0.001
m <- nrow(X)
#Number of iterations
iterations <- 200

# updating thetas using gradient update
for(i in 1:iterations)
{
  #a21 repeat
  teta_A1[1] <- teta_A1[1] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i]))
  teta_A1[2] <- teta_A1[2] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x1[i])
  teta_A1[3] <- teta_A1[3] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x2[i])
  teta_A1[4] <- teta_A1[4] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x3[i])
  #a22 repeat
  teta_A1[5] <- teta_A1[5] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i]))
  teta_A1[6] <- teta_A1[6] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x1[i])
  teta_A1[7] <- teta_A1[7] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x2[i])
  teta_A1[8] <- teta_A1[8] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x3[i])
  #a23 repeat
  teta_A1[9] <- teta_A1[9] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i]))
  teta_A1[10] <- teta_A1[10] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x1[i])
  teta_A1[11] <- teta_A1[11] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x2[i])
  teta_A1[12] <- teta_A1[12] - alpha * (1/m) * sum((sigmoid(X%*%teta_A1)- Y[i])*x3[i])
 
  teta_layer3[1] <- teta_layer3[1] - alpha * (1/m) * sum((sigmoid(A2%*%teta_layer3)- Y[i]))
  teta_layer3[2] <- teta_layer3[2] - alpha * (1/m) * sum((sigmoid(A2%*%teta_layer3)- Y[i])*A2_1[i])
  teta_layer3[3] <- teta_layer3[3] - alpha * (1/m) * sum((sigmoid(A2%*%teta_layer3)- Y[i])*A2_2[i])
  teta_layer3[4] <- teta_layer3[4] - alpha * (1/m) * sum((sigmoid(A2%*%teta_layer3)- Y[i])*A2_3[i])
  
    cst<-cost(teta_layer3)
  print(cst)
  if(cst<0.68){
    break
  }
}


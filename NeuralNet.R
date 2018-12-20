setwd("E:/all")
getwd()
d<-read.csv("train.csv")
library("neuralnet")
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
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

for (i in 1:200) {
  x0[i]<-1
 
}


#data setimizi yeniden tasaraladik 
n_Data_set<-matrix(c(Y,x0,x1,x2,x3),nrow = 200,ncol=5)
dimnames(n_Data_set)[[2]] <- c("House_Priece", "x0", "YrSold", "MoSold","BedroomAbvGr")

# build the neural network (NN)
nnt<-neuralnet(Y ~ x0+x1+x2+x3, n_Data_set, hidden = 5, lifesign = "minimal", 
               linear.output = FALSE, threshold = 0.01)
# plot the NN
plot(nnt, rep = "best")
nnt$result.matrix
testset <- n_Data_set[1:200, ]
temp_test <- subset( n_Data_set,select = c("x0", "x1","x2", "x3"))
head(temp_test)
nnt.results <- compute(nnt, temp_test)
results <- data.frame(actual = Y, prediction = nnt.results$net.result)
results$prediction <- round(results$prediction)
results[1:15, ]

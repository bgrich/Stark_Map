n <- c(15,16,17)
l <- c(0,1,2,3,4,5,6,7)

NumberMatrix <- numeric()

for(i in 1:length(n)){
  for(j in 1:length(l)){
    
    if(l[j]==0){
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l[j],1/2))  
    } else{
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l[j],l[j]-1/2))
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l[j],l[j]+1/2))
    }
  }
}

n1 <- NumberMatrix[,1]
n2 <- NumberMatrix[,1]
l1 <- NumberMatrix[,2]
l2 <- NumberMatrix[,2]
j1 <- NumberMatrix[,3]
j2 <- NumberMatrix[,3]
size <- length(n1)

mat <- matrix(, nrow = size, ncol = size)
ZeroFieldEnergy <- matrix(0,nrow = size, ncol = size)

for(i in 1:size){
  ZeroFieldEnergy[i,i] <- -1/(n1[i]- QuantumDefect(n1[i],l1[i],j1[i]))^2/2
}


for(i in 1:size){
  for(j in 1:size){
    mat[i,j] <- StarkMatrixElem(n1[i],n2[j],l1[i],l2[j],j1[i],j2[j],1/2,1/2)
  print(paste("i = ", i, ", j = ", j, sep = ''))
  }
}

field <- seq(0,6000, by = 5)
field.au <- field/5.142e9
Energy <- numeric()

for(k in 1:length(field)){
  temp <- eigen(ZeroFieldEnergy+mat*field.au[k])
  Energy <- rbind(Energy, temp$values)
}

plot(c(min(field), max(field)),c(min(Energy),max(Energy))*2.19475e5, col=0)
plot(c(min(field), max(field)),c(-540,-440), col=0)
for(i in 1:ncol(Energy)){
  lines(field, Energy[,i]*2.19475e5)
}
points(field, Energy[,1])
points(field, Energy[,2])
points(field, Energy[,3])

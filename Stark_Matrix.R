n <- c(13,14,15,16,17,18)
# l <- c(0,1,2,3,4,5,6,7)

NumberMatrix <- numeric()

for(i in 1:length(n)){
  l <- 0
  for(j in 1:(n[i]-1)){
    if(l==0){
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,1/2))  
    } else{
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l-1/2))
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l+1/2))
    }
    l <- l + 1
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

for(i in 1:size){
  for(j in 1:size){
    mat[i,j] <- StarkMatrixElem(n1[i],n2[j],l1[i],l2[j],j1[i],j2[j],1/2,1/2)
  print(paste("i = ", i, ", j = ", j, sep = ''))
  }
}

write.csv(mat, "Stark_Matrix_Output.csv", row.names = FALSE)
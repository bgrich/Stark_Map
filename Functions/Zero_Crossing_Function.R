#Zero crossing function

ZeroCross <- function(x, y){
  ZeroPos <- numeric()
  ZeroX <- numeric()
  
  for(i in 1:(length(y)-1)){
    if(sign(y[i]) == 0){
      ZeroPos <- c(ZeroPos, i)
      ZeroX <- c(ZeroX, x[i])
      next
    }
    if(sign(y[i])!=sign(y[i+1])){
      ZeroPos <- c(ZeroPos, i)
      ZeroX <- c(ZeroX, x[i])
    }
  }
  if(length(ZeroPos) == 0){
    print("This vector contains no zero crossings.")
  } else { 
  OutputMatrix <- cbind(ZeroPos, ZeroX)
  colnames(OutputMatrix) <- c("Index", "X.position")
  OutputMatrix
  }
}
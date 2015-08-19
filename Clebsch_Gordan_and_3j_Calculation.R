#Calculating the 3j symbol and Clebsch-Gordan coefficient

Clebsch_Gordan <- function(j1,j2,m1,m2,j,mj){
  
  if((m1+m2)!=mj){
    CG <- 0
  } else {
    NotSumNumerator <- sqrt((2*j+1)*factorial(j1+j2-j)*factorial(j1-m1)*factorial(j2-m2)*factorial(j+mj)*factorial(j-mj))
    
    NotSumDenominator <- sqrt(factorial(j1+j2+j+1)*factorial(j1-j2+j)*factorial(-j1+j2+j)*factorial(j1+m1)*factorial(j2+m2))
    
    s <- 0
    sum <- 0
    sum_s <- 0
    repeat{
      D1 <- s
      D2 <- j1-m1-s
      D3 <- j-mj-s
      D4 <- j2-j+m1+s
      
      if((D1<0)|(D2<0)|(D3<0)|(D4<0)){
        break
      } else{
        
        sum_s <- (-1)^(s+j1-m1)*factorial(j1+m1+s)*factorial(j2+j-m1-s)/(factorial(D1)*factorial(D2)*factorial(D3)*factorial(D4))
        
        sum <- sum+sum_s
        s <- s+1
      }
    }
    CG <- (NotSumNumerator/NotSumDenominator)*sum
  }
  
  CG
}
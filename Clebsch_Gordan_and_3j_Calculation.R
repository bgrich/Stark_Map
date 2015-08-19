#Calculating the 3j symbol and Clebsch-Gordan coefficient

#Calculates the Clebsch-Gordan coefficient for an arbitrary j1, j2, m1, m2, j, and mj using the analytic form found in Bob's Atomic Physics notes.
Clebsch_Gordan <- function(j1,j2,m1,m2,j,mj){
  
  #If case for the conditions that m1+m2 = mj
  if((m1+m2)!=mj){
    CG <- 0
  } else {
    #Calculates the terms that are not in the summation
    NotSumNumerator <- sqrt((2*j+1)*factorial(j1+j2-j)*factorial(j1-m1)*factorial(j2-m2)*factorial(j+mj)*factorial(j-mj))
    
    NotSumDenominator <- sqrt(factorial(j1+j2+j+1)*factorial(j1-j2+j)*factorial(-j1+j2+j)*factorial(j1+m1)*factorial(j2+m2))
    
    #Settings for the summation. Summation only counts up to s = 4999. If j's and m's are such that one of the terms would be beyond N, then the code will not handle all possible cases.
    N <- 5000
    s <- 0
    sum <- 0
    sum_s <- 0
    for(i in 1:N){
      D1 <- s
      D2 <- j1-m1-s
      D3 <- j-mj-s
      D4 <- j2-j+m1+s
      
      #If any of the denominator terms (D1 to D4) are negative then the term returns zero
      if((D1<0)|(D2<0)|(D3<0)|(D4<0)){
        sum_s <- 0
      } else{
        
        #Calculates the terms in the summation at this step of s
        sum_s <- (-1)^(s+j1-m1)*factorial(j1+m1+s)*factorial(j2+j-m1-s)/(factorial(D1)*factorial(D2)*factorial(D3)*factorial(D4))
        
      }
      #Adds current term to the sum and increments s
      sum <- sum+sum_s
      s <- s+1
    }
    CG <- (NotSumNumerator/NotSumDenominator)*sum
  }
  
  CG
}

#Calculates the Wigner3j coefficient using the Clebsch-Gordan coefficient code above
Wigner3j <- function(j1,j2,j3,m1,m2,m3){
  output <- (-1)^(j1-j2-m3)*Clebsch_Gordan(j1,j2,m1,m2,j3,-m3)/sqrt(2*j3+1)
  output
}

#Calculates the Clebsch-Gordan coefficient using the analytic form found in Cornwell's Group Theory of Physics for arbitrary j1, j2, m1, m2, j, and mj.
Clebsch_GordanV2 <- function(j1,j2,m1,m2,j,mj){
  
  #If case for the conditions that m1+m2 = mj
  if((m1+m2)!=mj){
    CG <- 0
  } else {
    #Calculates the terms that are not in the summation
    NotSumTerm1 <- sqrt((2*j+1)*factorial(j1+j2-j)*factorial(j1-j2+j)*factorial(-j1+j2+j)/factorial(j1+j2+j+1))
    
    NotSumTerm2 <- sqrt(factorial(j1+m1)*factorial(j1-m1)*factorial(j2+m2)*factorial(j2-m2)*factorial(j+mj)*factorial(j-mj))
    
    #Settings for the summation. Summation only counts up to s = 4999. If j's and m's are such that one of the terms would be beyond N, then the code will not handle all possible cases.
    N <- 5000
    s <- 0
    sum <- 0
    sum_s <- 0
    for(i in 1:N){
      D1 <- s
      D2 <- j1+j2-j-s
      D3 <- j1-m1-s
      D4 <- j2+m2-s
      D5 <- j-j2+m1+s
      D6 <- j-j1-m2+s
      
      #If any of the denominator terms (D1 to D6) are negative then the term returns zero
      if((D1<0)|(D2<0)|(D3<0)|(D4<0)|(D5<0)|(D6<0)){
        sum_s <- 0
      } else{
        #Calculates the terms in the summation at this step of s
        sum_s <- (-1)^s*(factorial(D1)*factorial(D2)*factorial(D3)*factorial(D4)*factorial(D5)*factorial(D6))^(-1)
      
      }
      #Adds current term to the sum and increments s
    sum <- sum+sum_s
    s <- s+1
    }
    CG <- (NotSumTerm1*NotSumTerm2)*sum
  }
  
  CG
}
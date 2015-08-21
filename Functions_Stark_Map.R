#Functions for calculating the Stark Map

#Loads libraries
library(dplyr)
library(ggplot2)

##Function to determine the quantum defect delta_nlj of an arbitrary state in Rubidium-85
#Requires as input the principal quantum number n, orbital angular momentum number l, and total angular momentum number j
#For example, if l = 1 and j = 1/2, this is the np_1/2 state

QuantumDefect <- function(n,l,j){
  
  #Chooses the quantum defect parameters based on the l and j quantum numbers
  #The quantum defect parameters come from Table 16.2 of Rydberg Atoms by Thomas Gallagher, Pg 353
  if(l == 0){
    delta_0 <- 3.13109
    delta_2 <- 0.204
    delta_4 <- -1.8
    delta_6 <- 0
    delta_8 <- 0
  } else if(l == 1 & j == 1/2){
    delta_0 <- 2.65456
    delta_2 <- 0.388
    delta_4 <- -7.904
    delta_6 <- 116.437
    delta_8 <- -405.907
  } else if (l == 1 & j == 3/2){
    delta_0 <- 2.64145
    delta_2 <- 0.33
    delta_4 <- -0.97495
    delta_6 <- 14.6001
    delta_8 <- -44.7265
  } else if(l == 2){
    delta_0 <- 1.347157
    delta_2 <- -0.59553
    delta_4 <- -1.50517
    delta_6 <- -2.4206
    delta_8 <- 19.736
  } else if(l == 3){
    delta_0 <- 0.016312
    delta_2 <- -0.064007
    delta_4 <- -0.36005
    delta_6 <- 3.2390
    delta_8 <- 0
  } else{
    delta_0 <- 0
    delta_2 <- 0
    delta_4 <- 0
    delta_6 <- 0
    delta_8 <- 0
  }
  
  #Calculates the quantum defect delta_nlj based on Equation 16.19 from Rydberg Atoms by Gallagher (Pg. 351)
  delta <- delta_0 + delta_2/(n - delta_0)^2 + delta_4/(n - delta_0)^4 + delta_6/(n - delta_0)^6 + delta_8/(n - delta_0)^8
  
  delta
}

##Script for determining the radial matrix element for two arbitrary states in Rubidium-85

RadialMatrixElement <- function(k,n1,n2,l1,l2,j1,j2){
  #Constants and Settings
  #Number of Electrons
  Z <- 1
  
  #Quantum numbers for states 1 and 2. Primary quantum number n, orbital angular momentum l, total angular momentum j
  delta1 <- QuantumDefect(n1,l1,j1)
  delta2 <- QuantumDefect(n2,l2,j2)
  E1 <- -1/(2*(n1-delta1)^2)
  E2 <- -1/(2*(n2-delta2)^2)
  
  #Inner and outer turning points, core radius for both states
  r1_O <- 2*n1*(n1+15)
  r1_I <- (n1^2 - n1*sqrt(n1^2 - l1*(l1+1)))/2
  r2_O <- 2*n2*(n2+15)
  r2_I <- (n2^2 - n2*sqrt(n2^2 - l2*(l2+1)))/2
  
  #Core radius as a function of core polarizability r = (a_c)^(1/3)
  #Core polarizability is a_c = 9.0760 for Rubidium
  core.radius <- (9.0760)^(1/3)
  
  #Determine which outer turning point is the larger to set as starting point
  r_0 <- max(r1_O, r2_O)
  
  #Defining scaled x-axis ksi = sqrt(r), step size h, and starting point ksi_0 = sqrt(r_0) 
  ksi_0 <- sqrt(r_0)
  h <- 0.01
  ksi_1 <- ksi_0 - h
  ksi_2 <- ksi_1 - h
  
  #Initial wavefunction guesses
  Psi1_0 <- 10^-15
  Psi1_1 <- 10^-14
  
  Psi2_0 <- 10^-15
  Psi2_1 <- 10^-14
  
  #Defining terms to be used in Numerov Algorithm
  ksi_iminus1 <- ksi_0
  ksi_i <- ksi_1
  ksi_iplus1 <- ksi_2
  
  Psi1_iminus1 <- Psi1_0
  Psi1_i <- Psi1_1
  
  Psi2_iminus1 <- Psi2_0
  Psi2_i <- Psi2_1
  
  #Establishing Numerov integration data frame
  Integral <- data.frame(ksi = numeric(), Psi1 = numeric(), Psi2 = numeric(), N1_i = numeric(), N2_i = numeric(), Psi12 = numeric()) 
  
  if(r1_O < r2_O){
    row_0 <- data.frame(ksi = ksi_0, Psi1 = 0, Psi2 = Psi2_0, N1_i = 0, N2_i = 2*ksi_0^2*Psi2_0^2, Psi12 = 0)
    row_1 <- data.frame(ksi = ksi_1, Psi1 = 0, Psi2 = Psi2_1, N1_i = 0, N2_i = 2*ksi_1^2*Psi2_1^2, Psi12 = 0)
  } else{
    row_0 <- data.frame(ksi = ksi_0, Psi1 = Psi1_0, Psi2 = 0, N1_i = 2*ksi_0^2*Psi1_0^2, N2_i = 0, Psi12 = 0)
    row_1 <- data.frame(ksi = ksi_1, Psi1 = Psi1_1, Psi2 = 0, N1_i = 2*ksi_1^2*Psi1_1^2, N2_i = 0, Psi12 = 0)
  }
  
  Integral <- rbind(Integral, row_0, row_1)
  
  #Numerov Algorithm
  #Iterates algorithm until the condition of ksi_(i+1) < sqrt(r_I) or ksi_(i+1) < sqrt(core.radius) is met
  repeat{
    #When ksi_i is larger than the smallest of the starting points, the normalization for the larger outer turning point accumulates while the other does not
    if(ksi_i > sqrt(min(r1_O, r2_O))){
      #First statement is case when r1_O < r2_O, second statement is r2_O < r1_O
      if(r1_O<r2_O){
        g2_iplus1  <- -8*(ksi_iplus1^2*E2 + Z - (l2 + 1/4)*(l2 + 3/4)/(2*ksi_iplus1^2))
        g2_i       <- -8*(ksi_i^2*E2 + Z - (l2 + 1/4)*(l2 + 3/4)/(2*ksi_i^2))
        g2_iminus1 <- -8*(ksi_iminus1^2*E2 + Z - (l2 + 1/4)*(l2 + 3/4)/(2*ksi_iminus1^2))
        
        Psi2_iplus1 <- (Psi2_iminus1*(g2_iminus1 - 12/h^2)+Psi2_i*(10*g2_i+24/h^2))/(12/h^2 - g2_iplus1)
        
        N1_iplus1 <- 0
        N2_iplus1 <- 2*ksi_iplus1^2*Psi2_iplus1^2*h
        
        new.row <- data.frame(ksi = ksi_iplus1, Psi1 = 0, Psi2 = Psi2_iplus1, N1_i = N1_iplus1, N2_i=N2_iplus1, Psi12 = 0)
        
        if(ksi_iplus1<sqrt(max(r1_I,r2_I))|ksi_iplus1<sqrt(core.radius)){
          break
        } else {
          Integral <- rbind(Integral, new.row)
        }
        
      } else{
        g1_iplus1  <- -8*(ksi_iplus1^2*E1 + Z - (l1 + 1/4)*(l1 + 3/4)/(2*ksi_iplus1^2))
        g1_i       <- -8*(ksi_i^2*E1 + Z - (l1 + 1/4)*(l1 + 3/4)/(2*ksi_i^2))
        g1_iminus1 <- -8*(ksi_iminus1^2*E1 + Z - (l1 + 1/4)*(l1 + 3/4)/(2*ksi_iminus1^2))
        
        Psi1_iplus1 <- (Psi1_iminus1*(g1_iminus1 - 12/h^2)+Psi1_i*(10*g1_i+24/h^2))/(12/h^2 - g1_iplus1)
        
        N1_iplus1 <- 2*ksi_iplus1^2*Psi1_iplus1^2*h
        N2_iplus1 <- 0
        
        new.row <- data.frame(ksi = ksi_iplus1, Psi1 = Psi1_iplus1, Psi2 = 0, N1_i = N1_iplus1, N2_i=N2_iplus1, Psi12 = 0)
        
        if(ksi_iplus1<sqrt(max(r1_I,r2_I))|ksi_iplus1<sqrt(core.radius)){
          break
        } else {
          Integral <- rbind(Integral, new.row)
        }
      }
      
      
      if(r1_O<r2_O){
        Psi2_iminus1 <- Psi2_i
        Psi2_i <- Psi2_iplus1
      } else{
        Psi1_iminus1 <- Psi1_i
        Psi1_i <- Psi1_iplus1
      }
      
    } else{
      
      g1_iplus1  <- -8*(ksi_iplus1^2*E1 + Z - (l1 + 1/4)*(l1 + 3/4)/(2*ksi_iplus1^2))
      g1_i       <- -8*(ksi_i^2*E1 + Z - (l1 + 1/4)*(l1 + 3/4)/(2*ksi_i^2))
      g1_iminus1 <- -8*(ksi_iminus1^2*E1 + Z - (l1 + 1/4)*(l1 + 3/4)/(2*ksi_iminus1^2))
      g2_iplus1  <- -8*(ksi_iplus1^2*E2 + Z - (l2 + 1/4)*(l2 + 3/4)/(2*ksi_iplus1^2))
      g2_i       <- -8*(ksi_i^2*E2 + Z - (l2 + 1/4)*(l2 + 3/4)/(2*ksi_i^2))
      g2_iminus1 <- -8*(ksi_iminus1^2*E2 + Z - (l2 + 1/4)*(l2 + 3/4)/(2*ksi_iminus1^2))
      
      Psi1_iplus1 <- (Psi1_iminus1*(g1_iminus1 - 12/h^2)+Psi1_i*(10*g1_i+24/h^2))/(12/h^2 - g1_iplus1)
      Psi2_iplus1 <- (Psi2_iminus1*(g2_iminus1 - 12/h^2)+Psi2_i*(10*g2_i+24/h^2))/(12/h^2 - g2_iplus1)
      
      Psi12_iplus1 <- 2*Psi1_iplus1*Psi2_iplus1*ksi_iplus1^(2+2*k)*h
      N1_iplus1 <- 2*ksi_iplus1^2*Psi1_iplus1^2*h
      N2_iplus1 <- 2*ksi_iplus1^2*Psi2_iplus1^2*h
      
      new.row <- data.frame(ksi = ksi_iplus1, Psi1 = Psi1_iplus1, Psi2 = Psi2_iplus1, N1_i = N1_iplus1, N2_i=N2_iplus1, Psi12 = Psi12_iplus1)
      
      if(ksi_iplus1<sqrt(max(r1_I,r2_I))|ksi_iplus1<sqrt(core.radius)){
        break
      } else {
        Integral <- rbind(Integral, new.row)
      }
      Psi1_iminus1 <- Psi1_i
      Psi1_i <- Psi1_iplus1
      Psi2_iminus1 <- Psi2_i
      Psi2_i <- Psi2_iplus1
    }
    
    ksi_iminus1 <- ksi_i
    ksi_i <- ksi_iplus1
    ksi_iplus1 <- ksi_iplus1 - h 
  }
  
  Integral <- Integral %>%
    tbl_df()%>%
    mutate(r = ksi^2, R1 = Psi1/r^(3/4)/sqrt(sum(Integral$N1_i)), R2 = Psi2/r^(3/4)/sqrt(sum(Integral$N2_i)))
  
  RadialMatrixElement <- sum(Integral$Psi12)/(sqrt(sum(Integral$N1_i))*sqrt(sum(Integral$N2_i)))
  RadialMatrixElement
}

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

#Calculates the Spherical Harmonic contribution to the matrix element: <l,m|cos(theta)|l',m>. This is based on Zimmerman et al, PRA 20, 2251 (1979).
SphereMatElement <- function(l, lprime, ml){
  if((lprime==(l+1))|(lprime==(l-1))){
    #If lprime is equal to l-1, then this if statement is processed
    if(lprime == (l-1)){
      
      MatElem <- sqrt((l^2 - ml^2)/((2*l+1)*(2*l-1)))      
      
    }
    #If lprime is equal to l+1, then this if statement is processed
    if(lprime == (l+1)){
      
      MatElem <- sqrt(((l+1)^2 - ml^2)/((2*l+3)*(2*l+1)))
      
    }
  } else{
    
    MatElem <- "Error. Must meet condition l\' = l +/- 1."
    
  }
  
  MatElem
  
}


#Calculates the matrix element for the Stark effect contribution to the energy of each state. Accepts an arbitrary n1, n2, l1, l2, j1, j2, mj1, mj2

StarkMatrixElem <- function(n1, n2, l1, l2, j1, j2, mj1, mj2){
  
  #Determines if the two mj terms are the same, if they are not then the matrix element is set to zero
  if(mj1 != mj2){
    StarkElem <- 0
  } else{
    #Determines if l2 = l1 +/-1. If not, the matrix element is set to zero.
    if((l2==(l1+1))|(l2==(l1-1))){
      
      #Calculates the spherical harmonic matrix element for mj1+1/2. If it is equal to zero, sets that term in the summation equal to zero.
      if(SphereMatElement(l1,l2,mj1+1/2)==0){
        
        SumPlus <- 0
        
      } else{
        
        SumPlus <- Clebsch_Gordan(l1,1/2,mj1+1/2,-1/2,j1,mj1)*Clebsch_Gordan(l2,1/2,mj1+1/2,-1/2,j2,mj1)*SphereMatElement(l1,l2,mj1+1/2)
        
      }
      #Calculates the spherical harmonic matrix element for mj1-1/2. If it is equal to zero, sets that term in the summation equal to zero.
      if(SphereMatElement(l1,l2,mj1-1/2)==0){
        
        SumMinus <- 0
        
      } else{
        
        SumMinus <- Clebsch_Gordan(l1,1/2,mj1-1/2,1/2,j1,mj1)*Clebsch_Gordan(l2,1/2,mj1-1/2,1/2,j2,mj1)*SphereMatElement(l1,l2,mj1-1/2)
        
      }
      
      #Calculates the stark matrix element.
      StarkElem <- RadialMatrixElement(1,n1,n2,l1,l2,j1,j2)*(SumPlus + SumMinus)
      
    } else{
      StarkElem <- 0
    }
  }
  
  StarkElem
  
}
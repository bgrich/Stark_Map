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
    
    print(delta)
}
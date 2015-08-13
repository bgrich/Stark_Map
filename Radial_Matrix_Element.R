##Script for determining the radial matrix element for two arbitrary states in Rubidium-85

#Constants and Settings
#Number of Electrons
Z <- 1

#Quantum numbers for states 1 and 2. Primary quantum number n, orbital angular momentum l, total angular momentum j
n1 <- 32
n2 <- 33
l1 <- 1
l2 <- 0
j1 <- 3/2
j2 <- 1/2
delta1 <- QuantumDefect(n1,l1,j1)
delta2 <- QuantumDefect(n2,l2,j2)
E1 <- -1/(2*(n1-delta1)^2)
E2 <- -1/(2*(n2-delta2)^2)

#Inner and outer turning points, core radius for both states
r1_O <- 2*n1*(n1+15)
r1_I <- (n1^2 - n1*sqrt(n1^2 - l1*(l1+1)))/2
r2_O <- 2*n2*(n2+15)
r2_I <- (n2^2 - n2*sqrt(n2^2 - l2*(l2+1)))/2
core.radius <- 235e-12/0.529e-10

#Determine which outer turning point is the larger to set as starting point
r_0 <- max(r1_O, r2_O)

#Defining scaled x-axis ksi = sqrt(r), step size h, and starting point ksi_0 = sqrt(r_0) 
ksi_0 <- sqrt(r_O)
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
WaveFunction <- data.frame(ksi = numeric(), N1_i = numeric(), N2_i = numeric(), Psi12 = numeric()) 

if(r1_O < r2_O){
  row_0 <- data.frame(ksi = ksi_0, N1_i = 0, N2_i = 2*ksi_0^2*Psi2_0^2, Psi12 = 0)
  row_1 <- data.frame(ksi = ksi_1, N1_i = 0, N2_i = 2*ksi_1^2*Psi2_1^2, Psi12 = 0)
} else{
  row_0 <- data.frame(ksi = ksi_0, N1_i = 2*ksi_0^2*Psi1_0^2, N2_i = 0, Psi12 = 0)
  row_1 <- data.frame(ksi = ksi_1, N1_i = 2*ksi_1^2*Psi1_1^2, N2_i = 0, Psi12 = 0)
}


WaveFunction <- rbind(WaveFunction, row_0, row_1)


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
##Numerov Integration


##Constants and settings

#Electrons
Z <- 1

#Primary quantum number, quantum defect, angular quantum number, and energy
n <- 10
delta <- 0
l <- 2
E <- 1/(2*(n-delta)^2)

#Inner and outer turning points
r_O <- 3*n^2
r_I <- (n^2 - n*sqrt(n^2 - l*(l+1)))/2

#Defining scaled x-axis ksi = sqrt(r) and starting point ksi_0 = sqrt(r_O) 
ksi_0 <- sqrt(r_O)
h <- 0.01
ksi_1 <- ksi_0 - h
ksi_2 <- ksi_1 - h

#Initial wavefunction guesses
Psi_0 <- 10^-15
Psi_1 <- 10^-14

#Defining terms to be used in Numerov Algorithm
ksi_iminus1 <- ksi_0
ksi_i <- ksi_1
ksi_iplus1 <- ksi_2

Psi_iminus1 <- Psi_0
Psi_i <- Psi_1

WaveFunction <- data.frame(ksi = numeric(), Psi = numeric(), N_i = numeric()) 
row_0 <- data.frame(ksi = ksi_0, Psi = Psi_0, N_i = 2*ksi_0^2*Psi_0^2)
row_1 <- data.frame(ksi = ksi_1, Psi = Psi_1, N_i = 2*ksi_1^2*Psi_1^2)

WaveFunction <- rbind(WaveFunction, row_0, row_1)

#Numerov Algorithm
repeat{
  g_iplus1  <- -8*(ksi_iplus1^2*E+Z-(l+1/4)*(l+3/4)/(2*ksi_iplus1^2))
  g_i       <- -8*(ksi_i^2*E+Z-(l+1/4)*(l+3/4)/(2*ksi_i^2))
  g_iminus1 <- -8*(ksi_iminus1^2*E+Z-(l+1/4)*(l+3/4)/(2*ksi_iminus1^2))
  
  Psi_iplus1 <- (Psi_iminus1*(g_iminus1 - 12/h^2)+Psi_i*(10*g_i+24/h^2))/(12/h^2 - g_iplus1)
  
  N_i <- 2*ksi_iplus1^2*Psi_iplus1^2
  
  new.row <- data.frame(ksi = ksi_iplus1, Psi = Psi_iplus1, N_i = N_i)
  
  if((ksi_iplus1<sqrt(r_I))&(Psi_iplus1>Psi_i)){
    break
  } else {
    WaveFunction <- rbind(WaveFunction, new.row)
  }

  Psi_iminus1 <- Psi_i
  Psi_i <- Psi_iplus1
  ksi_iminus1 <- ksi_i
  ksi_i <- ksi_iplus1
  ksi_iplus1 <- ksi_iplus1 - h 
}

WaveFunction <- WaveFunction%>%
  tbl_df%>%
  mutate(r = ksi^2, R = Psi*r^(-3/4)/sqrt(sum(N_i)))

p <- WaveFunction%>%
  ggplot(aes(x=r, y = R))+
  geom_line()

print(p)
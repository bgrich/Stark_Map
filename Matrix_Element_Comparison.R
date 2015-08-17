#Script for comparing the matrix elements produced by my code and the elements produced by Bob's code

#S states for n = 31 to n = 34
#Declaration of various n's, l's, and j's
n1 <- c(rep.int(31,4),rep.int(32,3),rep.int(33,2),34)
n2 <- c(31,32,33,34,32,33,34,33,34,34)
l1 <- rep.int(0, 10)
l2 <- rep.int(0, 10)
j1 <- rep.int(1/2, 10)
j2 <- rep.int(1/2, 10)

#Creating initial data frame as a dplyr tbl_df
s.states <- tbl_df(data.frame(index = c(1:length(n1)), n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

#Producing the quantum defects and radial matrix elements
s.states <- s.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))
  
s.states <- s.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

#The radial matrix elements as produced by Bob's code (hand copied in)
BobElement.Sstates <- c(1164.987, 261.438, -93.031, 50.606, 1250.093, 280.206, -99.602, 1338.199, 299.623, 1429.304)

s.states <- cbind(s.states, BobElement.Sstates)

s.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.Sstates)))+
  geom_point()

###########################################################

#S to P_1/2 states for n = 31 to n = 34

n1 <- c(rep.int(31,4), rep.int(32,4), rep.int(33,4), rep.int(34,4))
n2 <- rep.int(c(31,32,33,34),4)
l1 <- rep.int(0, 16)
l2 <- rep.int(1, 16)
j1 <- rep.int(1/2, 16)
j2 <- rep.int(1/2, 16)

#Creating initial data frame as a dplyr tbl_df
p12.states <- tbl_df(data.frame(index = c(1:length(n1)), n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

#Producing the quantum defects and radial matrix elements
p12.states <- p12.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))

p12.states <- p12.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

BobElement.Pstates <- c(913.049, -108.889, 43.702,-24.504, 862.236, 978.698, -116.712, 46.823, -129.289, 924.151, 1046.625, -124.805, 55.583, -138.343, 988.211, 1116.828)

p12.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.Pstates)))+
  geom_point()


##############################

#S to P_3/2 states for n = 31 to n = 34

n1 <- c(rep.int(31,4), rep.int(32,4), rep.int(33,4), rep.int(34,4))
n2 <- rep.int(c(31,32,33,34),4)
l1 <- rep.int(0, 16)
l2 <- rep.int(1, 16)
j1 <- rep.int(1/2, 16)
j2 <- rep.int(3/2, 16)

#Creating initial data frame as a dplyr tbl_df
p32.states <- tbl_df(data.frame(index = c(1:length(n1)), n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

#Producing the quantum defects and radial matrix elements
p32.states <- p32.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))

p32.states <- p32.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

BobElement.P32states <- c(899.208, -113.715, 46.336, -26.180, 878.422, 963.836, -121.866, 49.635, -125.852, 941.463, 1030.703, -130.300, 53.559, -134.655, 1006.687, 1099.811)

p32.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.P32states)))+
  geom_point()


####################################################


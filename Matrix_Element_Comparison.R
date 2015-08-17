library(dplyr)

n1 <- c(rep.int(31,4),rep.int(32,3),rep.int(33,2),34)
n2 <- c(31,32,33,34,32,33,34,33,34,34)
l1 <- rep.int(0, 10)
l2 <- rep.int(0, 10)
j1 <- rep.int(1/2, 10)
j2 <- rep.int(1/2, 10)


s.states <- tbl_df(data.frame(n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

s.states <- s.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))
  
s.states <- s.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

BobElement <- c(1164.987, 261.438, -93.031, 50.606, 1250.093, 280.206, -99.602, 1338.199, 299.623, 1429.304)

s.states <- cbind(s.states, BobElement)

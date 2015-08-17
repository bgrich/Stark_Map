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

s.states <- s.states%>%
  rename(BobElement.States = BobElement.Sstates)

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

p12.states <- cbind(p12.states, BobElement.Pstates)

p12.states <- p12.states%>%
  rename(BobElement.States = BobElement.Pstates)

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

p32.states <- cbind(p32.states, BobElement.P32states)

p32.states <- p32.states%>%
  rename(BobElement.States = BobElement.P32states)

p32.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.P32states)))+
  geom_point()


####################################################

#S to D states for n = 31 to n = 34

n1 <- c(rep.int(31,4), rep.int(32,4), rep.int(33,4), rep.int(34,4))
n2 <- rep.int(c(31,32,33,34),4)
l1 <- rep.int(0, 16)
l2 <- rep.int(2, 16)
j1 <- rep.int(1/2, 16)
j2 <- rep.int(5/2, 16)

#Creating initial data frame as a dplyr tbl_df
d.states <- tbl_df(data.frame(index = c(1:length(n1)), n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

#Producing the quantum defects and radial matrix elements
d.states <- d.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))

d.states <- d.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

BobElement.Dstates <- c(-152.560, 75.690, -47.046, 32.889, 611.788, -162.976, 80.789, -50.176, 1232.251, 652.732, -173.732, 86.052, 29.152, 1317.428, 694.997, -184.830)

d.states <- cbind(d.states, BobElement.Dstates)

d.states <- d.states%>%
  rename(BobElement.States = BobElement.Dstates)

d.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.Dstates)))+
  geom_point()

########################################

#S to F states for n = 31 to n = 34

n1 <- c(rep.int(31,4), rep.int(32,4), rep.int(33,4), rep.int(34,4))
n2 <- rep.int(c(31,32,33,34),4)
l1 <- rep.int(0, 16)
l2 <- rep.int(3, 16)
j1 <- rep.int(1/2, 16)
j2 <- rep.int(5/2, 16)

#Creating initial data frame as a dplyr tbl_df
f.states <- tbl_df(data.frame(index = c(1:length(n1)), n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

#Producing the quantum defects and radial matrix elements
f.states <- f.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))

f.states <- f.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

BobElement.Fstates <- c(52.323, -35.826, 26.674, -20.966, -93.443, 55.211, -37.762, 28.089, 229.479, -98.588, 58.174, -39.745, 1427.349, 242.174, -103.869, 61.212)

f.states <- cbind(f.states, BobElement.Fstates)

f.states <- f.states%>%
  rename(BobElement.States = BobElement.Fstates)

f.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.Fstates)))+
  geom_point()
########################################

#S to G states for n = 31 to n = 34

n1 <- c(rep.int(31,4), rep.int(32,4), rep.int(33,4), rep.int(34,4))
n2 <- rep.int(c(31,32,33,34),4)
l1 <- rep.int(0, 16)
l2 <- rep.int(4, 16)
j1 <- rep.int(1/2, 16)
j2 <- rep.int(7/2, 16)

#Creating initial data frame as a dplyr tbl_df
g.states <- tbl_df(data.frame(index = c(1:length(n1)), n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

#Producing the quantum defects and radial matrix elements
g.states <- g.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))

g.states <- g.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

BobElement.Gstates <- c(64.134, -44.370, 33.270, -26.282, -111.760, 67.247, -46.505, 34.851, 262.469, -117.182, 70.443, -48.680, 1427.559, 275.235, -122.722, 73.704)

g.states <- cbind(g.states, BobElement.Gstates)

g.states <- g.states%>%
  rename(BobElement.States = BobElement.Gstates)

g.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.Gstates)))+
  geom_point()

########################################

#P32 to D states for n = 31 to n = 34

n1 <- c(rep.int(31,4), rep.int(32,4), rep.int(33,4), rep.int(34,4))
n2 <- rep.int(c(31,32,33,34),4)
l1 <- rep.int(1, 16)
l2 <- rep.int(2, 16)
j1 <- rep.int(3/2, 16)
j2 <- rep.int(5/2, 16)

#Creating initial data frame as a dplyr tbl_df
p32d.states <- tbl_df(data.frame(index = c(1:length(n1)), n1 = n1, n2 = n2, l1 = l1, l2 = l2, j1 = j1, j2 = j2))

#Producing the quantum defects and radial matrix elements
p32d.states <- p32d.states%>%
  group_by(n1,n2,l1,l2,j1,j2)%>%
  mutate(delta1 = QuantumDefect(n1,l1,j1), delta2 = QuantumDefect(n2,l2,j2))

p32d.states <- p32d.states%>%
  mutate(R12 = RadialMatrixElement(1,n1,n2,l1,l2,j1,j2))

BobElement.P32Dstates <- c(-3.455, -7.054, 6.394, -5.258, 1195.950, -4.135, -7.282, 6.652, 677.515, 1277.910, -4.817371, -7.510, -158.450, 724.503, 1362.578, -5.606)

p32d.states <- cbind(p32d.states, BobElement.P32Dstates)

p32d.states <- p32d.states%>%
  rename(BobElement.States = BobElement.P32Dstates)

p32d.states%>%
  ggplot(aes(x = index,y=(R12-BobElement.P32Dstates)))+
  geom_point()

###############
#Combine all of the data frames together

states <- tbl_df(rbind(s.states, p12.states, p32.states, d.states, f.states, g.states, p32d.states))

states <- states %>%
  mutate(Diff = R12 - BobElement.States)

#Figure of the differences

png("Difference Between Programs.png", width = 1920, height = 1080)
states %>%
  ggplot(aes(x = n1, y = Diff, shape = as.factor(l2), color = as.factor(l1)))+
  geom_point(size = 10)+
  theme_bw()+
  xlab("N_1")+
  ylab("Difference between programs (atomic units)")+
  labs(shape = "l2", color = "l1")+
  ggtitle("Difference between Matrix Elements of Bob's Program and My Code")+
  theme(axis.title = element_text(size = 20), plot.title = element_text(size = 20), legend.title = element_text(size = 20), axis.text = element_text(size = 20))+
  scale_color_brewer(palette = "Set1")
dev.off()


#Sets the n and mj levels for the Stark Matrix
n <- c(13,14,15,16,17,18,19,20)
# n <- c(25,26,27,28,29,30,31,32,33,34,35)
mj <- 1/2

#Initializes and fills a matrix with all of the n, l, and j states for the Stark matrix
NumberMatrix <- numeric()
#The lowest l level is determined by l = mj - 1/2. For that l level, there is only a single j state j = mj. For all higher l's, there are two j states, j = l +/- 1/2.
for(i in 1:length(n)){
  l0 <- mj - 1/2
  l <- l0
  for(j in l0+1:(n[i])){
    if(l == l0){
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,mj))
    } else{
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l-1/2))
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l+1/2))
    }
    l <- l + 1
  }
}

#Breaks the matrix of all possible states into several vectors for each n1, n2, l1, l2, j1, and j2. Also determines the size of the matrix.
n1 <- NumberMatrix[,1]
n2 <- NumberMatrix[,1]
l1 <- NumberMatrix[,2]
l2 <- NumberMatrix[,2]
j1 <- NumberMatrix[,3]
j2 <- NumberMatrix[,3]
size <- length(n1)

#Initializes and computes the Stark Matrix
StarkMatrix <- matrix(, nrow = size, ncol = size)
#Fills the Stark matrix. Treats the Stark matrix as symmetric and computes only the elements for the upper right triangle of the matrix. Copies those into the symmetric terms on the lower left triangle of the matrix.
for(i in 1:size){
  for(j in i:size){
    StarkMatrix[i,j] <- StarkMatrixElem(n1[i],n2[j],l1[i],l2[j],j1[i],j2[j],1/2,1/2)
    StarkMatrix[j,i] <- StarkMatrix[i,j]
    print(paste("i = ", i, ", j = ", j, sep = ''))
  }
}

#Saves the Stark matrix to file
write.csv(StarkMatrix, paste("Output_Files/Stark_Matrix_Output_",min(n),"_to_",max(n),"_mj_" ,mj,".csv", sep = ''), row.names = FALSE)

#Initializes and fills a matrix for the energy at zero electric field.
ZeroFieldEnergy <- matrix(0,nrow = size, ncol = size)

for(i in 1:size){
  ZeroFieldEnergy[i,i] <- -1/(n1[i]- QuantumDefect(n1[i],l1[i],j1[i]))^2/2
}

#Determines the number of electric field points and the step size.
field <- seq(0,6000, by = 5)
field.au <- field/5.142e9
Energy <- numeric()

#Diagonalizes a matrix of the zero field energy + the Stark matrix times the electric field. Also builds a matrix of the eigenvalues
for(k in 1:length(field)){
  Energy.newRow <- eigen(ZeroFieldEnergy+StarkMatrix*field.au[k])
  Energy <- rbind(Energy, Energy.newRow$values)
  print(field[k])
}

#Outputs the Stark energy data frame to file.
write.csv(Energy, paste("Output_Files/Stark_Energy_Output_",min(n),"_to_",max(n),"_mj_" ,mj,".csv", sep = ''), row.names = FALSE)

##Data tidying
#Creates a data frame with the zero field energy and the related n,l,j,mj and a string representing the entire state.

ZeroEnergyDataFrame <- data.frame(E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), mj = numeric(), state = character())

for(i in 1:size){
  ZeroEnergy.newrow <- data.frame(E0 = -1/(n1[i]- QuantumDefect(n1[i],l1[i],j1[i]))^2/2, n = n1[i], l = l1[i], j = j1[i],mj = mj, state = paste(n1[i],l1[i], j1[i],mj, sep = ','))
  
  ZeroEnergyDataFrame <- rbind(ZeroEnergyDataFrame, ZeroEnergy.newrow)
}
#Turns the data frame in to a dplyr table.
ZeroEnergyDataFrame <- tbl_df(ZeroEnergyDataFrame)

#Determines which direction the Energy eigenvalues are going and arranges the zero energy data frame to match. If the minimum zero field energy is the last column, then the Zero Field data frame is put in descending order. If the min zero field energy is the first column, then the Zero Field data frame is put in ascending order.
if(which.min(Energy[1,])>1){
  ZeroFieldDataFrame <- ZeroEnergyDataFrame%>%arrange(desc(E0), desc(l))  
} else{
  ZeroFieldDataFrame <- ZeroEnergyDataFrame%>%arrange(E0, l)
}

EnergyDataFrame <- data.frame(Field = numeric(), E = numeric(), E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), mj = numeric(), state = character())

#Creates a tidy data frame of the Energy eigen states at all fields
for(k in 1:length(Energy[1,])){
  print(k)
  for(i in 1:length(field)){
    EnergyDataFrame.newrow <- data.frame(Field = field[i],E = Energy[i,k],E0 = ZeroFieldDataFrame$E0[k], n = ZeroFieldDataFrame$n[k], l = ZeroFieldDataFrame$l[k], j = ZeroFieldDataFrame$j[k], mj = ZeroFieldDataFrame$mj[k], state = ZeroFieldDataFrame$state[k])
    
    EnergyDataFrame <- rbind(EnergyDataFrame, EnergyDataFrame.newrow)
  }
}
EnergyDataFrame <- tbl_df(EnergyDataFrame)

#Writes tidy data frame to file.
write.csv(EnergyDataFrame, paste("Output_Files/Tidy_Stark_Energy_Output_n_",min(n),"_to_",max(n),"_mj_",mj,".csv", sep = ''), row.names = FALSE)
StarkMatrix1 <- tbl_df(read.csv("Output_Files/QDAdjStark_Matrix_Output_27_to_34_mj_0.5.csv"))
StarkMatrix2 <- tbl_df(read.csv("Output_Files/QDAdjStark_Matrix_Output_27_to_34_mj_1.5.csv"))
StarkMatrix3 <- tbl_df(read.csv("Output_Files/QDAdjStark_Matrix_Output_27_to_34_mj_2.5.csv"))
StarkMatrix4 <- tbl_df(read.csv("Output_Files/QDAdjStark_Matrix_Output_27_to_34_mj_3.5.csv"))
TotalTidyEnergy <- tbl_df(data.frame(Field = numeric(), E = numeric(), E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), mj = numeric(), state = character()))


#Sets the n and mj levels for the Stark Matrix
n <- c(27,28,29,30,31,32,33,34)
mj <- c(1/2,3/2,5/2,7/2)

for(k in 1:length(mj)){
  #Initializes and fills a matrix with all of the n, l, and j states for the Stark matrix
  NumberMatrix <- numeric()
  #The lowest l level is determined by l = mj - 1/2. For that l level, there is only a single j state j = mj. For all higher l's, there are two j states, j = l +/- 1/2.
  for(i in 1:length(n)){
    l0 <- mj[k] - 1/2
    l <- l0
    for(j in (l0):(n[i]-1)){
      if(l == l0){
        NumberMatrix <- rbind(NumberMatrix, c(n[i],l,mj[k]))
      } else{
        NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l-1/2))
        NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l+1/2))
      }
      l <- l + 1
    }
  }
  
  #Adds in the lower l states for some additional nearby n's.
  nadd <- c((min(n)-7):(min(n)-1),(max(n)+1):(max(n)+7))
  for(i in 1:length(nadd)){
    l0 <- mj[k]-1/2
    l <- l0
    for(j in (l0):(5)){
      if(l == l0){
        NumberMatrix <- rbind(NumberMatrix, c(nadd[i],l,mj[k]))
      } else{
        NumberMatrix <- rbind(NumberMatrix, c(nadd[i],l,l-1/2))
        NumberMatrix <- rbind(NumberMatrix, c(nadd[i],l,l+1/2))
      }
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
  
  field <- seq(0,25, by = 0.05)
  field.au <- field/5.142e9
  Energy <- numeric()
  
  #Diagonalizes a matrix of the zero field energy + the Stark matrix times the electric field. Also builds a matrix of the eigenvalues
  
  ZeroFieldEnergy <- matrix(0,nrow = size, ncol = size)
  
  for(i in 1:size){
    ZeroFieldEnergy[i,i] <- -1/(n1[i]- QuantumDefectAdjusted(n1[i],l1[i],j1[i]))^2/2
  }
  
  for(i in 1:length(field)){
    Energy.newRow <- eigen(ZeroFieldEnergy+get(paste("StarkMatrix",k, sep= ''))*field.au[i])
    Energy <- rbind(Energy, Energy.newRow$values)
    print(field[i])
  }
  
  ZeroEnergyDataFrame <- data.frame(E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), mj = numeric(), state = character())
  
  for(i in 1:size){
    ZeroEnergy.newrow <- data.frame(E0 = -1/(n1[i]- QuantumDefectAdjusted(n1[i],l1[i],j1[i]))^2/2, n = n1[i], l = l1[i], j = j1[i],mj = mj, state = paste(n1[i],l1[i], j1[i],mj[k], sep = ','))
    
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
  for(i in 1:length(Energy[1,])){
    EnergyDataFrame.newPiece <- tbl_df(data.frame(Field = field, E = Energy[,i],E0 = ZeroFieldDataFrame$E0[i], n = ZeroFieldDataFrame$n[i], l = ZeroFieldDataFrame$l[i], j = ZeroFieldDataFrame$j[i], mj = ZeroFieldDataFrame$mj[i], state = ZeroFieldDataFrame$state[i]))
    EnergyDataFrame <- rbind(EnergyDataFrame, EnergyDataFrame.newPiece)
  }
  
  TotalTidyEnergy <- rbind(TotalTidyEnergy, EnergyDataFrame)
  
}  

write.csv(TotalTidyEnergy, "Output_Files/QDAdj_Tidy_Stark_Energy_n_27_to_34_mj_0.5_to_3.5.csv", row.names = FALSE)
n <- c(13,14,15,16,17,18)
# l <- c(0,1,2,3,4,5,6,7)
# n <- c(15,16)


NumberMatrix <- numeric()

for(i in 1:length(n)){
  l <- 0
  for(j in 1:(n[i]-1)){
    if(l==0){
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,1/2))  
    } else{
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l-1/2))
      NumberMatrix <- rbind(NumberMatrix, c(n[i],l,l+1/2))
    }
    l <- l + 1
  }
}

n1 <- NumberMatrix[,1]
n2 <- NumberMatrix[,1]
l1 <- NumberMatrix[,2]
l2 <- NumberMatrix[,2]
j1 <- NumberMatrix[,3]
j2 <- NumberMatrix[,3]
size <- length(n1)

mat <- matrix(, nrow = size, ncol = size)

for(i in 1:size){
  for(j in 1:size){
    mat[i,j] <- StarkMatrixElem(n1[i],n2[j],l1[i],l2[j],j1[i],j2[j],1/2,1/2)
    print(paste("i = ", i, ", j = ", j, sep = ''))
  }
}

write.csv(mat, paste("Stark_Matrix_Output_",min(n),"to",max(n),".csv", sep = ''), row.names = FALSE)

ZeroFieldEnergy <- matrix(0,nrow = size, ncol = size)

for(i in 1:size){
  ZeroFieldEnergy[i,i] <- -1/(n1[i]- QuantumDefect(n1[i],l1[i],j1[i]))^2/2
}

field <- seq(0,6000, by = 5)
field.au <- field/5.142e9
Energy <- numeric()

for(k in 1:length(field)){
  temp <- eigen(ZeroFieldEnergy+mat*field.au[k])
  Energy <- rbind(Energy, temp$values)
}

png(paste("Stark_Map_for_n_",min(n),"_to_", max(n),",png",sep=''), width = 1920, height = 1080)
plot(c(min(field), max(field)),c(-540,-440), col=0)
for(i in 1:ncol(Energy)){
  lines(field, Energy[,i]*2.19475e5)
}
dev.off()


ZeroEnergyDataFrame <- data.frame(E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), state = character())

for(i in 1:size){
  ZeroEnergy.newrow <- data.frame(E0 = -1/(n1[i]- QuantumDefect(n1[i],l1[i],j1[i]))^2/2, n = n1[i], l = l1[i], j = j1[i], state = paste(n1[i],l1[i], j1[i], sep = ','))
  
  ZeroEnergyDataFrame <- rbind(ZeroEnergyDataFrame, ZeroEnergy.newrow)
}
ZeroEnergyDataFrame <- tbl_df(ZeroEnergyDataFrame)

test.frame2 <- ZeroEnergyDataFrame%>%arrange(E0, l)


EnergyDataFrame <- data.frame(Field = numeric(), E = numeric(), E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), state = character())

for(k in 1:length(Energy[1,])){
  print(k)
  for(i in 1:length(field)){
    EnergyDataFrame.newrow <- data.frame(Field = field[i],E = Energy[i,k],E0 = test.frame2$E0[k], n = test.frame2$n[k], l = test.frame2$l[k], j = test.frame2$j[k], state = test.frame2$state[k])
    
    EnergyDataFrame <- rbind(EnergyDataFrame, EnergyDataFrame.newrow)
  }
}
EnergyDataFrame <- tbl_df(EnergyDataFrame)

write.csv(EnergyDataFrame, paste("Tidy_Stark_Energy_Output_n_",min(n),"_to_",max(n),".csv", sep = ''), row.names = FALSE)

# 
# EnergyDataFrame%>%
#   group_by(state)%>%
#   ggplot(aes(x=Field, y = E*2.19475e5, group=state))+
#   geom_line()+
#   scale_y_continuous(limits=c(-740,-600))
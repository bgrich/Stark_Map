n <- c(13,14,15,16,17,18)

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

plot(c(min(field), max(field)),c(min(Energy),max(Energy))*2.19475e5, col=0)
png("Stark Map for n equals 15.png", width = 1920, height = 1080)
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

EnergyDataFrame%>%
  group_by(state)%>%
  ggplot(aes(x=Field, y = E*2.19475e5, group=state))+
  geom_line()+
  scale_y_continuous(limits=c(-740,-600))
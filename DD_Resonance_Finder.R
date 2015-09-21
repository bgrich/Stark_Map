EnergyDataFrame <- tbl_df(read.csv("Output_Files/Tidy_Stark_Energy_n_27_to_34_mj_0.5_to_3.5.csv"))

EnergyDataFrame <- EnergyDataFrame %>%
  tbl_df() %>%
  mutate(Ecm = E*2.19475e5)

#Function for accepting an arbitrary state input and a given data frame and outputing the Zero crossings for the case 2*A - (B + C)

DDRes <- function(Frame, AFrame, StateB, StateC){
  
  ZeroCross(AFrame$Field, (2*AFrame$Ecm - ((Frame%>%filter(state %in% StateB)%>%select(Field, Ecm))$Ecm + (Frame%>%filter(state%in%StateC)%>%select(Field,Ecm))$Ecm)))
  
}

AFrame <- EnergyDataFrame%>%
  filter(state %in% "32,1,1.5,1.5")%>%
  select(Field, Ecm)

#Creates a vector with all of the states being investigated
States <- numeric()

#Low angular momentum states
for(n in 27:34){
  for(l in 0:4){
    for(j in c(l-1/2,l+1/2)){
      for(mj in c(1/2,3/2,5/2,7/2)){
        if(mj > j){
          next()
        }
        CurState <- paste(n,l,j,mj, sep = ",")
        States <- c(States, CurState)
      }
    }
  }
}

#Full n manifolds
for(n in 29:30){
  for(l in 5:(n-1)){
    for(j in c(l-1/2,l+1/2)){
      for(mj in c(1/2,3/2,5/2,7/2)){
        if(mj > j){
          next()
        }
        CurState <- paste(n,l,j,mj, sep = ",")
        States <- c(States, CurState)
      }
    }
  }
}

#Creates a blank data frame to save the zero crossing information to
ZeroCrossingDF <- tbl_df(data.frame(index = numeric(), Field = numeric(), state = character()))

#Determines the zero crossing and saves it to a data frame
for(i in 1:length(States)){
  for(j in i:length(States)){
    # A <- "32,1,1.5,1.5"
    B <- States[j]
    C <- States[i]
    
    res <- DDRes(EnergyDataFrame, AFrame, B, C)
    if(is.character(res)){
    } else {
      for(k in 1:length(res[,2])){
        if(res[k,2]>10&res[k,2]<13){
          new.Row <- data.frame(index = res[k,1], Voltage = res[k,2], state = paste(B,C, sep = ","))
          
          ZeroCrossingDF <- rbind(ZeroCrossingDF, new.Row)
        }
      }
    }
    print(paste(i,j,sep=","))
  }
}

write.csv(ZeroCrossingDF, "Output_Files/Zero_Crossing_mj_1.5.csv", row.names = FALSE)
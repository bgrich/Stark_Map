EnergyDataFrame2 <- tbl_df(read.csv("Output_Files/QDAdj_Tidy_Stark_Energy_n_27_to_34_mj_0.5_to_3.5.csv"))

EnergyDataFrame2 <- EnergyDataFrame2 %>%
  tbl_df() %>%
  mutate(Ecm = E*2.19475e5)

#Function for accepting an arbitrary state input and a given data frame and outputing the Zero crossings for the case 2*A - (B + C)

DDRes <- function(Frame, StateA, StateB, StateC){
  A <- Frame %>%
    filter(state %in% StateA) %>%
    select(Field, Ecm)
  B <- Frame %>%
    filter(state == StateB) %>%
    select(Field, Ecm)
  C <- Frame %>%
    filter(state == StateC) %>%
    select(Field, Ecm)

  ZeroCross(A$Field, (2*A$Ecm - (B$Ecm + C$Ecm)))
  
}

AFrame <- EnergyDataFrame2%>%
  filter(state %in% StateA)%>%
  select(Field, Ecm)

DDRestest <- function(Frame, StateA, StateB, StateC){
  A <- Frame %>%
    filter(state == StateA) %>%
    select(Field, Ecm)
  B <- Frame %>%
    filter(state == StateB) %>%
    select(Field, Ecm)
  C <- Frame %>%
    filter(state == StateC) %>%
    select(Field, Ecm)
  
  ZeroCross(A$Field, (2*A$Ecm - (B$Ecm + C$Ecm)))
  
}

DDRestest2 <- function(Frame, StateA, StateB, StateC){
    A <- Frame %>%
      filter(state %in% StateA) %>%
      select(Field, Ecm)
  B <- Frame %>%
    filter(state %in% StateB) %>%
    select(Field, Ecm)
  C <- Frame %>%
    filter(state %in% StateC) %>%
    select(Field, Ecm)
  
    ZeroCross(A$Field, (2*A$Ecm - (B$Ecm + C$Ecm)))
  
}

DDRestest3 <- function(Frame, AFrame, StateB, StateC){

    ZeroCross(AFrame$Field, (2*AFrame$Ecm - ((Frame%>%filter(state %in% StateB)%>%select(Field, Ecm))$Ecm + (Frame%>%filter(state%in%StateC)%>%select(Field,Ecm))$Ecm)))
  
}

AFrame <- EnergyDataFrame2%>%
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
ZeroCrossingDF <- tbl_df(data.frame(index = numeric(), Voltage = numeric(), state = character()))

#Determines the zero crossing and saves it to a data frame
for(i in 1:length(States)){
  for(j in i:length(States)){
#     A <- "32,1,1.5,1.5"
    B <- States[j]
    C <- States[i]
    
    res <- DDRestest3(EnergyDataFrame2, AFrame, B, C)
    if(is.character(res)){
    } else {
      for(k in 1:length(res[,2])){
      if(res[k,2]>10&res[2]<13){
        new.Row <- data.frame(index = res[1], Voltage = res[2], state = paste(B,C, sep = ","))
        ZeroCrossingDF <- rbind(ZeroCrossingDF, new.Row)
      }
      }
    }
    print(paste(i,j,sep=","))
  }
  #   print(i)
}

write.csv(ZeroCrossingDF, "Output_Files/Zero_Crossing_mj_0.5.csv", row.names = FALSE)
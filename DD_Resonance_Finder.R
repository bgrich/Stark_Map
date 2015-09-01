EnergyDataFrame1 <- tbl_df(read.csv("Output_Files/Tidy_Stark_Energy_n_27_to_34_mj_0.5_to_3.5.csv"))

EnergyDataFrame1 <- EnergyDataFrame1 %>%
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


a <- EnergyDataFrame2 %>%
  filter(state %in% "32,1,1.5,0.5") %>%
  select(Field, Ecm)
b <- EnergyDataFrame2 %>%
  filter(state == "32,0,0.5,0.5") %>%
  select(Field, Ecm)
c <- EnergyDataFrame2 %>%
  filter(state == "33,0,0.5,0.5") %>%
  select(Field, Ecm)

ZeroCross(a$Field, (2*a$Ecm - (b$Ecm + c$Ecm)))

plot(a$Field, (2*a$Ecm - (b$Ecm + c$Ecm)))

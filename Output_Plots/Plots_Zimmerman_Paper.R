#Graphing of the various data frames

#Importing files
TidyEnergy1 <- tbl_df(read.csv("Output_Files/Tidy_Stark_Energy_Output_n_13_to_20_mj_0.5.csv"))
TidyEnergy2 <- tbl_df(read.csv("Output_Files/Tidy_Stark_Energy_Output_n_13_to_20_mj_1.5.csv"))
TidyEnergy3 <- tbl_df(read.csv("Output_Files/Tidy_Stark_Energy_Output_n_13_to_20_mj_2.5.csv"))
TidyEnergy4 <- tbl_df(read.csv("Output_Files/Tidy_Stark_Energy_Output_n_13_to_20_mj_3.5.csv"))
TidyEnergyAdj1 <- tbl_df(read.csv("Output_Files/QDAdj_Tidy_Stark_Energy_Output_n_13_to_20_mj_0.5.csv"))
TidyEnergyAdj2 <- tbl_df(read.csv("Output_Files/QDAdj_Tidy_Stark_Energy_Output_n_13_to_20_mj_1.5.csv"))
TidyEnergyAdj3 <- tbl_df(read.csv("Output_Files/QDAdj_Tidy_Stark_Energy_Output_n_13_to_20_mj_2.5.csv"))
TidyEnergyAdj4 <- tbl_df(read.csv("Output_Files/QDAdj_Tidy_Stark_Energy_Output_n_13_to_20_mj_3.5.csv"))

#Mutating data frames to give energy in cm^-1
TidyEnergy1 <- TidyEnergy1 %>%
  mutate(Ecm = E*2.19475e5)
TidyEnergy2 <- TidyEnergy2 %>%
  mutate(Ecm = E*2.19475e5)
TidyEnergy3 <- TidyEnergy3 %>%
  mutate(Ecm = E*2.19475e5)
TidyEnergy4 <- TidyEnergy4 %>%
  mutate(Ecm = E*2.19475e5)
TidyEnergyAdj1 <- TidyEnergyAdj1 %>%
  mutate(Ecm = E*2.19475e5)
TidyEnergyAdj2 <- TidyEnergyAdj2 %>%
  mutate(Ecm = E*2.19475e5)
TidyEnergyAdj3 <- TidyEnergyAdj3 %>%
  mutate(Ecm = E*2.19475e5)
TidyEnergyAdj4 <- TidyEnergyAdj4 %>%
  mutate(Ecm = E*2.19475e5)


#Plots
TidyEnergyAdj4%>%
  group_by(state)%>%
  filter(Ecm > -540, Ecm < -440)%>%
  ggplot(aes(x=Field, y = Ecm, group=state))+
  geom_line()+
  ggtitle("Stark Map for Rb-85: n = 15, |mj| = 7/2")+
  xlab("Field (V/cm)")+
  ylab("Energy (cm^-1)")+
  theme_bw()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20), plot.title = element_text(size = 20))
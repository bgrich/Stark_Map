highlight <- EnergyDataFrame2 %>%
  filter(Ecm > -128, Ecm < -127, Field < 100) %>%
  filter(state %in% c("32,1,1.5,1.5"))

PStateGraph <- EnergyDataFrame2%>%
  group_by(state)%>%
  filter(Ecm > -128, Ecm < -127, Field < 100) %>%
  ggplot(aes(x=Field, y = Ecm, group=state))+
  geom_line(size = 2)+
  geom_line(data=highlight, color= "red", size = 2)+
  ggtitle("32P states for Rb-85")+
  xlab("Field (V/cm)")+
  ylab("Energy (cm^-1)")+
  theme_bw()+
  theme(axis.text = element_text(size = 35), plot.title = element_text(size = 35), axis.title = element_text(size = 35))

png("Output_Plots/32pStates.png", width = 1920, height = 1080)
PStateGraph + annotate("text", x = 0, y = -127.33, label = "32p_3/2", size = 10)+
  annotate("text", x = 0, y = -127.45, label = "32p_1/2", size = 10)+
  annotate("text", x = 20, y = -127.36, label = "|mj| = 3/2", size = 10)+
  annotate("text", x = 15, y = -127.40, label = "|mj| = 1/2", size = 10)
dev.off()
  
  

highlight2 <- EnergyDataFrame2 %>%
  filter(state %in% c("32,1,1.5,1.5" , "33,0,0.5,0.5" , "32,0,0.5,0.5"))

highlight3 <- EnergyDataFrame2 %>%
  filter(state %in% c("31,1,1.5,1.5", "33,1,1.5,1.5"))

DDResPlot <- EnergyDataFrame2 %>%
  group_by(state)%>%
  filter(Ecm > -140, Ecm < -110) %>%
  ggplot(aes(x = Field, y = Ecm, group = state))+
  geom_line()+
  geom_line(data = highlight2, color = "red", size = 2)+
  ggtitle("32P, 32s, and 33s states for Rb-85")+
  xlab("Field (V/cm)")+
  ylab("Energy (cm^-1)")+
  theme_bw()+
  theme(axis.text = element_text(size = 35), plot.title = element_text(size = 35), axis.title = element_text(size = 35))

png("DDRes_32p_32s_33s.png", width = 1920, height = 1080)
DDResPlot + 
  annotate("text", x = 0, y = -127, label = "32p", size = 10)+
  annotate("text", x = 0, y = -130, label = "n = 29", size = 10)+
  annotate("text", x = 0, y = -131.3, label = "32s", size = 10)+
  annotate("text", x = 0, y = -123.5, label = "33s", size = 10)
dev.off()
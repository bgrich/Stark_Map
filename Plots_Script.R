highlight <- TidyEnergy1 %>%
  filter(Ecm > -135, Ecm < -121, Field < 100) %>%
  filter(state %in% c("32,1,1.5,0.5" , "33,0,0.5,0.5" , "32,0,0.5,0.5"))

TidyEnergy1%>%
  group_by(state)%>%
  filter(Ecm > -135, Ecm < -121, Field < 100) %>%
  ggplot(aes(x=Field, y = Ecm, group=state))+
  geom_line()+
  geom_line(data=highlight, color= "red")+
  theme(axis.text = element_text(size = 20))

test1 <- TidyEnergy1 %>%
  group_by(Field)
filter(Ecm > -135, Ecm < -121, Field < 100) %>%
  filter(state %in% "32,1,1.5,0.5") %>%
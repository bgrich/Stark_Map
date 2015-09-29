ZeroCrossDF <- tbl_df(read.csv("Output_Files/Zero_Crossing_mj_1.5.csv"))

SM1 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_0.5.csv"))
SM2 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_1.5.csv"))
SM3 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_2.5.csv"))
SM4 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_3.5.csv"))

SM <- list(SM1, SM2, SM3, SM4)

test <- ZeroCrossDF %>%
  mutate(k = 1:n()) %>%
  group_by(k)%>%
  mutate(charstate = strsplit(as.character(state), split = ","))%>%
  mutate(n1 = as.numeric(unlist(charstate))[1], l1 = as.numeric(unlist(charstate))[2], j1 = as.numeric(unlist(charstate))[3], mj1 = as.numeric(unlist(charstate))[4], n2 = as.numeric(unlist(charstate))[5], l2 = as.numeric(unlist(charstate))[6], j2 = as.numeric(unlist(charstate))[7], mj2 = as.numeric(unlist(charstate))[8])%>%
  mutate(state1 = paste(n1, l1, j1, mj1, sep = ","), state2 = paste(n2, l2, j2, mj2, sep = ','))

mj <- c(0.5, 1.5, 2.5)

StarkElements <- data.frame(Field = numeric(), state1 = numeric(), state2 = numeric(), mat_elem1 = numeric(), mat_elem2 = numeric())


for(j in 1:length(mj)){
  for(q in 1:length(mj)){
    
    teststates <- test %>%
      filter(mj1 %in% mj[j] & mj2 %in% mj[q]) %>%
      select(Voltage, state1, state2)
    
    for(i in 1:nrow(teststates)){
      a1 <- stark_radial_mat_elem(SM[[j]], "32,1,1.5,1.5", teststates$state1[i], teststates$Voltage[i], 27, 34)
      a2 <- stark_radial_mat_elem(SM[[q]], "32,1,1.5,1.5", teststates$state2[i], teststates$Voltage[i], 27, 34)
      
      new_Row <- data.frame(Field = teststates$Voltage[i], state1 = teststates$state1[i], state2 = teststates$state2[i], mat_elem1 = a1, mat_elem2 = a2)
      
      StarkElements <- rbind(StarkElements, new_Row)
      
      print(paste(j,q,i, sep = ","))
      
    }
  }
}


StarkElements %>%
  tbl_df() %>%
  mutate(mat_elem_prod = mat_elem1 * mat_elem2, fract = mat_elem_prod / (964*941)) %>%
  filter(abs(fract) > 0.5e-2)

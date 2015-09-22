# Import all Stark matricies needed and build list

SM1 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_0.5.csv"))
SM2 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_1.5.csv"))
SM3 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_2.5.csv"))
SM4 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_3.5.csv"))

# StarkMatList <- list("0.5" = SM1, "1.5" = SM2, "2.5" = SM3, "3.5" = SM4)
# mj <- c(1/2, 3/2, 5/2, 7/2)
# 
# NumMatList <- list("0.5" = NumMat(27, 34, 1/2), "1.5" = NumMat(27, 34, 3/2),
#                    "2.5" = NumMat(27, 34, 5/2), "3.5" = NumMat(27, 34, 7/2))

# States
StateA <- "32,1,1.5,1.5"
StateB <- "32,0,0.5,0.5"
StateC <- "33,0,0.5,0.5"

StarkRadialMatrixElement <- function(stark_mat, initial_state, stark_state, field, n_min, n_max){

  field_au <- field/5.142e9
  initial_state_num <- as.numeric(unlist(strsplit(initial_state, split = ",")))
  final_state_num <- as.numeric(unlist(strsplit(stark_state, split = ",")))
  
  number_matrix <- NumMat(n_min, n_max, final_state_num[4])
  size <- nrow(number_matrix)
  
  zero_field_energy <- matrix(0,nrow = size, ncol = size)
  
  zero_field_frame <- data.frame(E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), mj = numeric(), state = character())
  
  for(j in 1:size){
    
    zero_field_energy[j,j] <- -1 / (number_matrix[j, 1]- QuantumDefect(number_matrix[j, 1],  number_matrix[j, 2], number_matrix[j, 3])) ^ 2 / 2
    
    new_Row <- data.frame(E0 = -1 / (number_matrix[j, 1]- QuantumDefect(number_matrix[j, 1],  number_matrix[j, 2], number_matrix[j, 3])) ^ 2 / 2, n = number_matrix[j, 1], l = number_matrix[j, 2], number_matrix[j, 3], mj = final_state_num[4], state = paste(n1[i],l1[i], j1[i],mj, sep = ','))
    
    zero_field_frame <- rbind(zero_field_frame, new_Row)
  }
  
  #Turns the data frame in to a dplyr table.
  unordered_data_frame <- tbl_df(zero_field_frame)
  ordered_data_frame <- tbl_df(zero_field_frame)
  
  # Computes the eigen-energies for the zero field states
  energy_order <- eigen(zero_field_energy)$values
  
  # Computes the eigenvectors for an arbitrary field
  field_vectors <- eigen(zero_field_energy + stark_mat*field_au)$vectors
  
  # Orders the zero field data frame to match the energy order from eigen  
  if(which.min(energy_order[1,])>1){
    ordered_data_frame <- ordered_data_frame%>%
      arrange(desc(E0), desc(l))  
  } else{
    ordered_data_frame <- ordered_data_frame%>%
      arrange(E0, l)
  }
  
  ordered_data_frame <- ordered_data_frame %>%
    mutate(index = c(1:nrow(ordered_data_frame)))
  
  unordered_data_frame <- unordered_data_frame %>%
    mutate(index = c(1:nrow(unordered_data_frame)))
  
  column <- as.numeric(ordered_data_frame %>%
                       filter(state %in% stark_state) %>%
                       select(index))
  
  stark_vector <- field_vectors[, column]
  
  unordered_data_frame <- unordered_data_frame %>%
    group_by(index)%>%
    mutate(charstates = strsplit(as.character(state), split = ","))%>%
    mutate(n1 = as.numeric(unlist(charstates)[1]), l1 = as.numeric(unlist(charstates)[2]), j1 = as.numeric(unlist(charstates)[3]), mj1 = as.numeric(unlist(charstates)[4]))%>%
    ungroup()
  
  state_index <- unordered_data_frame%>%
    filter(l1 %in% (initial_state_num[2]-1) | l1 %in% (initial_state_num[2] + 1))%>%
    select(index)
  
  matrix_elem <- numeric()
  for(i in 1:length(state_index$index)){
    rad_mat_elem <- abs(RadialMatrixElement(1, initial_state_num[1], unordered_data_frame$n1[state_index$index[i]], intial_state_num[2], unordered_data_frame$l1[state_index$index[i]], initial_state_num[3], final_state_num[3]))
    
    matrix_elem <- c(matrix_elem, rad_mat_elem * stark_vector[state_index$index[i]])
  }
  sum(MatrixElemB)
  
}

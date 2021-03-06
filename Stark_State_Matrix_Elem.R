# # Import all Stark matricies needed and build list
# 
# SM1 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_0.5.csv"))
# SM2 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_1.5.csv"))
# SM3 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_2.5.csv"))
# SM4 <- as.matrix(read.csv("Output_Files/Stark_Matrix_Output_27_to_34_mj_3.5.csv"))

# Calculates the radial matrix element for a given initial state and an arbitrary
# stark state related to some zero field state at an arbitrary field
stark_radial_mat_elem <- function(stark_mat, initial_state, stark_state, field, n_min, n_max){

  # Converts the field to atomic units
  field_au <- field/5.142e9
  
  # Splits the initial and Stark state strings into numerics
  initial_state_num <- as.numeric(unlist(strsplit(initial_state, split = ",")))
  final_state_num <- as.numeric(unlist(strsplit(stark_state, split = ",")))
  
  # Creates the number matrix and sets the size
  number_matrix <- NumMat(n_min, n_max, final_state_num[4])
  size <- nrow(number_matrix)
  
  # Initializes the zero-field energy matrix
  zero_field_energy <- matrix(0,nrow = size, ncol = size)
  
  # Initializes the zero-field data frame
  zero_field_frame <- data.frame(E0 = numeric(), n = numeric(), l = numeric(), j = numeric(), mj = numeric(), state = character())
  
  # Builds the zero-field matrix and data frame
  for(i in 1:size){
    
    zero_field_energy[i, i] <- -1 / (number_matrix[i, 1]- QuantumDefect(number_matrix[i, 1],  number_matrix[i, 2], number_matrix[i, 3])) ^ 2 / 2
    
    new_Row <- data.frame(E0 = -1 / (number_matrix[i, 1]- QuantumDefect(number_matrix[i, 1],  number_matrix[i, 2], number_matrix[i, 3])) ^ 2 / 2, n = number_matrix[i, 1], l = number_matrix[i, 2], j = number_matrix[i, 3], mj = final_state_num[4], state = paste(number_matrix[i, 1], number_matrix[i, 2], number_matrix[i, 3], final_state_num[4], sep = ','))
    
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
  if(which.min(energy_order)>1){
    ordered_data_frame <- ordered_data_frame%>%
      arrange(desc(E0), desc(l))  
  } else{
    ordered_data_frame <- ordered_data_frame%>%
      arrange(E0, l)
  }
  
  # Mutates the ordered and unordered data frames to have an index
  ordered_data_frame <- ordered_data_frame %>%
    mutate(index = c(1:nrow(ordered_data_frame)))
  
  unordered_data_frame <- unordered_data_frame %>%
    mutate(index = c(1:nrow(unordered_data_frame)))
  
  # Determines the column of the Stark state eigen vector
  column <- as.numeric(ordered_data_frame %>%
                       filter(state %in% stark_state) %>%
                       select(index))
  
  # Sets the Stark state eigenvector
  stark_vector <- field_vectors[, column]
  
  # Splits the state character into numerics for later use
  unordered_data_frame <- unordered_data_frame %>%
    group_by(index)%>%
    mutate(charstates = strsplit(as.character(state), split = ","))%>%
    mutate(n1 = as.numeric(unlist(charstates)[1]), l1 = as.numeric(unlist(charstates)[2]), j1 = as.numeric(unlist(charstates)[3]), mj1 = as.numeric(unlist(charstates)[4]))%>%
    ungroup()
  
  # Matches states with the l +/- 1 states of the initial state
  state_index <- unordered_data_frame%>%
    filter(l1 %in% (initial_state_num[2]-1) | l1 %in% (initial_state_num[2] + 1))%>%
    select(index)
  
  # Determines the matrix element for each possible state and sums them
  matrix_elem <- numeric()
  for(i in 1:length(state_index$index)){
    rad_mat_elem <- RadialMatrixElement(1, initial_state_num[1], unordered_data_frame$n1[state_index$index[i]], initial_state_num[2], unordered_data_frame$l1[state_index$index[i]], initial_state_num[3], final_state_num[3])
    
    matrix_elem <- c(matrix_elem, rad_mat_elem * stark_vector[state_index$index[i]])
  }
  sum(matrix_elem)
  
}

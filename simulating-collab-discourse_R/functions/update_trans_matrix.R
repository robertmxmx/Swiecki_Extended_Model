#function for updating the interaction level of an individual given 
#a transition matrix

##matrix = transition matrix
##speaker = row to update
##val = new diagonal value

rand_vec_cont <- function(N, M) {
  vec <- rbeta(n = N,shape1 = 1.1,shape2 = 5)
  vec / sum(vec) * M
}


update_trans_matrix = function(t_matrix,speaker,val){
  size = nrow(t_matrix)
  t_matrix = as.numeric(t_matrix)
  speaker = as.numeric(speaker)
  t_matrix = matrix(t_matrix,nrow = size,ncol = size)
  new_row = t_matrix[speaker,]
  speaker_val = new_row[speaker]
  
  if(val < 0 | val > 1){
    stop("New value must be between zero and one.")
  }else{
    new_row[speaker] = val
    leftover = 1-val
    new_row[-speaker] = rand_vec_cont(N = length(new_row)-1,M = leftover)
    t_matrix[speaker,] = new_row
    #t_matrix = round(t_matrix,3)
  }
  return(t_matrix)
}

update_trans_matrix_delta = function(matrix,speaker,delta){
  speaker = as.numeric(speaker)
  new_row = matrix[speaker,]
  speaker_val = new_row[speaker]
  new_val = speaker_val + delta

  if(new_val < 0 | new_val > 1){
    stop("New value must be between zero and one. Update your delta accordingly")
  }else{
    new_row[speaker] = new_val
    leftover = 1-new_val
    new_row[-speaker] = rand_vec_cont(N = length(new_row)-1,M = leftover)
    matrix[speaker,] = new_row
    team.interactivity = team_interactivity(matrix = matrix,nrow(matrix))
    team.interactivity = rep(team.interactivity,nrow(matrix))
    matrix = cbind(matrix,team.interactivity)
    matrix = round(matrix,3)
  }
  return(matrix)
}
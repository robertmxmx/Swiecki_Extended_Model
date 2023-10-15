#sim_discourse_1.0
#function for simulating collaborative discourse.
#speaker sequence is based on transition matrix
#talk content is selected randomly

##speakers = no. of speakers
##codes = no. of codes
##steps = no. of lines

sim_discourse_1.0 = function(codes,steps,trans_mat){

  #simulate talk sequence using transition matrix
  mat_ = trans_mat
  speaker_seq = sim_speaker_sequence(mat_,steps)$sequence

  #simulate code occurrences
  code_vals = sample(x = c(1,0),size = codes*steps,replace = TRUE)
  code_df = data.frame(matrix(data = code_vals,nrow = steps,ncol = codes))
  code_names = LETTERS[1:codes]
  colnames(code_df) = code_names
  
  #combine speakers and codes
  sim_dis = code_df
  sim_dis$Speaker = speaker_seq
  sim_dis$Run = rep(det(trans_mat),nrow(sim_dis)) #getting unique identifier for the matrix
  sim_dis$Run = as.character(sim_dis$Run)
return(sim_dis)
}
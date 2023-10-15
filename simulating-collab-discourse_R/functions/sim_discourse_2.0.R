#DEPRECATED CODE AND SPEAKER MATCHING NOT WORKING

##sim_discourse_2.0
#function for simulating collaborative discourse.
#speaker sequence is based on transition matrix
#talk content is based on code probability matrix for team

##speakers = no. of speakers
##codes = no. of codes
##steps = no. of lines
##trans_mat = speaker transition matrix
##codes_mat = code prob matrix for team

sim_discourse_2.0 = function(codes,steps,trans_mat,codes_mat){
  
  #simulate talk sequence using transition matrix
  mat_ = trans_mat
  speaker_seq_info = sim_speaker_sequence(mat_,steps)
  speaker_seq = speaker_seq_info$sequence
  speaker_counts = speaker_seq_info$counts
  #browser()
  
  #simulated code occurrences using code frequency matrix
  codes.list = list()
  for (i in 1:length(speaker_counts)){
    code.freqs = codes_mat[i,]
    code.df = data.frame(matrix(data = NA,
                                nrow = speaker_counts[[i]],
                                ncol = length(code.freqs))
                         )
    for(j in 1:length(code.freqs)){
      code.vec = sample(x = c(0,1),
                        size = speaker_counts[[i]],
                        replace = TRUE,
                        prob = c(1-code.freqs[j], code.freqs[j])
                        )
      code.df[,j] = code.vec
    }
    codes.list[[i]] = code.df
  }
  #bind rows
  coded.data = bind_rows(codes.list,.id = "id")
  #reorder data according to transition matrix NOT WORKING
  browser()
  coded.data = coded.data %>% slice(match(speaker_seq, id))
 

  #combine speakers and codes
  sim_dis = code_df
  sim_dis$Speaker = speaker_seq
  sim_dis$Run = rep(det(trans_mat),nrow(sim_dis)) #getting unique identifier for the matrix
  sim_dis$Run = as.character(sim_dis$Run)
  return(sim_dis)
}


# #simulate code occurrences
# code_vals = sample(x = c(1,0),size = codes*steps,replace = TRUE)
# code_df = data.frame(matrix(data = code_vals,nrow = steps,ncol = codes))
# code_names = LETTERS[1:codes]
# colnames(code_df) = code_names
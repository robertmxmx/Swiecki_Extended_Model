#function for simulating random collaborative discourse
#roll dice to pick who talks
#roll dice to pick what they talk about

##speakers = no. of speakers
##codes = no. of codes
##steps = no. of lines

sim_rand_discourse = function(speakers,codes,steps,run){
  
  #simulate talk sequence
  speaker_ids = seq.int(1:speakers)
  speaker_seq = sample(x = speaker_ids, size = steps, replace = TRUE)
  
  #simulate code occurrences
  code_vals = sample(x = c(1,0),size = codes*steps,replace = TRUE)
  code_df = data.frame(matrix(data = code_vals,nrow = steps,ncol = codes))
  code_names = LETTERS[1:codes]
  colnames(code_df) = code_names
  
  #combine speakers and codes
  sim_dis = code_df
  sim_dis$Speaker = speaker_seq
  sim_dis$Run = rep(run,nrow(sim_dis))
  
  return(sim_dis)
}
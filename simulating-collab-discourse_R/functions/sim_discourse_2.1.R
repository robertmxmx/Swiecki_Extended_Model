##sim_discourse_2.1
#function for simulating collaborative discourse.
#speaker sequence is based on transition matrix
#talk content is based on code probability matrix for team

##speakers = no. of speakers
##codes = no. of codes
##steps = no. of lines
##trans_mat = speaker transition matrix
##codes_mat = code prob matrix for team

sim_discourse_2.1 = function(codes,steps,trans_mat,codes_mat){
  
  #simulate talk sequence using transition matrix
  mat_ = trans_mat
  speaker_seq_info = sim_speaker_sequence(mat_,steps)
  speaker_seq = speaker_seq_info$sequence
  speaker_counts = speaker_seq_info$counts
  #browser()
  
  #simulated code occurrences using code frequency matrix
  ##generate coded lines for each speaker. no of lines comes from trans mat
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
  
  temp = codes.list
  
  #order the coded lines by looping through the speaker order
  data.list = list()
  for(i in 1:length(speaker_seq)){
    speaker = speaker_seq[i]
    coded.lines = codes.list[[speaker]]
    line.id = sample(x = 1:nrow(coded.lines),size = 1)
    line = coded.lines[line.id,]
    line$Speaker = speaker
    data.list[[i]] = line
    codes.list[[speaker]] = coded.lines[-line.id,]
  }
  coded.df = bind_rows(data.list)
  sim_dis = coded.df
  colnames(sim_dis)[1:codes] = LETTERS[1:codes]
  sim_dis$Run = rep(det(trans_mat),nrow(sim_dis)) #getting unique identifier for the matrix
  sim_dis$Run = as.character(sim_dis$Run)
  return(sim_dis)
}



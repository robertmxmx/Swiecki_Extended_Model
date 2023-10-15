##sim_discourse_2.2
#function for simulating collaborative discourse.
#speaker sequence is based on transition matrix
#talk content is based on code probability matrix for team
#accounts for null codes

##speakers = no. of speakers
##codes = no. of codes
##steps = no. of lines
##trans_mat = speaker transition matrix
##codes_mat = code prob matrix for team

sim_discourse_2.2 = function(codes,trans_mat,codes_mat,type){
  
  #simulate talk sequence using transition matrix
  mat_ = trans_mat$mat
  steps = trans_mat$linetot
  speaker_seq_info = sim_speaker_sequence(mat_,steps,type)
  speaker_seq = speaker_seq_info$sequence
  speaker_counts = speaker_seq_info$counts
  
  #simulated code occurrences using code frequency matrix
  ##generate coded lines for each speaker. no of lines comes from trans mat
  ## account for null codes
  #browser()
  
  ##make null lines first
  codes.list = list()
  for (i in 1:length(speaker_counts)){
    code.freqs = codes_mat[i,]
    
    null.id = which(names(code.freqs)=="null_c")
    null.percent = code.freqs[null.id]
    null.length = round(speaker_counts[[i]]*null.percent)
    null.df = data.frame(matrix(data = 0,
                                nrow = as.numeric(null.length),
                                ncol = length(code.freqs))
                         )
    
    code.df = data.frame(matrix(data = NA,
                                nrow = speaker_counts[[i]] - nrow(null.df),
                                ncol = length(code.freqs))
                         
    )
    
    for(j in 1:length(code.freqs)){
      code.vec = sample(x = c(0,1),
                        size = speaker_counts[[i]] - nrow(null.df),
                        replace = TRUE,
                        prob = c(1-code.freqs[j], code.freqs[j])
      )
      code.df[,j] = code.vec
    }
    code.df[,null.id] = rep(0,nrow(code.df))
    code.df = rbind(code.df,null.df)
    codes.list[[i]] = code.df
  }
  
  names(codes.list) = names(speaker_counts)
  #order the coded lines by looping through the speaker order
  data.list = list()
  for(i in 1:length(speaker_seq)){
    speaker = speaker_seq[i]
    coded.lines = codes.list[[as.character(speaker)]]
    line.id = sample(x = 1:nrow(coded.lines),size = 1)
    line = coded.lines[line.id,]
    line$Speaker = speaker
    data.list[[i]] = line
    codes.list[[as.character(speaker)]] = coded.lines[-line.id,]
  }
  coded.df = bind_rows(data.list)
  sim_dis = coded.df
  colnames(sim_dis)[1:(codes + 1)] = c("null_c",LETTERS[1:codes]) ### ACCOUNT FOR NULL CODE HERE
  sim_dis$Run = rep(det(mat_),nrow(sim_dis)) #getting unique identifier for the matrix
  sim_dis$Run = as.character(sim_dis$Run)
  return(sim_dis)
}



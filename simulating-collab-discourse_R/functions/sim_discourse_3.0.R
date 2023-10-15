#THIS VERSION IS BUSTED BC THE ADJ MATS IT IS BASED ON ARE WRONG
#NEED ADJ MATS WHERE EACH ROW IS DIVIDED BY THE NUMBER OF WINDOWS IN 
#WHICH THAT CODE OCCURS, NOT TOTAL NUMBER OF WINDOWS


##sim_discourse_3.0
#function for simulating collaborative discourse.
#speaker sequence is based on transition matrix
#talk content is based on inter co-occurrence matrix for each individual
#accounts for null codes

##trans_mat = speaker transition matrix
##co_mat = code prob matrix for team

sim_discourse_3.0 = function(trans_mat,
                             adj_mats,
                             type){
  #browser()
  #simulate talk sequence using transition matrix
  mat_ = trans_mat$mat
  steps = trans_mat$linetot
  speaker_seq_info = sim_speaker_sequence(mat_,steps,type)
  speaker_seq = speaker_seq_info$sequence
  speaker_counts = speaker_seq_info$counts
  
  #simulated code occurrences using the connection matrices
  ##generate coded lines for each speaker. no of lines comes from trans mat
  ## account for null codes
  ## goes through line by line...
  code.num = ncol(adj_mats[[1]])
  browser()
  coded.lines.list = list()
  for(i in 1:length(speaker_seq)){
    if (i == 1){ #randomly selecting first code. will update later to select using baserates
      code.pos = sample(code.num,1)
      code.vec = rep(0,code.num)
      code.vec[code.pos] = 1
    }else{
      prev.code = which(coded.lines.list[[i-1]] == 1)
      speaker = speaker_seq[[i]]
      possible.codes = adj_mats[[speaker]][prev.code,]
      nas = is.na(possible.codes)
      possible.codes[nas] = 0
      #check if all are zero 
      if(any(possible.codes > 0)){
        if(sum(possible.codes > 0) == 1){ #prob of the single code and null sum to 1
          there = which(possible.codes > 0)
          possible.codes[code.num] = 1-there
          code.pos = sample(code.num,1,prob = possible.codes,replace = TRUE)
          code.vec = rep(0,code.num)
          code.vec[code.pos] = 1
        }else{
          code.pos = sample(code.num,1,prob = possible.codes,replace = TRUE)
          code.vec = rep(0,code.num)
          code.vec[code.pos] = 1
        }
      }else{## if all are zero pick null code
        code.vec = rep(0,code.num)
        code.vec[code.num] = 1
      }
    }
    coded.lines.list[[i]] = code.vec
  }
  coded.lines = do.call("rbind",coded.lines.list)
  #naming and cleaning
  colnames(coded.lines) = c(LETTERS[1:(code.num - 1)],"null_c")
  coded.lines = data.frame(coded.lines)
  coded.lines$Run = rep(det(mat_),nrow(coded.lines)) #getting unique identifier for the matrix
  coded.lines$Run = as.character(coded.lines$Run)
  coded.lines$Speaker = speaker_seq
  return(coded.lines)
}




##sim_discourse_3.1
#function for simulating collaborative discourse.
#speaker sequence is based on transition matrix
#talk content is based on inter co-occurrence matrix for each individual
#where each row is divided by the number of windows in which that code occurs
#accounts for null codes

##trans_mat = speaker transition matrix
##co_mat = code prob matrix for team

sim_discourse_3.1 = function(trans_mat,
                             adj_mats,
                             type,
                             steps = steps,
                             version,
                             iterative_mean){
  
  #simulate talk sequence using transition matrix
  if(type == "real"){
    mat_ = trans_mat$mat
    steps = trans_mat$linetot
  }else if(type == "sim"){
    mat_= trans_mat
    steps = steps
  }else{
    mat_= trans_mat$mat
    steps = steps
  }
  speaker_seq_info = sim_speaker_sequence(mat_,steps,type)
  speaker_seq = speaker_seq_info$sequence
  speaker_counts = speaker_seq_info$counts
  

  
  #simulated code occurrences using the connection matrices
  ##generate coded lines for each speaker. no of lines comes from trans mat
  ## account for null codes
  ## goes through line by line...
  code.num = ncol(adj_mats[[1]])
  coded.lines.list = list()
  
  
  if(version == "original"){

    for(i in 1:length(speaker_seq)){
      speaker = speaker_seq[[i]]
      if (i == 1){ #randomly selecting first code from among possible windows with null code
        prev.code = sample(c(1:code.num),1)
        possible.codes = adj_mats[[speaker]][prev.code,]
        if(sum(possible.codes) == 0){## if all are zero pick null code
          code.vec = rep(0,code.num)
          code.vec[code.num] = 1
        }else{
          code.pos = sample(c(1:code.num),1,prob = possible.codes,replace = TRUE)
          code.vec = rep(0,code.num)
          code.vec[code.pos] = 1 
        }
      }else{
        prev.code = which(coded.lines.list[[i-1]] == 1)
        possible.codes = adj_mats[[speaker]][prev.code,]
        #check if all are zero 
        if(sum(possible.codes) == 0){## if all are zero pick null code #if this is fucked check here
          code.vec = rep(0,code.num)
          code.vec[code.num] = 1
        }else{
          code.pos = sample(c(1:code.num),1,prob = possible.codes,replace = TRUE)
          code.vec = rep(0,code.num)
          code.vec[code.pos] = 1
        }
      }
      coded.lines.list[[i]] = code.vec
    }

  }else if (version == "iterative"){
    
    generate_random_number <- function(mean) {
      sd <- 1
      random_number <- rnorm(1, mean+sd, sd)
      random_number <- ifelse(random_number < 1, 1, random_number)
      random_number <- ifelse(random_number > 9, 9, random_number) 
      random_number <- floor(random_number)
      return(random_number)
    }
    
    
    
    for(i in 1:length(speaker_seq)){
      speaker = speaker_seq[[i]]
  
      if (i == 1){ #randomly selecting first code from among possible windows with null code
        prev.code = sample(c(1:code.num),1)
        possible.codes = adj_mats[[speaker]][prev.code,]
        
      }else{
        prev.code = sample(c(which(coded.lines.list[[i-1]] == 1)),1)
        possible.codes = adj_mats[[speaker]][prev.code,]
      }
        
      if(sum(possible.codes) == 0){## if all are zero pick null code #if this is fucked check here
        code.vec = rep(0,code.num)
        code.vec[code.num] = 1
      }else{
        code.pos = sample(c(1:code.num),generate_random_number(iterative_mean),prob = possible.codes,replace = TRUE)
        #print(code.pos)
        code.vec = rep(0,code.num)
        code.vec[code.pos] = 1
      }
      coded.lines.list[[i]] = code.vec
      
    }
    
  }
  
  coded.lines = do.call("rbind",coded.lines.list)
  #naming and cleaning
  colnames(coded.lines) = c(LETTERS[1:(code.num - 1)],"null_c")
  coded.lines = data.frame(coded.lines)
  coded.lines$Run = rep(det(mat_),nrow(coded.lines)) #getting unique identifier for the matrix
  coded.lines$Run = as.character(coded.lines$Run)
  coded.lines$Speaker = speaker_seq
  
  #current_time <- format(Sys.time(), "%Y%m%d_%H%M%S_%OS")
  #filename <- paste("coded_lines_", current_time, ".csv", sep = "")
  #file_path <- "C:/Users/Desktop/Data/"
  #full_path <- paste(file_path, filename, sep = "")
  #write.csv(coded.lines, full_path, row.names = FALSE)
  
  return(coded.lines)
}



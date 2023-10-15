#function for simulating speaker sequence based on transition matrix

##matrix = speaker transition matrix for the team
##steps = no. of desired lines for this run of the sim


sim_speaker_sequence = function(matrix,steps, type = c("real","sim")){
  #set up initials
  seq_list = list()
  #make markovchain object that is needed to generate sequence
  speaker_ids =  as.character(seq.int(ncol(matrix)))
  rownames(matrix) = speaker_ids
  
  trans_mat = new("markovchain", 
                  states = speaker_ids,
                  transitionMatrix = matrix)
  
  #generate speaker sequence based on matrix
  seq_ = markovchainSequence(n = steps,
                             markovchain = trans_mat
  )
  seqs_ = as.numeric(seq_)

  #count number of lines for each speaker
  counts = as.list(table(seqs_))
  
  return(list(sequence = seqs_, counts = counts))
}

###############################################################################
sim_speaker_sequence_dep = function(matrix,steps, type = c("real","sim")){
  #set up initials
  seq_list = list()
  #make markovchain object that is needed to generate sequence

  speaker_ids =  as.character(seq.int(ncol(matrix)))
  rownames(matrix) = speaker_ids

  trans_mat = new("markovchain", 
                  states = speaker_ids,
                  transitionMatrix = matrix)
  
  #generate speaker sequence based on matrix
  #running the sequence generation n times, where n is the number of speakers.
  #doing this to ensure that each speakers is represented at least once.
  #randomly choose the speaker (without replacement) from the list of possible speakers
  seqs_list = list()
  chain_order = sample(c(1:length(speaker_ids)),length(speaker_ids),FALSE)
  for(i in chain_order){
    seq_ = markovchainSequence(n = (steps/nrow(matrix)) - 1, #this will become problematic if steps are not divisible by number of speakers. -1 is bc including the initial state adds one to n
                               markovchain = trans_mat,
                               t0 = speaker_ids[i], 
                               include.t0 = TRUE
                               )
    seqs_list[[i]] = as.numeric(seq_)
  }

  #combine sequences
  seqs_ = unlist(seqs_list)
  #count number of lines for each speaker
  counts = as.list(table(seqs_))
  
  return(list(sequence = seqs_, counts = counts,lists = seqs_list))
}



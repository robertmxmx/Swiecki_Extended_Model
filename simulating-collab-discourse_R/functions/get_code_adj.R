#script for returning the code probability adj matrices for each team

get_code_adj = function(dat_,
                        units,
                        conversations,
                        codes,
                        metadata,
                        window.size){
  
  ###accumulate with null code
  codes2 = data.frame(rs.fg[,c(codenames,"null_c")])
  
  accum.inter.null = 
    ena.accumulate.data(
      units = units,
      conversation = conversation,
      codes = codes2,
      metadata = meta,
      window.size.back = 2)
  
  ###get connections in adjacency form
  co_mats = connection.matrix(accum.inter.null)
  
  ###make symmetric
  co_mats = map(co_mats,update.net)
  
  ###get self-references for adj matrices
  new.row.connection.counts = cbind(accum.inter.null$model$row.connection.counts,cons)
  d = new.row.connection.counts %>% select(UserName,GroupName,all_of(con.names))
  self_cons = d %>% group_by(UserName,GroupName) %>% summarise(across(contains("&"),sum),.groups = "keep")
  
  ###update adj mat diagonal with self references
  co_mats = co_mats[sort(names(co_mats))]
  
  update_diag = function(diags,mats){
    diags = ungroup(diags)
    diags = diags %>% select(contains("&"))
    for(i in 1:length(mats)){
      diag(mats[[i]]) = as.numeric(diags[i,])
    }
    return(mats)
  }
  
  co_mats = update_diag(self_cons,co_mats)
  
  ###convert to probabilities
  speakers = accum.inter$meta.data$ENA_UNIT
  speakers = sort(speakers)
  codenames2 = c(codenames,"null_c")
  
  ####get window counts for each code for each person
  speaker.list = list()
  for (i in 1:length(speakers)){
    unit = speakers[[i]]
    window.count.list = list()
    for (j in 1:length(codenames2)){
      code = codenames2[[j]]
      window.count = window_counts_code(speaker = unit,
                                        code = code,
                                        row.connection.counts = new.row.connection.counts)
      window.count.list[[j]] = window.count
    } 
    names(window.count.list) = codenames2
    speaker.list[[i]] = unlist(window.count.list)
  }
  names(speaker.list) = speakers
  
  ####divide co_mats by line counts
  co_mats = map2(.x = co_mats,.y = speaker.list,.f = function(x,y) x/y)
  
  ####remove nans introduced by zero division
  remove.nans = function(x){
    nans = which(is.nan(x))
    infs = which(is.infinite(x))
    x[nans] = 0
    x[infs] = 0
    return(x)
  }
  co_mats = map(.x = co_mats,.f = remove.nans )
  
  ###restructure data to be in list for each team
  team.names = unique(accum.inter$meta.data$GroupName)
  co_mats_team = list()
  for(i in 1:length(team.names)){
    ids = str_which(names(co_mats),team.names[i])
    mats = co_mats[ids]
    co_mats_team[[i]] = mats
  }
  names(co_mats_team) = team.names
  
  ###sort by unit names within team
  for(i in 1:length(co_mats_team)){
    co_mats_team[[i]] = co_mats_team[[i]][order(names(co_mats_team[[i]]))]
  }
  
}
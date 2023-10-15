#function for getting the average similarity of a given unit to the other 
#units on their team
cog.sym = function(groupData,unitCols,teamCol){
  #browser()
  speakers = unique(groupData[,unitCols])
  code.vecs = groupData %>%
    group_by(!!!syms(unitCols)) %>% 
    summarise(across(.cols = everything(),.fns = sum)) #there is an error here to fix
  
  #browser()
  
  # norm.code.vecs = code.vecs %>% 
  #   dplyr::select(where(is.numeric)) %>% 
  #   as.matrix %>% 
  #   rENA::fun_sphere_norm()
  
  code.vecs = code.vecs[,-(c(1))] # NEED TO GENERALIZE
  code.vecs = as.matrix(code.vecs)
  norm.code.vecs = rENA::fun_sphere_norm(code.vecs)
  
  ##just need to swap to distance because of zero vectors
  sim.list = list()
  for (i in 1:length(speakers)){
    test.vec = norm.code.vecs[i,]
    other.vecs = norm.code.vecs[-i,]
    sim = get.euc.dist_single(x1 = other.vecs,x2 = test.vec)
    #browser()
    sim = unlist(sim)
    avg.sim = mean(sim)
    sim.list[[i]] = avg.sim
  }
  team = unique(groupData[,teamCol])
  #browser()
  simdf = as.data.frame(cbind(unlist(sim.list),speakers,rep(team,length(speakers))))
  colnames(simdf)[1] = "cog.sim" #NEED TO GENERALIZE
  simdf$cog.sim = as.numeric(simdf$cog.sim)
  return(simdf)
}

#specific to RS data
cog.sym_dep2 = function(groupData,unitCols,teamCol){
  #browser()
  speakers = unique(groupData[,unitCols])
  code.vecs = groupData %>%
    group_by(!!!syms(unitCols)) %>% 
    summarise(across(.cols = contains("_c"),.fns = mean))

  code.vecs = code.vecs[,-(c(1,2))]
  
  sim.list = list()
  for (i in 1:nrow(speakers)){
    test.vec = code.vecs[i,]
    other.vecs = code.vecs[-i,]
    sim = get.euc.dist_single(x1 = other.vecs,x2 = test.vec)
    #browser()
    sim = unlist(sim)
    avg.sim = mean(sim)
    sim.list[[i]] = avg.sim
  }
  team = unique(groupData[,teamCol])
  #browser()
  simdf = as.data.frame(cbind(unlist(sim.list),speakers,rep(team,nrow(speakers))))
  colnames(simdf) = c("cog.sim","UserName","GameHalf","Group") #NEED TO GENERALIZE
  simdf$cog.sim = as.numeric(simdf$cog.sim)
  return(simdf)
}



#specific to RS data
cog.sym_dep = function(groupData,unitCols,teamCol){
  #browser()
  speakers = unique(groupData[,unitCols])
  code.vecs = groupData %>%
    group_by(!!!syms(unitCols)) %>% 
    summarise(across(.cols = contains("_c"),.fns = sum))
  
  #browser()
  
  # norm.code.vecs = code.vecs %>% 
  #   dplyr::select(where(is.numeric)) %>% 
  #   as.matrix %>% 
  #   rENA::fun_sphere_norm()
  
  code.vecs = code.vecs[,-(c(1,2))]
  code.vecs = as.matrix(code.vecs)
  norm.code.vecs = rENA::fun_sphere_norm(code.vecs)
  
  ##just need to swap to distance because of zero vectors
  sim.list = list()
  for (i in 1:nrow(speakers)){
    test.vec = norm.code.vecs[i,]
    other.vecs = norm.code.vecs[-i,]
    sim = get.euc.dist_single(x1 = other.vecs,x2 = test.vec)
    #browser()
    sim = unlist(sim)
    avg.sim = mean(sim)
    sim.list[[i]] = avg.sim
  }
  team = unique(groupData[,teamCol])
  #browser()
  simdf = as.data.frame(cbind(unlist(sim.list),speakers,rep(team,nrow(speakers))))
  colnames(simdf) = c("cog.sim","UserName","GameHalf","Group") #NEED TO GENERALIZE
  simdf$cog.sim = as.numeric(simdf$cog.sim)
  return(simdf)
}


#function for returning the standard deviation of the code frequency vectors of a given team

cog.sd = function(groupData,unitCols){
  
  speakers = unique(groupData[,unitCols])
  code.vecs = groupData %>%
    group_by(!!!syms(unitCols)) %>% 
    summarise(across(.cols = contains("_c"),.fns = sum))
  
  #should I be norming here?
  code.vecs = code.vecs[,-(c(1,2))]
  code.vecs = as.matrix(code.vecs)
  norm.code.vecs = rENA::fun_sphere_norm(code.vecs)
  

  
  centroid = colMeans(norm.code.vecs) #mean of the cloud
  dists = get.euc.dist_single(x1 = norm.code.vecs,x2 = centroid) #get distances
  sdev = sd(unlist(dists))
  #browser()
  return(sdev)
}
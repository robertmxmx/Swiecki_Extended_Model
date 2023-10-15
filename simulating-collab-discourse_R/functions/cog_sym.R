#function for calculating cognitive symmetry metrics for code frequency matrices

##version for calculating cog sim based on probability adj matrix
#compares the whole matrix

#array based version for simulation
cog_sym_adj_array = function(team.adj){
  #browser()
  mat_dist  = function(testMat,otherMats){
    dist.list = list()
    for (i in 1:dim(otherMats)[3]){
      x = as.numeric(testMat)
      y = as.numeric(otherMats[,,i])
      eucdist = dist(rbind(x,y))
      dist.list[[i]] = eucdist
    }
    return(dist.list)
  }
  
  avg.dist.list = list()
  for(i in 1:dim(team.adj)[3]){
    #browser()
    test.mat = team.adj[,,i]
    others = team.adj[,,-i]
    eucdists = mat_dist(testMat = test.mat,otherMats = others)
    avgdist = mean(unlist(eucdists))
    avg.dist.list[[i]] = avgdist
  }
  avgdists = as.numeric(unlist(avg.dist.list))
  unit.names = names(team.adj)
  res = data.frame(cbind(unit.names,avgdists))
  return(res)
}


cog_sym_adj = function(team.adj){
  #browser()
  mat_dist  = function(testMat,otherMats){
    dist.list = list()
    for (i in 1:length(otherMats)){
      x = as.numeric(testMat)
      #browser()
      y = as.numeric(otherMats[[i]])
      eucdist = dist(rbind(x,y))
      dist.list[[i]] = eucdist
    }
    return(dist.list)
  }
  
  avg.dist.list = list()
  for(i in 1:length(team.adj)){
    #browser()
    test.mat = team.adj[[i]]
    others = team.adj[-i]
    eucdists = mat_dist(testMat = test.mat,otherMats = others)
    avgdist = mean(unlist(eucdists))
    avg.dist.list[[i]] = avgdist
  }
  avgdists = as.numeric(unlist(avg.dist.list))
  avgdists = round(avgdists,digits = 5)
  #unit.names = names(team.adj)
  #res = data.frame(cbind(unit.names,avgdists))
  res = data.frame(avgdists)
  return(res)
}

cog_sym_adj_real = function(team.adj){
  #browser()
  mat_dist  = function(testMat,otherMats){
    dist.list = list()
    for (i in 1:length(otherMats)){
      x = as.numeric(testMat)
      #browser()
      y = as.numeric(otherMats[[i]])
      eucdist = dist(rbind(x,y))
      dist.list[[i]] = eucdist
    }
    return(dist.list)
  }
  
  avg.dist.list = list()
  for(i in 1:length(team.adj)){
    #browser()
    test.mat = team.adj[[i]]
    others = team.adj[-i]
    eucdists = mat_dist(testMat = test.mat,otherMats = others)
    avgdist = mean(unlist(eucdists))
    avg.dist.list[[i]] = avgdist
  }
  avgdists = as.numeric(unlist(avg.dist.list))
  unit.names = names(team.adj)
  res = data.frame(cbind(unit.names,avgdists))
  return(res)
}

##version for calculating cog sim based on adj matrix
#calculates average distance between upper tris. Will need to update
#when diagonal is in place

cog_sym_adj_dep = function(team.adj){
  #browser()
  get.upper = function(x){
    ids = upper.tri(x)
    upper = x[ids]
  }
  tri.dist = function(testvec,othervecs){
    dist.list = list()
    for(i in 1:length(othervecs)){
      eucdist = dist(rbind(testvec,othervecs[[i]]))
      dist.list[[i]] = eucdist
    }
    return(dist.list)
  }
  
  upper.tris = map(team.adj,get.upper)
  avg.dist.list = list()
  for(i in 1:length(upper.tris)){
    test.vec = upper.tris[[i]]
    others = upper.tris[-i]
    eucdists = tri.dist(testvec = test.vec,othervecs = others)
    avgdist = mean(unlist(eucdists))
    avg.dist.list[[i]] = avgdist
  }
  avgdists = as.numeric(unlist(avg.dist.list))
  unit.names = names(team.adj)
  res = data.frame(cbind(unit.names,avgdists))
  return(res)
}

cog_sym_ind = function(code_mat){#for each row, calculated average distance to other rows
  #removing null code
  null.id = which(colnames(code_mat) == "null_c")
  code_mat = code_mat[,-null.id]
  sim.list = list()
  for (i in 1:nrow(code_mat)){
    test.vec = code_mat[i,]
    other.vecs = code_mat[-i,]
    sim = get.euc.dist_single(x1 = other.vecs,x2 = test.vec)
    #browser()
    sim = unlist(sim)
    avg.sim = mean(sim)
    sim.list[[i]] = avg.sim
  }
  sims = unlist(sim.list)
  speaker = seq.int(1:nrow(code_mat))
  res = data.frame(cbind(speaker,sims))
  return(res)
}
  
cog_sym_group = function(code_mat){#returns the stdev of the vecs from centroid
  centroid = colMeans(code_mat) #mean of the cloud
  dists = get.euc.dist_single(x1 = code_mat,x2 = centroid) #get distances
  sdev = sd(unlist(dists))
  return(sdev)
}



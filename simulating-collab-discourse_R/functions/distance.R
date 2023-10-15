#function for calculating distances between vectors


get.euc.dist = function(x1,x2){
  euc.dist = sapply(seq.int(dim(x1)[1]), function(i) as.numeric(dist(rbind(x1[i,],x2[i,]))))
}

get.euc.dist_single = function(x1,x2){
  euc.dist = sapply(seq.int(dim(x1)[1]), function(i) as.numeric(dist(rbind(x1[i,],x2))))
}

get.mah.dist = function(x1,x2,covmats){
  #browser()
  mah.dist = sapply(seq.int(dim(x1)[1]), function(i) mahalanobis(x = as.numeric(x1[i,]),center = as.numeric(x2[i,]),cov = covmats[[i]]))
}



#function for generating unit adjacency matrices for using in data simulation
##for now, sampling co-occur rates from a real distribution
##rowsums, must equal 1
##all vals non-negative
## for each cell, samples from real distribution

gen_adj_matrix = function(ncodes, distribution){
  sample_dist = function(x){
    val = sample(x = x,size = 1)
    return(val)
  }
  vec = apply(X = distribution,MARGIN = 2,FUN = sample_dist)
  mat_ = matrix(vec,ncodes,ncodes)
  
  ##make sure ros/cols sum to 1
  update_row = function(x){
    if(sum(x) == 0 | sum(x) == 1){
      x = x
    }else{
      x = x/sum(x)
    }
    return(x)
  }
  
  mat_ = apply(mat_, 1, update_row)
  mat_ = t(mat_)
  
  return(mat_)
}

gen_adj_matrix_team = function(ncodes,
                               nspeakers,
                               distribution,
                               noise){
  mat_list = list()
  init_mat = gen_adj_matrix(ncodes,distribution)
  mat_list[[1]] = init_mat
  for(i in 2:nspeakers){
    mat_ = jitter(x = init_mat,factor = noise,amount = NULL)
    mat_ = abs(mat_)
    mat_ = apply(mat_, 1, function(i) i/sum(i))
    mat_ = t(mat_)
    nans = is.nan(mat_)
    mat_[nans] = 0
    mat_list[[i]] = mat_
  }
  return(mat_list)
}

###############################################################################
gen_adj_matrix_dep = function(ncodes,
                          distribution){
  
  ###update to jitter from the initial matrix, not make a new matrix each time
  
  tot = ncodes*ncodes
  vals = sample(x = distribution,size = tot,replace = TRUE)
  mat_ = matrix(vals,nrow =  ncodes,ncol = ncodes)
  mat_ = apply(mat_, 1, function(i) i/sum(i))
  mat_ = t(mat_)
  nans = is.nan(mat_)
  mat_[nans] = 0

  rsums = rowSums(mat_)
  if (suppressWarnings(any(rsums)>1)){
    stop("Invalid matrix. rowSums > 1")
  }
  return(mat_)
}


###############################################################################
#without jitter

gen_adj_matrix_team_nj = function(ncodes,
                                  nspeakers,
                                  distribution){
  
  team_adj_mats = replicate(n = nspeakers,expr = gen_adj_matrix(ncodes,distribution),simplify = FALSE)
  return(team_adj_mats)
}
###############################################################################
  
  
    



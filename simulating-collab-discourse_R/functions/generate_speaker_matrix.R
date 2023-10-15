#function for generating a speaker transition matrix for use in data simulation

##draw diag from uniform distribution, then create matrix
gen_speaker_matrix = function(speakers){
  t_mat = matrix(data = rep(0,speakers*2),speakers, speakers)
  diag_vals = runif(n = speakers, min = 0, max = 0.95)
  for(i in 1:length(diag_vals)){
    t_mat = update_trans_matrix(t_matrix = t_mat,speaker = i,val = diag_vals[i])
  }
  return(t_mat)
}

##set diagonal, then generate transition matrix
gen_speaker_matrix_diag = function(diag_vals){
  t_mat = gen_speaker_matrix(length(diag_vals*2),.3)
  og = t_mat
  for(i in 1:length(diag_vals)){
    t_mat = update_trans_matrix(t_matrix = t_mat,speaker = i,val = diag_vals[i])
  }
  #return(list(og = og,t_mat = t_mat))
  return(t_mat)
}

##using normal dis
gen_speaker_matrix_norm = function(n,sd){
  vals = rnorm(n = n^2,mean = runif(1,min = 0,max = 1),sd = sd) #may want to experiment with other parameters or distributions
  #print(mean(vals))
  mat = matrix(vals,nrow = n,ncol = n)
  mat = abs(mat)
  mat = apply(mat, 1, function(i) i/sum(i))
  mat = t(mat)
  return(mat)
}

##using gamma dis
gen_speaker_gamma = function(n, shape, scale){
  vals = rgamma(n = n^2, shape = runif(1,min = 2, max = 8),scale = scale) 
  mat = matrix(vals,nrow = n,ncol = n)
  mat = apply(mat, 1, function(i) i/sum(i))
  mat = t(mat)
  return(mat)
}

##using uni dis
gen_speaker_uni = function(n){
  vals = runif(n = n^2, min = 0,max = 1) 
  mat = matrix(vals,nrow = n,ncol = n)
  mat = apply(mat, 1, function(i) i/sum(i))
  mat = t(mat)
  return(mat)
}
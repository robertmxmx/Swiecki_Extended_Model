# function for calculating individual and team interactivity metrics

interactivity = function(mat_){
  ind = diag(mat_)
  ind = 1 - ind  #re-map for interpretation
  base_mat = diag(x = 1,nrow(mat_),nrow(mat_)) #n,n
  group = norm(x = base_mat-mat_,type = "F")
  speakers = seq.int(1:nrow(mat_))
  res = data.frame(cbind(speakers,ind))
  colnames(res) = c("Speaker","interactivity.ind")
  res$interactivity.group = rep(group,nrow(res))
  #res$run = rep(run,nrow(res))
  return(res)
}


team_interactivity = function(t_matrix){
  speakers = nrow(t_matrix)
  base_mat = diag(x = 1,speakers,speakers) #n,n
  group = norm(x = base_mat-t_matrix,type = "F")
  return(group)
}
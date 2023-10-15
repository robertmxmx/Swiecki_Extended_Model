###boot dist
boot_dist = function(data,indices){
  #sample.ids = sample(seq.int(1:length(distribution)),size = 52,replace = TRUE)
  #distribution = distribution[sample.ids]
  data = distribution[indices]
  statistic = mean(data)
  return(statistic)
}

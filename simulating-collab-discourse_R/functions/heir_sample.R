# hierarchical resampling

hier_sample = function(df,
                       groupCols){
  
  split_dat = split(df,df[,groupCols])
  samp = sample(x = c(1:length(split_dat)),size = length(split_dat),replace = TRUE)
  
  dat_list = list()
  for(i in 1:length(samp)){
    dat_list[[i]] = split_dat[[samp[i]]]
  }
  new_dat = bind_rows(dat_list)
  return(new_dat)
}
#function for returning inter mods from sim data (as opposed to distances)

sim_mods = function(codes,
                    trans_mat,
                    adj_mats,
                    window_size_inter,
                    type,
                    normalize,
                    steps){
  
  ##simulate data
  dat_ = sim_discourse_3.1(trans_mat = trans_mat,
                           adj_mats = adj_mats,
                           type = type,
                           steps = steps)
  
  
  #browser()
  
  ##model set up
  code_ids = LETTERS[1:codes]
  units = "Speaker"
  convo = "Run"
  
  ##make inter model
  inter.accum = ena.accumulate.data(units = data.frame(dat_[,units]),
                                    conversation = data.frame(dat_[,convo]),
                                    codes = data.frame(dat_[,code_ids]),
                                    window.size.back = window_size_inter)
  
  ##make ind model
  ind.accum = ena.accumulate.data(units = data.frame(dat_[,units]),
                                  conversation = data.frame(dat_[,convo]),
                                  codes = data.frame(dat_[,code_ids]),
                                  window.size.back = 1) 
  
  #browser()
  
  if (normalize == TRUE){
    inter.lws = inter.accum$connection.counts %>%
      arrange(ENA_UNIT) %>% 
      as.matrix() %>% 
      rENA::fun_sphere_norm()
    
    ind.lws = ind.accum$connection.counts %>%
      arrange(ENA_UNIT) %>% 
      as.matrix() %>% 
      rENA::fun_sphere_norm()
  }else{
    inter.lws = inter.accum$connection.counts %>%
      arrange(ENA_UNIT) %>% 
      as.matrix()
    
    ind.lws = ind.accum$connection.counts %>%
      arrange(ENA_UNIT) %>% 
      as.matrix()
  }
  correct.meta = inter.accum$meta.data %>% arrange(ENA_UNIT)
  inter.lws = cbind(correct.meta,inter.lws)
  
  write.csv(dat_, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Test3_Raw.csv", row.names=FALSE)
  return(inter.lws)
 #  return(dat_) look later 
}
#script for generating a distribution of interdependence impact measures 
#based on simulated data


##########################################

sim_dists = function(codes,
                     trans_mat,
                     adj_mats,
                     window_size_inter,
                     type,
                     normalize){
  
  ##simulate data
  dat_ = sim_discourse_3.1(trans_mat = trans_mat,
                           adj_mats = adj_mats,
                           type = type)

  
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
  
  ##calculate impact
  dists = get.euc.dist(ind.lws,inter.lws)
  
  #browser()
  order.meta = inter.accum$meta.data %>% arrange(ENA_UNIT)
  names(dists) = order.meta$ENA_UNIT
  
  #calculate independent variables
  ##density

  
  
  ##soc_sym
  # soc_syms = soc.sym.ind(groupData = dat_,
  #                        speakerCol = "Speaker",
  #                        teamCol = "Run")
  ##cog_sym
  # cog_syms = cog.sym(groupData = dat_,
  #                    unitCols = "Speaker",
  #                    teamCol = "Run")
  
  
  #return(list(distances = dists, cog = cog_syms$cog.sim, soc = soc_syms$soc.ind,runs = rep(run,length(dists))))
  #return(list(distances = dists, soc = soc_syms$soc.ind))
  return(dists)
  
}
###############################################################################
sim_dists_soc = function(speakers,
                     codes,
                     steps = 500,
                     trans_mat,
                     window_size_inter){

##simulate data
dat_ = sim_discourse_1.0(codes = codes,
                         trans_mat = trans_mat,
                          steps = steps)

##model set up
code_ids = LETTERS[1:codes]
units = "Speaker"
convo = "Run"

#browser()
##make inter model
inter.accum = ena.accumulate.data(units = data.frame(dat_[,units]),
                      conversation = data.frame(dat_[,convo]),
                      codes = data.frame(dat_[,code_ids]),
                      window.size.back = window_size_inter)

  inter.lws = inter.accum$connection.counts %>%
  as.matrix() %>% 
  rENA::fun_sphere_norm()

##make ind model
ind.accum = ena.accumulate.data(units = data.frame(dat_[,units]),
                                conversation = data.frame(dat_[,convo]),
                                codes = data.frame(dat_[,code_ids]),
                                window.size.back = 1) 
  
ind.lws = ind.accum$connection.counts %>%
  as.matrix() %>% 
  rENA::fun_sphere_norm()

##calculate impact
dists = get.euc.dist(ind.lws,inter.lws)

#calculate independent variables
##soc_sym
# soc_syms = soc.sym.ind(groupData = dat_,
#                        speakerCol = "Speaker",
#                        teamCol = "Run")
##cog_sym
# cog_syms = cog.sym(groupData = dat_,
#                    unitCols = "Speaker",
#                    teamCol = "Run")


#return(list(distances = dists, cog = cog_syms$cog.sim, soc = soc_syms$soc.ind,runs = rep(run,length(dists))))
#return(list(distances = dists, soc = soc_syms$soc.ind))
return(dists)

}

###############################################################################


sim_dists_rand = function(speakers,
                          codes,
                          run,
                          steps = 500,
                          window_size_inter){
  
  ##simulate data
  dat_ = sim_rand_discourse(speakers = speakers,
                            codes = codes,
                            run = run,
                            steps = steps)
  
  ##model set up
  code_ids = LETTERS[1:8]
  units = "Speaker"
  convo = "Run"
  
  ##make inter model
  inter.set = ena.accumulate.data(units = data.frame(dat_[,units]),
                                  conversation = data.frame(dat_[,convo]),
                                  codes = data.frame(dat_[,code_ids]),
                                  window.size.back = window_size_inter) %>% 
    ena.make.set()
  
  ##make ind model
  ind.set = ena.accumulate.data(units = data.frame(dat_[,units]),
                                conversation = data.frame(dat_[,convo]),
                                codes = data.frame(dat_[,code_ids]),
                                window.size.back = 1) %>% 
    ena.make.set()
  
  ##calculate impact
  ind.lws = as.matrix(ind.set$line.weights)
  inter.lws = as.matrix(inter.set$line.weights)
  dists = get.euc.dist(ind.lws,inter.lws)
  
  #calculate independent variables
  ##soc_sym
  soc_syms = soc.sym.ind(groupData = dat_,
                         speakerCol = "Speaker",
                         teamCol = "Run")
  ##cog_sym
  cog_syms = cog.sym(groupData = dat_,
                     unitCols = "Speaker",
                     teamCol = "Run")
  
  
  return(list(distances = dists, cog = cog_syms$cog.sim, soc = soc_syms$soc.ind,runs = rep(run,length(dists))))
  #return(list(distancers = dists, soc = soc_syms$soc.ind))
  
}



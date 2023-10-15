#function for returning inter mods from sim data (projected in to real space)
#also calculates soc/cog sym on simulated data



sim_mods_proj_1.1 = function(codes,
                    trans_mat,
                    adj_mats,
                    window_size_inter,
                    proj_set,
                    type,
                    steps){
  #browser()
  
  ##simulate data
  dat_ = sim_discourse_3.1(trans_mat = trans_mat,
                           adj_mats = adj_mats,
                           type = type,
                           steps = steps)
  
  ##model set up
  code_ids = LETTERS[1:(codes - 1)]
  units = c("Speaker","Run")
  convo = "Run"
  
  ##make inter model
  ###accumulate
  inter.accum = ena.accumulate.data(units = data.frame(dat_[,units]),
                                    conversation = data.frame(dat_[,convo]),
                                    codes = data.frame(dat_[,code_ids]),
                                    window.size.back = window_size_inter)
  ###make set and project
  inter.set = ena.make.set(enadata = inter.accum,rotation.set = proj_set)
  
  
  ###get points
  sim_points = inter.set$points
  sim_points = sim_points %>% arrange(ENA_UNIT)
  sim_points = sim_points %>% select(ENA_UNIT,Speaker,Run,MR1)
  sim_points$Speaker = as.character(sim_points$Speaker)
  
  #calculate social symmetry on generating matrix
  soc.sym.ind = interactivity(trans_mat)
  names(soc.sym.ind) = c("Speaker","soc.sym.ind","soc.sym.team")
  soc.sym.ind = soc.sym.ind[,c(1,2)]
  soc.sym.ind$Speaker = as.character(soc.sym.ind$Speaker)
  sim_points = left_join(sim_points,soc.sym.ind,"Speaker")
  
  #calculate cognitive symmetry  on generating matrices
  cog.sym.ind = cog_sym_adj(adj_mats)
  cog.sym.ind$Speaker = as.character(seq.int(1:nrow(cog.sym.ind)))
  names(cog.sym.ind) = c("cog.sym.ind","Speaker")
  sim_points = left_join(sim_points,cog.sym.ind, by = "Speaker")
  
  #calculate level two metrics
  sim_points = sim_points %>%
    mutate(soc.mean = mean(soc.sym.ind),cog.mean = mean(cog.sym.ind))
  
  return(sim_points)
}

#validation test (using empirical distance tests)
rm(list = ls())

#libraries
library(rENA)
library(magrittr)
library(markovchain)
library(tidyverse)
library(ICC)
library(lmerTest)
library(interactions)
library(matlab)
library(truncnorm)
library(broom)
library(broom.mixed)
library(coxed)

#functions
source('~/Rprojects/simulating-collab-discourse/functions/sim_discourse_3.1.R')
source('~/Rprojects/simulating-collab-discourse/functions/generate_speaker_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/interactivity.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_speaker_sequence.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/soc.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')
source('~/Rprojects/simulating-collab-discourse/functions/update_trans_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_dists.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_boot_3.0.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_code_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog_sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/matrices_from_observed.R')
source('~/Rprojects/simulating-collab-discourse/functions/empirical_distance_test.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove.silent.R')
source('~/Rprojects/simulating-collab-discourse/functions/empirical_paired.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove_missing_units.R')
#source('~/Rprojects/simulating-collab-discourse/functions/sim_mods.R')
source('~/Rprojects/simulating-collab-discourse/functions/ena.compare.models.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')

#load data
source('~/Rprojects/simulating-collab-discourse/analysis/real_data_results_final.R')

#split data by team
team.split = split(rs.fg,rs.fg$GroupName)

##get cognitive and social matrices from real data
###social 
trans_mats = lapply(team.split,
                    get_trans_mat,
                    speakerCol = "UserName",
                    teamCol = "GroupName")

###cognitive
co_mats_team = co_mats_team[sort(names(co_mats_team))]

###########
sim_discourse_3.1 = function(trans_mat,
                             adj_mats,
                             type,
                             steps = steps){
  
  #simulate talk sequence using transition matrix
  if(type == "real"){
    mat_ = trans_mat$mat
    steps = trans_mat$linetot
  }else if(type == "sim"){
    mat_= trans_mat
    steps = steps
  }else{
    mat_= trans_mat$mat
    steps = steps
  }
  speaker_seq_info = sim_speaker_sequence(mat_,steps,type)
  speaker_seq = speaker_seq_info$sequence
  speaker_counts = speaker_seq_info$counts
  
  
  
  #simulated code occurrences using the connection matrices
  ##generate coded lines for each speaker. no of lines comes from trans mat
  ## account for null codes
  ## goes through line by line...
  code.num = ncol(adj_mats[[1]])
  coded.lines.list = list()
  coded.lines.list
  
  for(i in 1:length(speaker_seq)){
    speaker = speaker_seq[[i]]
    if (i == 1){ #randomly selecting first code from among possible windows with null code
      prev.code = code.num
      possible.codes = adj_mats[[speaker]][prev.code,]
      if(sum(possible.codes) == 0){## if all are zero pick null code
        code.vec = rep(0,code.num)
        code.vec[code.num] = 1
      }else{
        code.pos = sample(c(1:code.num),1,prob = possible.codes,replace = TRUE)
        code.vec = rep(0,code.num)
        code.vec[code.pos] = 1 
      }
    }else{
      prev.code = which(coded.lines.list[[i-1]] == 1)
      possible.codes = adj_mats[[speaker]][prev.code,]
      #check if all are zero 
      if(sum(possible.codes) == 0){## if all are zero pick null code #if this is fucked check here
        code.vec = rep(0,code.num)
        code.vec[code.num] = 1
      }else{
        code.pos = sample(c(1:code.num),1,prob = possible.codes,replace = TRUE)
        code.vec = rep(0,code.num)
        code.vec[code.pos] = 1
      }
    }
    coded.lines.list[[i]] = code.vec
  }
  coded.lines = do.call("rbind",coded.lines.list)
  #naming and cleaning
  colnames(coded.lines) = c(LETTERS[1:(code.num - 1)],"null_c")
  coded.lines = data.frame(coded.lines)
  coded.lines$Run = rep(det(mat_),nrow(coded.lines)) #getting unique identifier for the matrix
  coded.lines$Run = as.character(coded.lines$Run)
  coded.lines$Speaker = speaker_seq
  
  return(coded.lines)
}



sim_mods = function(codes,
                    trans_mat,
                    adj_mats,
                    window_size_inter,
                    type,
                    normalize,
                    steps){
  
  
  #simulate data
  #dat_ = sim_discourse_3.1(trans_mat = trans_mat,adj_mats = adj_mats,type = type,steps = steps)
  
  folder_path <- file.path("C:", "Users", "Desktop", "Swiecki_Extended_Model", "output", "new", "iterative_330_new", as.character(trans_mat$linetot))
  file_list <- list.files(folder_path, pattern = "\\.csv", full.names = TRUE)
  random_csv <- sample(file_list, 1)
  dat_ <- read.csv(random_csv)

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
  
  
  return(inter.lws)
  #  return(dat_) look later 
}
sim_boot.3.0 = function(codes, 
                        window, 
                        t_matrices,
                        adj_mats,
                        type,
                        normalize,
                        steps){
  
  
  
  #simulate dataset based on sampled matrices
  sim_inter_mods = map2(.x = t_matrices,
                        .y = adj_mats,
                        .f = sim_mods,
                        codes = codes,
                        window_size_inter = window,
                        type = type,
                        normalize = normalize,
                        steps = steps) 
  
  
  return(sim_inter_mods)
}
#############

#run simulation using these matrices
set.seed(1234)

mod.results = replicate(n = 1000, expr = sim_boot.3.0(
  codes = 8,
  window = 2,
  t_matrices = trans_mats,
  adj_mats = co_mats_team ,
  type = "val", 
  normalize = FALSE,
  steps = 200), simplify = FALSE)

#extract simulated adjacency vectors
mod.results = map(mod.results,bind_rows,.id = "id")

#add ids
add_rnames = function(x){
  rnames = paste(x$id,x$ENA_UNIT,sep = ".")
  rownames(x) = rnames
  return(x)
}

mod.results = map(mod.results,add_rnames)

#get real model
##get metadata
correct.meta = set.inter$meta.data %>% arrange(ENA_UNIT)

##arrange data
real_mod = set.inter$connection.counts %>% 
  arrange(ENA_UNIT) %>% 
  as.matrix()
##bind with metadata
real_mod = cbind(correct.meta,real_mod)

##re-order to align with simulated results
real_mod = real_mod %>% arrange(GroupName,UserName)
##restructure to be team list
team.counts = correct.meta %>% group_by(GroupName) %>% count()
names.list = list()
for (i in 1:nrow(team.counts)){
  group = team.counts[i,1]
  unitno = team.counts[i,2]
  group = rep(group, unitno)
  unitno = seq.int(1:as.numeric(unitno))
  ids = cbind(group,unitno)
  names.list[[i]] = ids
}
ids = do.call("rbind",names.list)
ids = data.frame(ids)
rownames(ids) = NULL
ids$id = paste(ids$group,ids$unitno,sep = ".")
rownames(real_mod) = ids$id

#find missing units from the simulation--those who ended up with no connections in a given run
full.names = rownames(real_mod)
missing.units = map(mod.results,get.silent.mod,all.names = full.names)
missing.units = discard(missing.units,function(x)length(x)==0)
missing.units = unique(unlist(missing.units))

#remove missing units from real and simulated data
real_mod = remove_missing_units(real_mod,missing.units,"real")
mod.results = map(mod.results,remove_missing_units,missing.units,"real")


#check that real and simulated data have the same number of units
unit.counts = map(mod.results,nrow)
real.count = nrow(real_mod)

if(all(unlist(unit.counts)==real.count)){
  print("all lists have equal counts")
}else{
  print("you messed up")
}

#compare using empirical distance test.
##See 
##Swiecki, Z. (in press). 
##The expected value test: A new statistical warrant for theoretical saturation. 
##Paper submitted to the Third International Conference on Quantitative Ethnography.

test = ena.compare.models(observedMod = real_mod,
                          simMods = mod.results,
                          method = "euclidean",
                          normalise = FALSE)

#calculate bias corrected, accelerated percentile intervals
test.ci = bca(test$distribution)
test.ci

test.ci = bca(test$distribution)
test.ci

mean(test$distribution)
mean(test$observed_distances)
?bca




#rm(list = ls())

library(rENA)
library(tidyverse)
library(markovchain)
library(ICC)
library(lmerTest)
library(performance)
library(PerformanceAnalytics)
library(lattice)
library(MuMIn)
library(HLMdiag)
library(parameters)

source('~/Rprojects/simulating-collab-discourse/functions/update.net.R')
source('~/RProjects/simulating-collab-discourse/functions/distance.R')
source('~/Rprojects/simulating-collab-discourse/functions/window_counts_code.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog_sym.R')
source('~/RProjects/simulating-collab-discourse/functions/soc.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/self_connections.R')
source('~/Rprojects/simulating-collab-discourse/functions/update.net.R')

################################################################################
#load data
rs.fg = read.csv(file = "~/Rprojects/simulating-collab-discourse/data/rs.fg.single.R1valid.csv",
                 stringsAsFactors = FALSE)

#set up model params
codenames = c("Performance.Parameters_c",
              "Client.and.Consultant.Requests_c",
              "Electric_c",
              "Hydraulic_c",
              "PAM_c",
              "Pneumatic_c",
              "SE_c",
              "Technical.Constraints_c")

rs.fg$Run = rep(1,nrow(rs.fg))
units = data.frame(rs.fg[,c("UserName","GroupName")])
conversation = data.frame(rs.fg[,c("GroupName")])
codes = data.frame(rs.fg[,c(codenames)])
meta = data.frame(rs.fg[,c("GameHalf","Run")])

#ena accumulation
accum.inter = 
  ena.accumulate.data(
    units = units,
    conversation = conversation,
    codes = codes,
    metadata = meta,
    window.size.back = 2)

#ena rotation  
set.inter = ena.make.set(enadata = accum.inter,
                         rotation.by = ena.rotate.by.mean, 
                         rotation.params = list(accum.inter$meta.data$GameHalf == "First",
                                                accum.inter$meta.data$GameHalf == "Second"))

#flip axes to ease interpretation
set.inter$points$MR1 = set.inter$points$MR1 * (-1)
set.inter$rotation$nodes$MR1 = set.inter$rotation$nodes$MR1 * (-1)

# #plot the ENA space
# fhalf = as.matrix(set.inter$points$GameHalf$First)
# fhalf.lws = as.matrix(set.inter$line.weights$GameHalf$First)
# fhalf.mean = colMeans(fhalf.lws)
# shalf = as.matrix(set.inter$points$GameHalf$Second)
# shalf.lws = as.matrix(set.inter$line.weights$GameHalf$Second)
# shalf.mean = colMeans(shalf.lws)
# sub = fhalf.mean - shalf.mean
# sub = sub * 3 #scale network for easier interpretation
# 
# newnames = c(c("Performance.Parameters",
#                "Client.and.Consultant.Requests",
#                "Electric",
#                "Hydraulic",
#                "PAM",
#                "Pneumatic",
#                "Series.Elastic",
#                "Technical.Constraints")) #codenames for plotting
# 
# code.labels = c("middle right","middle left", rep("middle right",5),"middle left")
# 
# plot = ena.plot(enaset = set.inter,
#                 title = "Pre-Jigsaw vs. Post-Jigsaw",
#                 font.size = 22,
#                 font.family = "Courier New");
# plot = ena.plot.points(plot, fhalf, colors = "red");
# plot = ena.plot.group(plot,fhalf,labels = "Pre-Jigsaw", colors = "red", confidence.interval = "box");
# plot = ena.plot.points(plot,shalf, colors = "blue")
# plot = ena.plot.group(plot, shalf,labels = "Post-Jigsaw", colors = "blue", confidence.interval = "box");
# plot = ena.plot.network(enaplot = plot,network = sub,colors = c("red","blue"),
#                         labels = newnames,
#                         label.offset = code.labels);
# 
# plot$plot


#compare position on first dimension between game halves
dat_ = set.inter$points
dat_$GameHalf = as.factor(dat_$GameHalf)
dat_$GameHalf = relevel(dat_$GameHalf,"First")
dat_= data.frame(dat_)
mod = lmer(MR1 ~ 1 + GameHalf + (1|GroupName),data = dat_)
summary(mod)

################################################################################
#calculate metrics that may relate to interdependence
##social symmetry (individual)--measure of interactivity. are they likely to respond to others
#measure by the diagonal of their talk transition matrix. Ranges from 0 to 1
#lower values means more interactive, higher values means less interactive
dat_ = dat_[,c(1:6)]
team.split = split(rs.fg,rs.fg$GroupName)
soc.syms.ind = lapply(team.split,soc.sym.ind,speakerCol = "UserName",teamCol = "GroupName")
soc.syms.ind = data.frame(do.call("rbind",soc.syms.ind))
names(soc.syms.ind) = c("soc.sym.ind","UserName","GroupName")
dat_$ENA_UNIT = as.character(dat_$ENA_UNIT)
dat_$UserName = as.character(dat_$UserName)
dat_$GroupName = as.character(dat_$GroupName)
dat_ = left_join(dat_,soc.syms.ind,by = c("UserName","GroupName"))

##cognitive symmetry -- measure of how similar individuals are to their teammates
#in terms of how similar their talk is overall
#average distance between code prob adjacency matrices of an individual
#and their teammates. Large distance means, different on average, small distance
#means similar on average

###accumulate with null code
codes2 = data.frame(rs.fg[,c(codenames,"null_c")])

accum.inter.null = 
  ena.accumulate.data(
    units = units,
    conversation = conversation,
    codes = codes2,
    metadata = meta,
    window.size.back = 2)

###get connections in adjacency form
co_mats = connection.matrix(accum.inter.null)

###make symmetric
co_mats = map(co_mats,update.net)

###get self-references for adj matrices
new.row.connection.counts = cbind(accum.inter.null$model$row.connection.counts,cons)
d = new.row.connection.counts %>% select(UserName,GroupName,all_of(con.names))
self_cons = d %>% group_by(UserName,GroupName) %>% summarise(across(contains("&"), ~ sum(.)),.groups = "keep")

###update adj mat diagonal with self references
co_mats = co_mats[sort(names(co_mats))]

update_diag = function(diags,mats){
  diags = ungroup(diags)
  diags = diags %>% select(contains("&"))
  for(i in 1:length(mats)){
    diag(mats[[i]]) = as.numeric(diags[i,])
  }
  return(mats)
}

co_mats = update_diag(self_cons,co_mats)

###convert to probabilities
speakers = accum.inter$meta.data$ENA_UNIT
speakers = sort(speakers)
codenames2 = c(codenames,"null_c")

####get window counts for each code for each person
speaker.list = list()
for (i in 1:length(speakers)){
  unit = speakers[[i]]
  window.count.list = list()
  for (j in 1:length(codenames2)){
    code = codenames2[[j]]
    window.count = window_counts_code(speaker = unit,
                                      code = code,
                                      row.connection.counts = new.row.connection.counts)
    window.count.list[[j]] = window.count
  } 
  names(window.count.list) = codenames2
  speaker.list[[i]] = unlist(window.count.list)
}
names(speaker.list) = speakers

####divide co_mats by line counts
co_mats = map2(.x = co_mats,.y = speaker.list,.f = function(x,y) x/y)

####remove nans introduced by zero division
remove.nans = function(x){
  nans = which(is.nan(x))
  infs = which(is.infinite(x))
  x[nans] = 0
  x[infs] = 0
  return(x)
}
co_mats = map(.x = co_mats,.f = remove.nans )

###restructure data to be in list for each team
team.names = unique(accum.inter$meta.data$GroupName)
co_mats_team = list()
for(i in 1:length(team.names)){
  ids = str_which(names(co_mats),team.names[i])
  mats = co_mats[ids]
  co_mats_team[[i]] = mats
}
names(co_mats_team) = team.names

###sort by unit names within team
for(i in 1:length(co_mats_team)){
  co_mats_team[[i]] = co_mats_team[[i]][order(names(co_mats_team[[i]]))]
}

###calculate cog symmetry
cog.sym.ind = map(co_mats_team, cog_sym_adj_real)
cog.sym.ind = bind_rows(cog.sym.ind)
names(cog.sym.ind) = c("ENA_UNIT","cog.sym.ind")
dat_ = left_join(dat_,cog.sym.ind,by = "ENA_UNIT")

dat_$cog.sym.ind = as.numeric(dat_$cog.sym.ind)
dat_$Run = as.numeric(dat_$Run)

##calculate level-2 metrics
dat_ = dat_ %>% group_by(GroupName) %>% 
  mutate(soc.mean = mean(soc.sym.ind),cog.mean = mean(cog.sym.ind))

################################################################################
#model data
##explore relationships
cor.dat = dat_[,c(6,7,8)]
chart.Correlation(cor.dat,histogram = TRUE)

##check if mixed model is necessary
ICCest(x = GroupName,y = MR1, data = dat_) #significant. nesting has an effect
#ICCest(x = UserName,y = MR1, data = dat_) #significant. nesting has an effect

##add unique identifier for model puposes

##explore hlm regression models
mod_test = lmerTest::lmer(MR1 ~ 1 + cog.sym.ind*soc.sym.ind + cog.mean + soc.mean + (1|GroupName),data = dat_)
summary(mod_test)
AIC(mod_test)
mod_test = lmerTest::lmer(MR1 ~ 1 + cog.sym.ind + cog.mean + soc.sym.ind + (1|GroupName),data = dat_)
summary(mod_test)
AIC(mod_test)
mod_test = lmerTest::lmer(MR1 ~ 1 + cog.sym.ind + cog.mean + soc.sym.ind + soc.mean + (1|GroupName),data = dat_)
summary(mod_test)
AIC(mod_test)
mod_test = lmerTest::lmer(MR1 ~ 1 + cog.sym.ind + cog.mean + (1|GroupName),data = dat_)
summary(mod_test)
AIC(mod_test)
mod_test = lmerTest::lmer(MR1 ~ 1 + cog.mean + (1|GroupName),data = dat_) #best model
summary(mod_test)
AIC(mod_test)

#rename cols and vals and rerun
names(dat_)[c(3,4,6,7,8,9,10)] = c("Team","Jigsaw","ENA1","interactivity","dissimilarity","int.mean","diss.mean")
dat_ = dat_ %>% mutate(Jigsaw = if_else(Jigsaw == "First","Pre","Post"))
mod_test = lmerTest::lmer(ENA1 ~ 1 + diss.mean + (1|Team),data = dat_) #best model
print(mod_test)
summary(mod_test)

r.squaredGLMM(mod_test)

################################################################################
#model diagnostics

##outliers
check_out = check_outliers(mod_test)
plot(check_out)

##level 1 residuals
residmod = hlm_resid(object = mod_test, level = 1, standardize = FALSE,type = "LS")
qplot(x = .ls.fitted, y = .ls.resid, data = residmod,geom = c("point","smooth"))
qplot(x = diss.mean, y = .ls.resid, data = residmod,geom = c("point","smooth"))

##heteroskedasticity
resid2mod = hlm_resid(object = mod_test, level = 1, standardize = "semi",type = "LS")
qplot(x = diss.mean, y = .semi.ls.resid, data = resid2mod) +
  geom_smooth(method = "lm") + ylab("semi-standarized resid") + xlab("diss.mean") 

##normality
norm_check = check_normality(mod_test)
plot(norm_check,type = "qq")

##level 2 residuals
random_check = check_normality(mod_test,effects = "random")
plot(random_check) #some evidence of non-normality however, 


#validation test (using empirical distance tests)
#rm(list = ls())

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
source('~/Rprojects/simulating-collab-discourse/functions/gen_code_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog_sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/matrices_from_observed.R')
source('~/Rprojects/simulating-collab-discourse/functions/empirical_distance_test.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove.silent.R')
source('~/Rprojects/simulating-collab-discourse/functions/empirical_paired.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove_missing_units.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_mods.R')
source('~/Rprojects/simulating-collab-discourse/functions/ena.compare.models.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')

#load data
#source('~/Rprojects/simulating-collab-discourse/analysis/real_data_results_final.R')

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

#run simulation using these matrices
set.seed(2)



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

mod.results = replicate(n = 5, expr = sim_boot.3.0(
  codes = 8,
  window = 2,
  t_matrices = trans_mats,
  adj_mats = co_mats_team,
  type = "real", 
  normalize = TRUE,
  steps = null), simplify = FALSE)


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
real_mod = set.inter$line.weights %>% 
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

write.csv(real_mod, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Real_mod.csv", row.names=FALSE)
write.csv(mod.results, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Sim_mod.csv", row.names=FALSE)


mod_a = real_mod %>% select(contains("&"))

mod_b = map(mod.results,select,contains("V"))
mod_b = map(mod_b,data.frame)

print(mod_a)
print(mod_b)

write.csv(mod_a, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Real_mod_a.csv", row.names=FALSE)
write.csv(mod_b, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Sim_mod_b.csv", row.names=FALSE)

test = ena.compare.models(observedMod = mod_a,
                          simMods = mod_b,
                          method = "euclidean")
print(test)
#calculate bias corrected, accelerated percentile intervals
test.ci = bca(test$distribution)
print(test.ci)


#data sim (combined trans and code matrix analysis. Adj-based code matrix generation)
#composite sim (jitter,bootstrap, random)
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
source('~/Rprojects/simulating-collab-discourse/functions/soc.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')
source('~/Rprojects/simulating-collab-discourse/functions/update_trans_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_dists.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_boot_3.1.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_code_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog_sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove.silent.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove_missing_units.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_mods_proj_1.1.R')
source('~/Rprojects/simulating-collab-discourse/functions/ena.compare.models.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_adj_matrix.R')

#load data
source("~/Rprojects/simulating-collab-discourse/analysis/generate_sim_grid.R")
load("~/Rprojects/simulating-collab-discourse/data/co_dists.Rdata")
source('~/Rprojects/simulating-collab-discourse/analysis/gen_proj_set.R')

#simulate data & run models
print("simulating data")
proj_set = set.inter$rotation

set.seed(1234)

t_matrix_sample
c_matrix_sample

mod.results = replicate(n = 10, expr = sim_boot.3.1(
                           codes = 9,
                           window = 2,
                           steps = 187, #average number of lines from observed data
                           type = "sim",
                           t_matrices = t_matrix_sample,
                           adj_mats = c_matrix_sample,
                           proj_set = proj_set,
                           version = "original",
                           iterative_mean = 2), simplify = FALSE)

mod.results

print("getting results")

#examine results
boot_res = map(mod.results,pluck,"coefs")
boot_mod = map(mod.results,pluck,"model")
boot_res_df = bind_rows(boot_res)

means = colMeans(boot_res_df) ##means

#for bca conf intervals https://rdrr.io/cran/coxed/man/bca.html
bcas = apply(X = boot_res_df,MARGIN = 2,FUN = bca)

#find range of outcome var

dfs = map(boot_mod,pluck,"frame")
dfs = bind_rows(dfs)
out_range = range(dfs$MR1)

mod.results
dfs
out_range




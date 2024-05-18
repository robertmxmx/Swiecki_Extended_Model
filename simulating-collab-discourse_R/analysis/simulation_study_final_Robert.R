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
source('~/Rprojects/simulating-collab-discourse/functions/sim_mods_proj_1.1_Robert.R')
source('~/Rprojects/simulating-collab-discourse/functions/ena.compare.models.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_adj_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_discourse_3.1.R')
#load data
#source("~/Rprojects/simulating-collab-discourse/analysis/ona_ena.R")
source("~/Rprojects/simulating-collab-discourse/analysis/generate_sim_grid_Robert.R")
load("~/Rprojects/simulating-collab-discourse/data/co_dists.Rdata")
source('~/Rprojects/simulating-collab-discourse/analysis/gen_proj_set.R')

library(rENA)
library(ona)
library(tma)
library(tidyverse)
library(dplyr)

#simulate data & run models
print("simulating data")
proj_set = set.inter$rotation

#set.seed(1253)


data_sim = list()
for (i in 1:100){ 
  t_matrix_sample
  c_matrix_sample 
  data_sim_value = map2(.x = t_matrix_sample,
                               .y = c_matrix_sample,
                               .f = sim_mods_proj_1.1,
                               codes = 9,
                               window_size_inter = 1,
                               type = "sim",
                               steps = 300,
                               proj_set = proj_set,
                               version = "original",
                               iterative_mean = 2)
  
  data_sim_value <- bind_rows(data_sim_value)
  data_sim[[i]] = data_sim_value
}
data_sim

ona_ena = function(data,unitCols,codeCols){
  #browser()

  #knitr::opts_chunk$set(echo = TRUE)
  #rm(list = ls())

  #Prep data
  data1 = data
  data2 = data1
  
  data1$class = rep("NoUptake", nrow(data1))
  data2$class = rep("Uptake", nrow(data2))
  
  data = rbind(data1,data2)
  
  #Set up ONA models

  my_hoo_rules <- conversation_rules(
    (class %in% UNIT$class & 
       Run %in% UNIT$Run #team %in% UNIT$team
    ))
  #Accumulate connections
  accum.ona <-
    contexts(data, 
             units_by = unitCols, 
             hoo_rules = my_hoo_rules) |>
    accumulate_contexts(codes = codeCols, 
                        decay.function = decay(simple_window, window_size = 1),
                        return.ena.set = FALSE)
  
  #remove diagonal and combine connections for nouptake
  df = as.matrix(accum.ona$connection.counts$class$NoUptake)
  
  #print(df)
  #as.vector(df[1,])
  
  matrices <- apply(df, 1, function(row) {
    # Number of items in a row that form a square matrix
    n <- sqrt(length(row))
    # Reshape the row into a square matrix
    matrix(as.numeric(row), nrow = n, ncol = n)
    #browser()
  },simplify = FALSE)
  
  #check
  #matrices[[1]]
  
  #update matrices
  
  matrices <- map(matrices, function(x) {
    #remove diagonal
    diag(x) = 0 
    #combine upper and lower tris
    up = x[upper.tri(x)]
    low = x[lower.tri(x)]
    both = up + low
    #split evenly
    even = both/2
    #fill matrix
    x[upper.tri(x)] = even
    x[lower.tri(x)] = even
    return(x)
  })
  
  #check
  #matrices[[1]]
  
  #convert back to vector for each unit
  
  vectors = map(matrices, function(x){
    adj.vec = c(x)
    return(adj.vec)
  })
  
  #check
  #vectors[1]
  
  #combine
  connection.counts.nouptake = do.call("rbind", vectors)
  
  #add colnames and metadata
  connection.counts.nouptake = as.data.table(connection.counts.nouptake)
  colnames(connection.counts.nouptake) = colnames(df)
  connection.counts.nouptake = cbind(accum.ona$meta.data$class$NoUptake,connection.counts.nouptake)
  
  #covert to ena matrix and connection object
  connection.counts.nouptake = rENA::as.ena.matrix(connection.counts.nouptake)
  connection.counts.nouptake = ona::as.ena.co.occurrence(connection.counts.nouptake)
  
  #check
  #class(connection.counts)
  
  #combine connections for uptake
  df = as.matrix(accum.ona$connection.counts$class$NoUptake)
  
  
  #as.vector(df[1,])
  
  matrices <- apply(df, 1, function(row) {
    # Number of items in a row that form a square matrix
    n <- sqrt(length(row))
    # Reshape the row into a square matrix
    matrix(as.numeric(row), nrow = n, ncol = n)
    #browser()
  },simplify = FALSE)
  
  #check
  #matrices[[1]]
  
  #update matrices
  
  matrices <- map(matrices, function(x) {
    #remove diagonal
    #diag(x) = 0 
    #combine upper and lower tris
    up = x[upper.tri(x)]
    low = x[lower.tri(x)]
    both = up + low
    #split evenly
    even = both/2
    #fill matrix
    x[upper.tri(x)] = even
    x[lower.tri(x)] = even
    return(x)
  })
  
  #check
  #matrices[[1]]
  
  #convert back to vector for each unit
  
  vectors = map(matrices, function(x){
    adj.vec = c(x)
    return(adj.vec)
  })
  
  #check
  #vectors[1]
  
  #combine
  connection.counts.uptake = do.call("rbind", vectors)
  
  #add colnames and metadata
  connection.counts.uptake = as.data.table(connection.counts.uptake)
  colnames(connection.counts.uptake) = colnames(df)
  connection.counts.uptake = cbind(accum.ona$meta.data$class$Uptake, connection.counts.uptake)
  
  #covert to ena matrix and connection object
  connection.counts.uptake = rENA::as.ena.matrix(connection.counts.uptake)
  connection.counts.uptake = ona::as.ena.co.occurrence(connection.counts.uptake)
  
  #check
  #class(connection.counts)
  
  #replace original co-occurrence counts
  #browser()
  new.counts = rbind(connection.counts.nouptake,connection.counts.uptake)
  new.counts = rENA::as.ena.matrix(new.counts)
  new.counts = ona::as.ena.co.occurrence(new.counts)
  
  accum.ona$connection.counts = new.counts
  
  #Make set with means rotation between nouptake and uptake

  set.ona <-
    model(accum.ona,                            
          rotate.using ="mean",                  
          rotation.params =                      
            list(NoUptake=accum.ona$meta.data$class == "NoUptake",
                 Uptake=accum.ona$meta.data$class == "Uptake")   
    )
  #Do statistical test and get effect size
  
  x = set.ona$points %>% filter(class == "NoUptake") %>% select(MR1) %>% as.vector() %>% unlist()
  y = set.ona$points %>% filter(class == "Uptake") %>% select(MR1) %>% as.vector() %>% unlist()
  
  test = t.test(x,y,paired = TRUE)
  
  #test
  
  ## calculate effect size
  est = test$estimate
  
  
  diffs = x - y
  
  sd_diffs = sd(diffs)
  
  d = as.numeric(est/sd_diffs)
  
  return(list(d,est,test$p.value))
}


data_sim <- lapply(data_sim, function(df) {
  return(df)
})

data_sim


a = map(.x = data_sim,
            .f = ona_ena,
            unitCols = c("Speaker","class","Run"),
            codeCols = c('A', 'B', 'C', 'D', 'E', 'F','G','H'))
a
#codeCols = c(LETTERS[1:8])

flatten_df <- data.frame(d = unlist(lapply(a, function(x) x[[1]])),
                         est = unlist(lapply(a, function(x) x[[2]])),
                         p_value = unlist(lapply(a, function(x) x[[3]])))

write.csv(flatten_df, file = "output.csv", row.names = FALSE)



#data_old = rENA::RS.data
#data_old
#b = ona_ena(data_old,  
#            unitCols = c("Condition", "UserName","class"),
#            codeCols = c('Data', 'Technical.Constraints', 'Performance.Parameters', 'Client.and.Consultant.Requests', 'Design.Reasoning', 'Collaboration')
#)
#b


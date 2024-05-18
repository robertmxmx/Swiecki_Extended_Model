#script for generating simulation grid--the joint distribution
#of cognitive and social symmetry matrices used in the simulation

#load libraries
library(rENA)
library(magrittr)
library(markovchain)
library(tidyverse)

#source functions
source('~/Rprojects/simulating-collab-discourse/functions/generate_speaker_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/interactivity.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_speaker_sequence.R')
source('~/Rprojects/simulating-collab-discourse/functions/soc.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/update_trans_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_code_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog_sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_adj_matrix.R')


SIZE = 100
#set.seed(1000)
Desired_Uptake_Average = 0.006
sd = 0.0001

generate_random_matrices <- function(n, size, x) {
  matrices_list <- list()
  
  for (i in 1:n) {
    matrices <- list()
    for (j in 1:x) {
      random_matrix <- matrix(runif(size*size), nrow = size)
      normalized_matrix <- t(apply(random_matrix, 1, function(row) row / sum(row)))
      matrices[[j]] <- normalized_matrix
    }
    matrices_list[[i]] <- matrices
  }
  return(matrices_list)
}

cap_at_3sd <- function(input) {
  threshold <- 3 * sd
  random_number <- rnorm(1, mean = 0, sd = sd)
  result <- input + random_number
  return(result)
}

t_matrix_sample <- generate_random_matrices(n=SIZE, size=5,x=1)
t_matrix_sample <- flatten(t_matrix_sample)
t_matrix_sample[1]

adj_mat_list <- generate_random_matrices(n=SIZE, size=9,x=5)

set_diagonal_and_cap <- function(matrix, desired_avg, sd) {
  n <- nrow(matrix)
  diag(matrix) <- desired_avg  
  diag_capped <- sapply(diag(matrix), function(diag_val) {
    cap_at_3sd(diag_val)
  })
  diag_capped <- pmax(pmin(diag_capped, 1), 0) 
  diag(matrix) <- diag_capped 
  
  return(matrix)
}

normalize_rows <- function(matrix) {
  n <- nrow(matrix)
  diag_vals <- diag(matrix)
  
  for (i in 1:n) {
    row_sum_excluding_diag <- sum(matrix[i, ]) - diag_vals[i]
    normalization_factor <- 1 - diag_vals[i]
    if (row_sum_excluding_diag != 0) {
      matrix[i, ] <- matrix[i, ] * (normalization_factor / row_sum_excluding_diag)
    }
    matrix[i, i] <- diag_vals[i]  # Restore diagonal values
  }
  
  return(matrix)
}

adjusted_adj_mat_list <- lapply(adj_mat_list, function(matrices) {
  lapply(matrices, function(matrix) {
    adjusted_matrix <- set_diagonal_and_cap(matrix, Desired_Uptake_Average, sd)
    normalized_matrix <- normalize_rows(adjusted_matrix)
    return(normalized_matrix)
  })
})

# Print adjusted matrices
adj_mat_list <- adjusted_adj_mat_list
#adj_mat_list[1]
c_matrix_sample <- adjusted_adj_mat_list
#c_matrix_sample

#c_matrix_ids = sample(c(1:length(adj_mat_list)),size = 5,replace = TRUE)
#c_matrix_sample = adj_mat_list[c_matrix_ids]

########################################################################################################
# #view and check distributions of social and cognitive symmetry
# ##calc social
# t_check_full = lapply(t_matrix_dist,interactivity) 
# t_check_full = bind_rows(t_check_full)
# t_check_full$type = rep("pop",nrow(t_check_full))
# t_check = lapply(t_matrix_sample,interactivity)
# t_check = bind_rows(t_check)
# t_check$type = rep("sample",nrow(t_check))
# 
# ##calc cog
# c_check_full = lapply(adj_mat_list,cog_sym_adj) 
# c_check_full = bind_rows(c_check_full)#do.call("rbind",c_check_full)
# c_check_full$type = rep("pop",nrow(c_check_full))
# c_check = lapply(c_matrix_sample,cog_sym_adj)
# c_check = bind_rows(c_check)
# c_check$type = rep("sample",nrow(c_check))
# 
# ##plot social symmetry distributions
# t_matrix_info = rbind(t_check_full, t_check)
# p = ggplot(aes(x = interactivity.ind, fill = type),data = t_matrix_info) +
#   geom_density(alpha = 0.2, position = "identity") +
#   xlim(c(0,1)) + ggtitle("Speakers")
# p
# ##plot cognitive symmetry distributions
# c_matrix_info = rbind(c_check_full, c_check)
# p2 = ggplot(aes(x = avgdists, fill = type),data = c_matrix_info) +
#   geom_density(alpha = 0.2, position = "identity")+ 
#   ggtitle("Codes") #+ xlim(0,1.5)
# 
# ##plot joint distribution from sample
# both_mat = cbind(t_check,c_check)
# both_mat = both_mat[,c(1,2,4,5)]
# p3 = ggplot(aes(x = avgdists, y = interactivity.ind), data = both_mat) +
#   geom_point() + ggtitle("Joint Distribution",subtitle = "Simulation") + ylim(0,1)+ xlim(0,4)+ xlab(label = "dissimilarity") + ylab(label = "interactivity") + theme_minimal()
# 
# ##plot joint distribution from real
# p4 = ggplot(aes(x = dissimilarity, y = interactivity), data = dat_) +
#   geom_point() + ggtitle("Joint Distribution",subtitle = "Observed") + ylim(0,1)+ xlim(0,4)+ xlab(label = "dissimilarity") + ylab(label = "interactivity") + theme_minimal()

average_diagonal <- function(list_of_lists_of_matrices) {
  diagonal_values <- c()  # Vector to store diagonal values
  
  # Loop through each element in the list
  for (mat_list in list_of_lists_of_matrices) {
    # Loop through each matrix in the nested list
    for (mat in mat_list) {
      # Extract diagonal values from the current matrix
      diagonal_values <- c(diagonal_values, diag(mat))
    }
  }
  
  # Calculate the average of diagonal values
  avg_diagonal <- mean(diagonal_values)
  
  return(avg_diagonal)
}

# Call the function with your list 'adj_mat_list_jit'
average_diag <- average_diagonal(adj_mat_list)
#average_diag

#function for generating code probability matrix for simulated teams

##includes a "null" code. 
##properties of the matrix: 
#row and col sums can exceed 1 because lines may be multi-coded
#individual cells can vary from 0 to 1 but tend to be below 0.5
# will need to update later to vary the null code. leaving out for now

##params
#codes = no. of codes
#speakers = no. of speakers

gen_code_matrix = function(codes,speakers){
  tot = codes*speakers
  vals = runif(tot,0,1)
  code_mat = matrix(data = vals,nrow = speakers,ncol = codes) #will likely need to vary to have less densely coded matrices
  return(code_mat)
}

gen_code_matrix_norm = function(codes,speakers,mean,sd){
  tot = codes*speakers
  vals = rtruncnorm(n = tot,a = 0,b = 1,mean = mean,sd = sd)
  code_mat = matrix(data = vals,nrow = speakers,ncol = codes) 
  return(code_mat)
}

#separate distribution for null code and regular code
gen_code_matrix_mixed = function(codes,speakers,mean,sd){#uses separate distribution for the null code
  tot = codes*speakers
  vals = rtruncnorm(n = tot,a = 0, b = 1,mean = mean,sd = sd)
  code_mat = matrix(data = vals,nrow = speakers,ncol = codes)
  null_code = rnorm(n = nrow(code_mat),mean = 0.5,sd = 0.13)
  code_mat = cbind(null_code,code_mat)
  return(code_mat)
}

#role based

gen_code_matrix_role = function(ncodes,
                                nroles,
                                speakers,
                                regMean,
                                roleInflation){
#make null code  
null_c = rnorm(n = speakers,mean = 0.5,sd = 0.13)

#make non-role codes
reg_tot = (ncodes - nroles) * speakers
#reg_vals = rbeta(n = reg_tot,shape1 = 2,shape2 = 4)
reg_vals = rtruncnorm(n = reg_tot,a = 0,b = 1,mean = regMean,sd = 0.15)
reg_mat = matrix(data = reg_vals,nrow = speakers,ncol = (ncodes - nroles))

#make role codes; each speaker gets a different role code; current version
#requires speakers to equal number of role codes
code.vec.list = list()
for (i in 1:speakers){
  code.vec = rtruncnorm(n = nroles,a = 0, b = 1,mean = 0,sd = .05)
  code.vec[i] = rtruncnorm(n = 1,a = 0, b = 1,mean = roleInflation,sd = .05)
  code.vec.list[[i]] = code.vec
}

role_mat = as.matrix(do.call("rbind",code.vec.list))

#combine
code_mat = cbind(null_c,role_mat,reg_mat)

return(code_mat)
}
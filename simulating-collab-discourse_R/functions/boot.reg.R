#boot strap regression function
boot.reg = function(data,indices){
  data = data[indices,]
  mod.dat =  lmer(formula = MR1 ~ 1 + cog.sym.ind + soc.sym.ind + (1|GroupName),data = data)
  x =   coefficients(mod.dat)
  betas = ncol(x[[1]])
  x = x[[1]][1,]
  x = as.numeric(x)
  names(x) = c("intercept","cog.sym","soc.sym")
  return(x)
}


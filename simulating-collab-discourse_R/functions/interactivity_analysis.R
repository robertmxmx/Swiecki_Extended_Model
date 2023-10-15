interactivity = function(speakers, run){
  diag_vals = runif(n = speakers, min = 0, max = 0.95)
  mat_ = gen_speaker_matrix_diag(diag_vals)
  #individual interactivity
  ind = diag(mat_)
  #group interactivity
  base_mat = diag(x = 1,speakers,speakers) #n,n
  group = norm(x = base_mat-mat_,type = "F")
  speakers = seq.int(1:nrow(mat_))
  res = data.frame(cbind(speakers,ind))
  colnames(res) = c("speaker","interactivity.ind")
  res$interactivity.group = rep(group,nrow(res))
  res$run = rep(run,nrow(res))
  #return(res) #update to keep matrices
  return(list(metrics = res,matrix = mat_))
}

test = lapply(c(1:10000), interactivity,
              speakers = 6)
test = do.call("rbind",test)
#head(test)
hist(test$interactivity.ind)
hist(test$interactivity.group)

ind.quant = quantile(test$interactivity.ind,probs = seq(0,1,0.333))
group.quant = quantile(test$interactivity.group,probs = seq(0,1,0.333))

Percentile_00  = ind.quant[1]
Percentile_33  = ind.quant[2]
Percentile_67  = ind.quant[3]
Percentile_100 = ind.quant[4]


RB = rbind(Percentile_00, Percentile_33, Percentile_67, Percentile_100)
dimnames(RB)[[2]] = "Value"

RB

test$ind.int[test$interactivity.ind >= Percentile_00 & test$interactivity.ind  <  Percentile_33]  = "Upper_third"
test$ind.int[test$interactivity.ind  >= Percentile_33 & test$interactivity.ind  <  Percentile_67]  = "Middle_third"
test$ind.int[test$interactivity.ind  >= Percentile_67 & test$interactivity.ind  <= Percentile_100] = "Lower_third"

Percentile_00_g  = group.quant[1]
Percentile_33_g  = group.quant[2]
Percentile_67_g  = group.quant[3]
Percentile_100_g = group.quant[4]

RB_group= rbind(Percentile_00_g, Percentile_33_g, Percentile_67_g, Percentile_100_g)
dimnames(RB_group)[[2]] = "Value"

RB_group

test$group.int[test$interactivity.group >= Percentile_00_g & test$interactivity.group  <  Percentile_33_g]  = "Lower_third"
test$group.int[test$interactivity.group  >= Percentile_33_g & test$interactivity.group  <  Percentile_67_g]  = "Middle_third"
test$group.int[test$interactivity.group  >= Percentile_67_g & test$interactivity.group  <= Percentile_100_g] = "Upper_third"

table(list(test$ind.int,test$group.int))


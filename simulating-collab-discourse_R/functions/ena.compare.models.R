# this function tests whether a given ENA model (individual or group), m,
# is statistically different from a distribution ENA models (typically
# models where randomness has been introduced) This is an empirical test (i.e.,
# it does not assume a particular distribution). It can be thought of as an
#empirical Hoetelling's Test

ena.compare.models = function(observedMod,
                              simMods,
                              method = c("euclidean","mahalanobis")
                              ){
  
  
  #browser()
  #remove meta
  observedMod = observedMod %>% select(contains("&"))
  simMods = map(simMods,select,contains("V"))
  simMods = map(simMods,data.frame)
  
  

  #calculate the mean of the distribution of models (i.e., the centroid of the 
  #point cloud)
  meanMod = Reduce("+",simMods)/length(simMods)
  
  #transform the distribution so that we have an mxn matrix of points for each indiv-
  #idual where m = number of simulations and n = number of variables (connections)
  # will need this to produce the covariance matrices for the distance metric calculation
  simModsByUnit = list()
  for (u in 1:nrow(simMods[[1]])){
    unitDfList = list()
    for (n in 1:length(simMods)){
      unitDfList[[n]] = simMods[[n]][u,]
    }
    unitDf = do.call("rbind",unitDfList)
    simModsByUnit[[u]] = unitDf
  }
  
  #browser()
  #generate covariance matrices 
  if (method == "euclidean"){
    #for euclidean distance all cov matrices are identity 
    #https://en.wikipedia.org/wiki/Mahalanobis_distance
    dims = ncol(simModsByUnit[[1]])
    covs = replicate(n = length(simModsByUnit),diag(1,nrow = dims,ncol = dims),simplify = F)
  }

  else if (method == "mahalanobis"){
    covs = lapply(simModsByUnit,cov) #get covariance matrices for each unit in the sim data
  }
  
  else{
    stop("Chosen method not supported. Ensure you are using either Euclidean or Mahalanobis")
  }
  
  # calculate distance between each observed point and the centroid.
  obs.distances = get.mah.dist(x1 = observedMod,x2 = meanMod,covmats = covs)
  #mean these distances to get the test statistic
  testStat = mean(obs.distances)
  #Calculate distances between each point in the distribution and the centroid
  #Obtain a distribution of distances. This is the null hypothesis distribution
  distribution = sapply(seq.int(length(simMods)),
                          function (i) mean(get.mah.dist(x1 = simMods[[i]],
                                                         x2 = meanMod,
                                                         covmats = covs)))
  # get pval (number of distances more extreme than the test stat)
  x = sum(distribution >= testStat)
  p = x/length(distribution) #get p-val
  ci = quantile(distribution,probs = c(0.25,0.975))
    
  return(list(statistic = testStat, pval = p, ci = ci, distribution = distribution,observed_distances = obs.distances))
}

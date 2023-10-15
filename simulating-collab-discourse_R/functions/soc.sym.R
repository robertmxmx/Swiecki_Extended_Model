#function for calculating the social symmetry of a given group in
#collaborative discourse data

#group one is distance from the identity ?matrix of the team

soc.sym = function(groupData,
                   speakerCol){
  #get sequence of speakers
  sequence = groupData[,speakerCol]
  #get unique speakers
  speakers = unique(groupData[,speakerCol])
  #calculate the number of unique speakers
  n = length(speakers)
  #calculate the base matrix (i.e, nxn diagaonal matrix (=1) where n = number
  #of speakers)
  baseMat = diag(x = 1,n,n)
  #calculated the transition probablilty matrix from the given sequence
  groupMat = createSequenceMatrix(sequence,sanitize = FALSE, toRowProbs = TRUE)
  #browser()
  #calculate the distance between the matrices
  distance = norm(x = groupMat-baseMat,type = "F")
  
  #return(list(distance = distance, baseMat = baseMat, groupMat = groupMat))
  return(distance)
}

soc.sym.ind = function(groupData,
                   speakerCol,teamCol){
  #get sequence of speakers
  sequence = groupData[,speakerCol]
  #get unique speakers
  speakers = unique(groupData[,speakerCol])
  #calculate the number of unique speakers
  n = length(speakers)
  #calculate the base matrix (i.e, nxn diagaonal matrix (=1) where n = number
  #of speakers)
  baseMat = diag(x = 1,n,n)
  #calculated the transition probablilty matrix from the given sequence
  #browser()
  groupMat = createSequenceMatrix(sequence,sanitize = FALSE, toRowProbs = TRUE)
  soc.ind = diag(groupMat)
  soc.ind = 1-soc.ind #re-map
  soc.ind = as.data.frame(soc.ind)
  soc.ind$Speaker = rownames(soc.ind)
  team = unique(groupData[,teamCol])
  soc.ind$Team = rep(team,nrow(soc.ind))
  return(soc.ind)
}
## example
# testdat = dat_ %>% filter(Team == "4/1/1999", Scenario == "Bravo")
# test.dist = soc.sym.ind(groupData = testdat,speakerCol = "Speaker",teamCol = "Team")

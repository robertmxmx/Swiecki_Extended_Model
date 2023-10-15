#script for extracting social and cognitive matrices from real data

##social matrices
get_trans_mat = function(groupData, 
                          speakerCol, 
                          teamCol){
  #get sequence of speakers
  sequence = groupData[,speakerCol]
  #get unique speakers
  speakers = unique(groupData[,speakerCol])
  #calculated the transition probablilty matrix from the given sequence
  groupMat = createSequenceMatrix(sequence,sanitize = FALSE, toRowProbs = TRUE)
  line_tot = nrow(groupData)
  return(list(mat = groupMat,linetot= line_tot))
}

##code matrices
#need to add null code beforehand

baserates = function(groupData,unitCols){
  #browser()
  speakers = unique(groupData[,unitCols])
  code.vecs = groupData %>%
    group_by(!!!syms(unitCols)) %>% 
    summarise(across(.cols = contains("_c"),.fns = mean))
  return(code.vecs)
}



##############################################################################

# testDat = team.split[[1]]
# # get_trans_mat(groupData = testDat,
# #               speakerCol = "UserName",
# #               teamCol = "GroupName")
# 
# x = lapply(team.split,baserates,unitCols = c("UserName","GameHalf"))

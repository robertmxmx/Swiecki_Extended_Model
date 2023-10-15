#topic coverage TODO



topic.coverage = function(vec){
  s = sum(vec)
  hits = length(which(vec>0))
  coverage = s*hits
  return(coverage)
}


get.topic.coverage = function(groupData,unitCols,teamCol){
  speakers = unique(groupData[,unitCols])
  code.vecs = groupData %>%
    group_by(!!!syms(unitCols)) %>% 
    summarise(across(.cols = contains("_c"),.fns = mean))
  
  code.vecs = code.vecs %>% 
    rowwise() %>% 
    mutate(coverage = topic.coverage(across(contains("_c"))))
  
  return(code.vecs)
  
}

# 
# test.dat = team.split[[1]]
# 
# test = get.topic.coverage(test.dat,c("UserName","GameHalf"),teamCol = "GroupName")

#looking at baserates for  observed codes

baserates = function(groupData,unitCols){
  #browser()
  speakers = unique(groupData[,unitCols])
  code.vecs = groupData %>%
    group_by(!!!syms(unitCols)) %>% 
    summarise(across(.cols = contains("_c"),.fns = mean))
}

# rs.fg = rs.fg %>% 
#   rowwise(line.id) %>% 
#   mutate(total = base::sum(across(contains("_c"))))
# 
# rs.fg$null_c = ifelse(rs.fg$total == 0,1,0)
# 
# team.split = split(rs.fg,rs.fg$GroupName)
# test = lapply(team.split,baserates,unitCols = c("UserName","GameHalf"))
# test.df = do.call("rbind",test)
# 
# test.df = test.df[,-c(1,2,7)]
# 
# vals = unlist(test.df)

#function for getting window counts per code per person in the data

#can get this from adj matrices. remove later

window_counts_code = function(speaker,code,row.connection.counts){
  #get connection data
  d = row.connection.counts %>% select(contains('&'))
  d$ENA_UNIT = row.connection.counts$ENA_UNIT
  
  #subset by individual
  d = d %>% filter(ENA_UNIT == speaker)
  
  #subset by code
  d = d %>% select(contains(code))
  
  #count_windows
  window_counts = sum(rowSums(d))
  
  return(window_counts)
}
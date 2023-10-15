#function for removing units that don't have lines in at least one sim run


get.silent.mod = function(all.names,sim.results){
  silent = which(all.names %in% rownames(sim.results)==FALSE)
  silent.name = all.names[silent]
  return(silent.name)
}


get.silent = function(all.names,sim.results){
  silent = which(all.names %in% names(sim.results)==FALSE)
  silent.name = all.names[silent]
  return(silent.name)
}

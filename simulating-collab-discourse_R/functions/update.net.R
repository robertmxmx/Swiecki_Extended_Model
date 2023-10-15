### function to update diag and lower.tri of code adjacency matrices

update.net = function(net){
  diag(net) = 0
  net[lower.tri(net)] = t(net)[lower.tri(net)]
  return(net)
}



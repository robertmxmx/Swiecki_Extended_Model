# large grain unit test for the simulation. If window_size_inter is set to 1
# returned distances should be all 0. This should return false

unit_test_dist = function(runs){
  runs = seq.int(1:runs)
  rand_dists = lapply(runs, sim_dists,speakers = 5, codes = 8,window_size_inter = 1)
  dists = unlist(map(rand_dists,1))
  any(dists > 0)
}


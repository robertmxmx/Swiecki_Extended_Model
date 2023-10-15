#com density

soc_mat = gen_speaker_matrix(5)
cog_mat = gen_code_matrix_norm(codes = 8,speakers = 5,.4,sd = .4)


com.density = dat_ %>%
  rowwise(line.id) %>%
  mutate(total = base::sum(!!!syms(codenames)))
com.density = com.density %>%
  group_by(!!!syms(c("UserName","GameHalf"))) %>%
  summarise(density = mean(total))
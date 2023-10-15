#script for generating projection set
library(rENA)
#load data
rs.fg = read.csv(file = "~/Rprojects/simulating-collab-discourse/rs.fg.sim.test.csv",stringsAsFactors = FALSE)
codenames = c("Performance.Parameters_c",
              "Client.and.Consultant.Requests_c",
              "Electric_c",
              "Hydraulic_c",
              "PAM_c",
              "Pneumatic_c",
              "SE_c",
              "Technical.Constraints.2_c")

rs.fg$Run = rep(1,nrow(rs.fg))
units = data.frame(rs.fg[,c("UserName","GroupName")])
conversation = data.frame(rs.fg[,c("GroupName")])
codes = data.frame(rs.fg[,c(codenames)])
meta = data.frame(rs.fg[,c("GameHalf","Run")])

accum.inter = 
  ena.accumulate.data(
    units = units,
    conversation = conversation,
    codes = codes,
    metadata = meta,
    window.size.back = 2)

set.inter = ena.make.set(enadata = accum.inter,
                         rotation.by = ena.rotate.by.mean, 
                         rotation.params = list(accum.inter$meta.data$GameHalf == "First",
                                                accum.inter$meta.data$GameHalf == "Second"))

#flip first axis for interpetation
set.inter$points$MR1 = set.inter$points$MR1 * (-1)


#script for getting self connection counts 
rs.fg = read.csv(file = "~/Rprojects/simulating-collab-discourse/rs.fg.sim.test.csv",stringsAsFactors = FALSE)


codenames = c("Performance.Parameters_c",
              "Client.and.Consultant.Requests_c",
              "Electric_c",
              "Hydraulic_c",
              "PAM_c",
              "Pneumatic_c",
              "SE_c",
              "Technical.Constraints.2_c")

code_cols = c(codenames,"null_c")

dat <- data.table::as.data.table(rs.fg);
conversations <-  c("Condition", "GroupName");

window.size <- 2

summed_stanzas <- dat[, {
  referants <- as.matrix(.SD[, c(code_cols), with = F]);
  colnames(referants) <- NULL;
  summed <- .SD[, 
                {
                  padding_rows <- data.table::data.table(matrix(0, nrow = window.size - 1, ncol = ncol(.SD), dimnames = list(NULL, colnames(.SD))))
                  combined <- rbind(padding_rows, .SD)
                  summed <- data.table::frollsum(combined, window.size)
                  summed_wo_padding <- lapply(summed, function(l) l[window.size:nrow(combined)])
                  summed_wo_padding
                }
                ,.SDcols = code_cols];
  
  summed * referants
}, by = c(conversations)]

summed_stanzas

cons = data.frame(summed_stanzas)
cons = cons[,-c(1:length(conversations))]
#update vals
cons = apply(cons,MARGIN = 2,FUN = function(x)ifelse(x >= 2,1,0))
##names
con.names = paste(code_cols,code_cols,sep = " & ")
colnames(cons) = con.names





library(reshape2)

data <- read.table(file = "../mutation_table.txt", header = T, sep = "\t")
mut <- melt(data, id.vars=c("UPN"),na.rm = TRUE)

write.table(mut, file = "../mutation_table_final.txt", sep = "\t", row.names = F, quote = F)

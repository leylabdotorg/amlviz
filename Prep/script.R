reads <- read.table(file = "lfq.txt", header = T, sep = "\t", check.names = F)
names(reads)[1] <- "gene"
rownames(reads) <- reads$gene
for(i in 1:nrow(reads))
{
  print(i)
  write.table(reads[i,], file=paste("Gene_files/LFQ/", rownames(reads)[i], ".tsv", sep=""), row.names=FALSE, sep="\t", quote = FALSE)
}



reads <- read.table(file = "tmt.txt", header = T, sep = "\t", check.names = F)
names(reads)[1] <- "gene"
rownames(reads) <- reads$gene
for(i in 1:nrow(reads))
{
  print(i)
  write.table(reads[i,], file=paste("Gene_files/TMT/", rownames(reads)[i], ".tsv", sep=""), row.names=FALSE, sep="\t", quote = FALSE)
}



reads <- read.table(file = "mRNA.txt", header = T, sep = "\t", check.names = F)
names(reads)[1] <- "gene"
rownames(reads) <- reads$gene
for(i in 1:nrow(reads))
{
  print(i)
  write.table(reads[i,], file=paste("Gene_files/mRNA/", rownames(reads)[i], ".tsv", sep=""), row.names=FALSE, sep="\t", quote = FALSE)
}



reads <- read.table(file = "phospho.txt", header = T, sep = "\t", check.names = F)
genes <- as.character(unique(reads$Gene))
for(i in 1:length(genes))
{
  print(i)
  a <- as.data.frame(reads[which(reads$Gene %in% genes[i]),])
  a <- subset(a, select = -c(Gene))
  write.table(a, file=paste("Gene_files/Phospho/",genes[i], ".tsv", sep=""), row.names=FALSE, sep="\t", quote = FALSE)
}



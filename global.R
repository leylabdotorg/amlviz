library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggbeeswarm)
library(DBI)
library(data.table)
library(stringr)
source("functions.R")


# database
database <- dbConnect(RSQLite::SQLite(), "Proteomics.db")
print("Connected to database")

# database vars
# choices are only loaded once per server rather than every client
TMT_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='TMT';"))
LFQ_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='LFQ';"))
phosphosite_choices <- dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='Phosphosite';")
phosphosite_choices <- as.data.frame(lapply(phosphosite_choices, function (x) {gsub("_.*$", "", x)}))
phosphosite_choices <- unique(phosphosite_choices)
mRNA_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='mRNA';"))

# local files
mutation_specific_genes <- read.table(file = "data/mutation_specific_genes.txt", sep="\t", skipNul=T, encoding="UTF-8", quote = "")
mutation_table <- read.table(file = "data/mutations.txt", header=T, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
genenames_corr <- read.table("data/correlation_genes.txt", stringsAsFactors=F, header=F, sep = "\t", skipNul=T, encoding="UTF-8", quote = "")

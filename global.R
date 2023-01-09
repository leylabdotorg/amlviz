library(shiny)
library(shinyjs)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggbeeswarm)
library(DBI)
source("functions.R")


# database
# loaded once per server rather than every client
database <- dbConnect(RSQLite::SQLite(), "master_shiny_app.db")
print("Connected to database")

query <- paste0("SELECT * FROM master_database;")
dataset <- dbGetQuery(database,query)

# Generate all available plots
available_plots <- new.env(hash=TRUE)
available_plots[["tcga_aml"]] <- c("Multiplot","FAB", "Cyto_risk", "Fusion","Mutations")
available_plots[["beat_aml"]] <- c("Multiplot","Fusion","Mutations")

# Generate all gene drop downs
geneList <- new.env(hash=TRUE)
for(i in dataset$Short_hand_code) {
  if(!file.exists(paste0("gene_list/", i, ".txt"))) {
    query <- geneQuery(factors = c("Gene"), table = i, unique = TRUE, sort = TRUE)
    write.table(dbGetQuery(database,query), paste0("gene_list/", i, ".txt"),sep="\t",row.names=FALSE,col.names=FALSE)
  }
  geneList[[i]] <- read.delim(paste0("gene_list/", i, ".txt"), header = FALSE, sep = "\t", dec = ".")
}

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

# Generate all available plots and gene dropdowns
available_plots <- new.env(hash=TRUE)
geneList <- new.env(hash=TRUE)

for(i in dataset$Short_hand_code) {
  # Generate gene dropdown if cache doesn't exist
  if(!file.exists(paste0("gene_list/", i, ".txt"))) {
    query <- geneQuery(factors = c("Gene"), table = i, unique = TRUE, sort = TRUE)
    write.table(dbGetQuery(database,query), paste0("gene_list/", i, ".txt"),sep="\t",row.names=FALSE,col.names=FALSE)
  }
  geneList[[i]] <- read.delim(paste0("gene_list/", i, ".txt"), header = FALSE, sep = "\t", dec = ".")
  available_plots[[i]] <- c("Multiplot")
}

# Add other factors to available plots
dbFields <- dbListFields(database, "master_clinical")
dbFields <- dbFields[-c(1,2,3,4)] # Removes UPN and other columns that can't be plotted
for(i in dbFields) {
  query <- paste("SELECT DISTINCT Short_hand_code FROM master_clinical WHERE", i, "IS NOT NULL")
  datasetPlots <- dbGetQuery(database, query)
  for(j in datasetPlots$Short_hand_code) {
    available_plots[[j]] <- append(available_plots[[j]], i)
  }
}

# Add Mutations option if dataset is in mutation table
query <- "SELECT DISTINCT Short_hand_code FROM master_mutation"
datasetMutations <- dbGetQuery(database, query)
for(i in datasetMutations$Short_hand_code) {
  available_plots[[i]] <- append(available_plots[[i]], "Mutations")
}

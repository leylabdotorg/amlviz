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
  query <- geneQuery(factors = c("Gene"), table = i, unique = TRUE, sort = TRUE)
  geneList[[i]] <- dbGetQuery(database,query)
}

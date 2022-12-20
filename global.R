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
# loaded once per server rather than every client
# proteomics <- dbConnect(RSQLite::SQLite(), "Proteomics.db")
database <- dbConnect(RSQLite::SQLite(), "master_shiny_app.db")
print("Connected to database")

query <- paste0("SELECT * FROM master_database;")
dataset <- dbGetQuery(database,query)

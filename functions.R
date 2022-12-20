geneQuery <- function(factors=c("UPN","Gene","Expression"), unique=FALSE, table="Genes", genes, subset=NULL) {
  #SELECT UPN,Gene,Value FROM Genes WHERE (Gene='DNMT3A' OR Gene='TP53') AND Type='TMT';

  factors <- paste(factors,collapse=",")

  if(unique) {
    query <- paste("SELECT DISTINCT", factors, "FROM", table, "WHERE (")
  }
  else {
    query <- paste("SELECT", factors, "FROM", table, "WHERE (")
  }

  genesToQuery <- paste("Gene='",genes,"'",sep="",collapse=" OR ")
  query <- paste(query,genesToQuery,")",sep="")
  if(is.null(subset)) {
    query <- paste0(query,";")
  }
  else {
    # TODO: Change Proteomics to subset
    query <- paste(query," AND Type='",subset,"';",sep="")
  }
  return(query)
}

# Function is most likely useless and can be condensed with geneQuery; however, I suspect that there may be some edge cases where it's necessary
clinicalQuery <- function(factors=c("UPN","Short_hand_code"), unique=FALSE,table="master_clinical",type=NULL, subtypes=NULL) {
  # SELECT * FROM TMT_Clinical WHERE (UPN="ND8" OR ...);

  factors <- paste(factors,collapse=",")
  if(unique) {
    query <- paste("SELECT DISTINCT", factors, "FROM", table)
  }
  else {
    query <- paste("SELECT", factors, "FROM", table)
  }

  if(is.null(subtypes)) {
    query <- paste0(query,";")
  }
  else {
    subtypesToQuery <- paste(type,"='",subtypes,"'",sep="",collapse=" OR ")
    query <- paste(query," WHERE (",subtypesToQuery,");",sep="")
  }
  return(query)
}

hideAllElements <- function() {
  shinyjs::hide(id = "genes")
  shinyjs::hide(id = "gene")
  shinyjs::hide(id = "subtype_options")
  shinyjs::hide(id = "mutation_status")
}

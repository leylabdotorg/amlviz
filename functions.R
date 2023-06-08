geneQuery <- function(factors=c("UPN","Gene","Expression"), table="Genes", genes=NULL, unique=FALSE, sort=FALSE) {
  query <- "SELECT"

  if(unique) {
    query <- paste(query, "DISTINCT")
  }

  factors <- paste(factors, collapse=",")
  query <- paste(query, factors, "FROM", table)

  if(!is.null(genes)) {
    genesToQuery <- paste0("Gene='",genes,"'",collapse=" OR ")
    query <- paste(query, "WHERE", genesToQuery)
  }

  if(sort) {
    query <- paste(query, "ORDER BY Gene")
  }

  return(query)
}

clinicalQuery <- function(factors=c("UPN"), table="clinical", dataset, type=NULL, subtypes=NULL, unique=FALSE, sort=FALSE) {
  query <- "SELECT"

  if(unique) {
    query <- "SELECT DISTINCT"
  }

  factors <- paste0("U.", factors)
  factors <- paste(factors, collapse=",")
  query <- paste(query, factors, "FROM", table, "U INNER JOIN mappings P on P.UPN = U.UPN WHERE P.Study_ID =", paste0("'", dataset, "'"))

  if(!is.null(subtypes)) {
    subtypesToQuery <- paste0("U.", type, "='",subtypes,"'",collapse=" OR ")
    query <- paste0(query, " AND (", subtypesToQuery,")")
  }

  # Sort query by specific type
  if(sort) {
    query <- paste0(query, " ORDER BY ", "U.", type)
  }

  return(query)
}

hideAllElements <- function() {
  shinyjs::hide(id = "genes")
  shinyjs::hide(id = "gene")
  shinyjs::hide(id = "subtype_options")
  shinyjs::hide(id = "mutation_status")
}



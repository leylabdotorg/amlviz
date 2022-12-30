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

clinicalQuery <- function(factors=c("UPN","Short_hand_code"), table="master_clinical", type=NULL, subtypes=NULL, dataset=NULL, unique=FALSE, sort=FALSE) {
  query <- "SELECT"

  if(unique) {
    # TODO: Rework to make more general. Currently just using arg for options
    # query <- paste(query, "DISTINCT")
    return(paste0("SELECT DISTINCT ", type, " FROM ", table," WHERE Short_hand_code='",dataset,"' ORDER BY ", type))
  }

  factors <- paste(factors, collapse=",")
  query <- paste(query, factors, "FROM", table)

  if(!is.null(type)) {
    subtypesToQuery <- paste0(type, "='",subtypes,"'",collapse=" OR ")
    query <- paste0(query, " WHERE (", subtypesToQuery,")")
  }
  else {
    # TODO: Same as above
    query <- return(paste0(query, " WHERE Short_hand_code='", dataset,"'"))
  }

  if(!is.null(dataset)) {
    query <- paste0(query, " AND Short_hand_code='", dataset,"'")
  }

  if(sort) {
    query <- paste(query, "ORDER BY", type)
  }

  return(query)
}

hideAllElements <- function() {
  shinyjs::hide(id = "genes")
  shinyjs::hide(id = "gene")
  shinyjs::hide(id = "subtype_options")
  shinyjs::hide(id = "mutation_status")
}

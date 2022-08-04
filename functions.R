geneQuery <- function(factors=c("UPN","Gene","Value"), table="Genes", genes, type) {
  #SELECT UPN,Gene,Value FROM Genes WHERE (Gene='DNMT3A' OR Gene='TP53') AND Type='TMT';
  factors <- paste(factors,collapse=",")
  query <- paste("SELECT", factors, "FROM", table, "WHERE (")
  genesToQuery <- paste("Gene='",genes,"'",sep="",collapse=" OR ")
  query <- paste(query,genesToQuery,")",sep="")
  # TODO: Maybe make type default to NULL in case there is no type
  query <- paste(query," AND Type='",type,"';",sep="")
  return(query)
}

# Function is most likely useless and can be condensed with geneQuery; however, I suspect that there may be some edge cases where it's necessary
clinicalQuery <- function(factors=c("UPN","Name","TCGA_ID","TCGA_Name"), table="Clinical", subtypes=NULL, type=NULL) {
  # SELECT * FROM TMT_Clinical WHERE (UPN="ND8" OR ...);
  factors <- paste(factors,collapse=",")
  query <- paste("SELECT", factors, "FROM", table)
  if(is.null(subtypes)){
    query <- paste(query,";",sep="")
  }
  else
  {
    subtypesToQuery <- paste(type,"='",subtypes,"'",sep="",collapse=" OR ")
    query <- paste(query," WHERE (",subtypesToQuery,");",sep="")
  }
  return(query)
}

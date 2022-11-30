server <- function(input, output,session) {

  # Handle event when user selects dataset
  # Changes gene list and available types of plots based on the dataset
  observeEvent(input$dataset, {
    if(input$dataset != "") {
      # Update available plots based on datasets
      source(paste0("configs/",input$dataset,".R"), local = TRUE)
      updateSelectInput(session, "subtype",choices = c("",as.character(available_plots)), selected = NULL)

      # Update gene and genes
      query <- paste("SELECT DISTINCT Gene FROM", input$dataset, ";")
      geneChoices <- dbGetQuery(database, query) # TODO: Sort this
      updateSelectizeInput(session, "genes", choices = c("",as.character(geneChoices$Gene)),selected = NULL, server = TRUE)
      updateSelectizeInput(session, "gene", choices = c("",as.character(geneChoices$Gene)),selected = NULL, server = TRUE)

      # TODO: Remove Genes and Gene when a dataset is changed
    }
  })
}

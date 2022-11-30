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

  # Handle event when user selects subtype
  # Toggles several ui elements based on the dataset and subtype
  observeEvent(input$subtype, {
    if(input$subtype != "") {
      if(input$subtype != "Multiplot" && input$subtype != "Mutations") {
        query <- clinicalQuery(factors=c(input$subtype), unique=TRUE, type="Short_hand_code",subtypes=input$dataset)
        subtype_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
        subtype_choices <- str_sort(subtype_choices)
        updateCheckboxGroupInput(session,
                                 "subtype_options",
                                 label = "Subtypes",
                                 choices = subtype_choices,
                                 selected = subtype_choices)
      }

      else if(input$subtype == "Mutations") {

      }

    }
  })
}

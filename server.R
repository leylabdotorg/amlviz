server <- function(input, output,session) {

  # Handle event when user selects dataset
  # Changes gene list and available types of plots based on the dataset
  observeEvent(input$dataset, {
    # TODO: Add update subtype when we get other subtypes

    # Update subtype options
    if(input$subtype != "Multiplot" && input$subtype != "Mutation") {
      query <- clinicalQuery(factors=c(input$subtype), unique=TRUE, type="Short_hand_code",subtypes=input$dataset)
      subtype_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
      subtype_choices <- str_sort(subtype_choices)
      # TODO: Remove empty option
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Subtypes",
                               choices = subtype_choices,
                               selected = subtype_choices)
    }

    # Update gene and genes
    query <- paste("SELECT DISTINCT Gene FROM", input$dataset, ";")
    geneChoices <- unique(dbGetQuery(database, query)) # TODO: Sort this
    updateSelectizeInput(session, "genes", choices = geneChoices$Gene, server = TRUE)
    updateSelectizeInput(session, "gene", choices = geneChoices$Gene, server = TRUE)
  })

  # Handle event when user selects subtype
  # Toggles several ui elements based on the dataset and subtype
  observeEvent(input$subtype, {
    if(input$subtype != "Multiplot" && input$subtype != "Mutation") {
      query <- clinicalQuery(factors=c(input$subtype), unique=TRUE, type="Short_hand_code",subtypes=input$dataset)
      subtype_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
      subtype_choices <- str_sort(subtype_choices)
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Subtypes",
                               choices = subtype_choices,
                               selected = subtype_choices)
    }
    if(input$subtype == "Mutation") {
      query <- clinicalQuery(c("Mutation"),unique=TRUE,table="master_mutation",type="Short_hand_code", subtypes=input$dataset)

      mutation_choices <- dbGetQuery(database,query)
      updateSelectInput(session, "mutation_status", label = NULL, choices = mutation_choices)

      query <- paste0("SELECT DISTINCT Gene FROM master_mutation WHERE Short_hand_code='",input$dataset,"';")
      print(query)
      geneChoices <- unique(dbGetQuery(database, query)) # TODO: Sort this
      updateSelectizeInput(session, "gene", choices = geneChoices$Gene, server = TRUE)
    }

  })

  # observeEvent(input$gene, {
  #   if(input$subtype == "Mutation") {
  #     # Update gene and genes
  #     query <- paste("SELECT DISTINCT Gene FROM", input$dataset, ";")
  #     geneChoices <- unique(dbGetQuery(database, query)) # TODO: Sort this
  #     updateSelectizeInput(session, "genes", choices = geneChoices$Gene, server = TRUE)
  #     updateSelectizeInput(session, "gene", choices = geneChoices$Gene, server = TRUE)
  #
  #   }
  #
  # })

  # Handles output for plot
  output$plot <- renderPlotly({
    plotReady <- FALSE

    # Multiplot
    if(input$subtype == "Multiplot" && length(input$genes) > 0) {
      query <- geneQuery(genes = input$genes, table = input$dataset)

      tcga <- dbGetQuery(database,query)

      query <- clinicalQuery(factors = "*")

      clinical <- dbGetQuery(database,query)
      clinical <- merge(tcga, clinical, by="UPN")

      g <- ggplot(clinical,aes(fill=Gene, y=Expression, x=UPN, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", Short_hand_code))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"),
              axis.title=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Multiple protein view") +
        ylab("Log2 Expression") + xlab("")

      plotReady <- TRUE
    }

    # Subtype
    else if(length(input$subtype_options) > 0) {
      query <- geneQuery(genes = input$gene, table = input$dataset)

      tcga <- dbGetQuery(database,query)
      query <- clinicalQuery(factors="*",
                             subtypes=input$subtype_options,
                             type=input$subtype)

      clinical <- dbGetQuery(database,query)

      clinical <- merge(clinical,tcga, by="UPN")

      g <- ggplot(clinical,aes(eval(as.name(input$subtype)), Expression, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", Short_hand_code))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),
              axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")
      plotReady <- TRUE
    }

    else if(input$subtype == "Mutation") {
      query <- geneQuery(genes = input$gene, table = input$dataset)
      tcga <- dbGetQuery(database,query)

      query <- paste0("SELECT UPN,Mutation,Mutation_type FROM master_mutation WHERE Short_hand_code='",input$dataset,"' AND Gene='",input$gene,"' AND Mutation='",input$mutation_status,"';")
      print(query)
      clinical <- dbGetQuery(database,query)
      clinical <- merge(clinical,tcga, by="UPN")

      g <- ggplot(clinical,aes(Mutation, Expression, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", Short_hand_code))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),
              axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")
      plotReady <- TRUE
    }

    # Plotting
    if(plotReady) {
      ggplotly(g, tooltip="text")
    }

  })
}

server <- function(input, output,session) {
  # Handle event when user selects dataset
  # Changes gene list and available types of plots based on the dataset
  # TODO: Change db to have list of available plots
  observeEvent(input$dataset, {
    # TODO: Add update subtype when we get other subtypes

    # Update subtype options
    if(input$subtype != "Multiplot" && input$subtype != "Mutations") {
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
    print(query)
    geneChoices <- unique(dbGetQuery(database, query)) # TODO: Sort this
    updateSelectizeInput(session, "genes", choices = geneChoices$Gene, server = TRUE)
    updateSelectizeInput(session, "gene", choices = geneChoices$Gene, server = TRUE)
    itemPlot <<- "Gene"
  })

  # Handle event when user selects subtype
  # Toggles several ui elements based on the dataset and subtype
  observeEvent(input$subtype, {
    if(input$subtype == "Multiplot") {
      updateSelectizeInput(session, "genes", label = paste("Multi",tolower(itemPlot)," plot",sep=""))
    }

    else if(input$subtype == "Mutations") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
    }

    else {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      query <- clinicalQuery(factors=c(input$subtype), unique=TRUE, type="Short_hand_code",subtypes=input$dataset)
      print(input$subtype)
      print(query)
      subtype_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
      subtype_choices <- str_sort(subtype_choices)
      # TODO: Remove empty option
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Subtypes",
                               choices = subtype_choices,
                               selected = subtype_choices)
    }

  })

  # Handles output for plot
  output$plot <- renderPlotly({
    plotReady <- FALSE


    # Multiplot
    if(input$subtype == "Multiplot" && length(input$genes) > 0) {
      query <- geneQuery(genes = input$genes, table = input$dataset)
      print(query)
      tcga <- dbGetQuery(database,query)

      query <- clinicalQuery(factors = "*")
      print(query)
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
    else if(input$subtype == "FAB" && length(input$subtype_options) > 0) {
      query <- geneQuery(genes = input$gene, table = input$dataset)

      tcga <- dbGetQuery(database,query)
      query <- clinicalQuery(factors="*",
                             subtypes=input$subtype_options,
                             type="FAB")
      clinical <- dbGetQuery(database,query)

      clinical <- merge(clinical,tcga, by="UPN")
      clinical$Gene <- input$gene

      g <- ggplot(clinical,aes(FAB, Expression, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", Short_hand_code))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),
              axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")
      plotReady <- TRUE
    }

    # Cytogenetics

    # Fusion

    # Mutations


    # Plotting
    if(plotReady) {
      ggplotly(g, tooltip="text")
    }

  })
}

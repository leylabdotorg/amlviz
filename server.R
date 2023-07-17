server <- function(input, output, session) {
  # Handle event when user selects dataset
  # Changes gene list and available types of plots based on the dataset
  observeEvent(input$dataset, {
    if(input$dataset != "") {
      shinyjs::show(id = "subtype") # Show subtype when dataset is selected
      hideAllElements() # Hide old options when new dataset is selected

      # Update available plots based on datasets
      updateSelectizeInput(session = session, inputId = "subtype",choices = c("", available_plots[[input$dataset]]), selected = "", server = TRUE)

      # Update gene and genes
      updateSelectizeInput(session = session, inputId = "genes", choices = geneList[[input$dataset]]$V1, selected = NULL, server = TRUE)
      updateSelectizeInput(session = session, inputId = "gene", choices = geneList[[input$dataset]]$V1, selected = NULL, server = TRUE)
    }
  })

  # Handle event when user selects subtype
  # Toggles several ui elements based on the dataset and subtype
  observeEvent(input$subtype, {
    hideAllElements()
    if (input$dataset != ""){
      if(input$subtype == "Multiplot") {
        shinyjs::show(id = "genes")
        # Render ui option
        output$toggle_options <- renderUI({
          tagList(
            tags$b("Plotting options:"),
            checkboxInput(inputId = "toggle_log",
                          label = "Log2 scale y-axis",
                          value = TRUE)
          )
        })
      }
      else {
        if(input$subtype == "Mutations") {
          shinyjs::show(id = "gene")
          shinyjs::show(id = "mutation_status")

          query <- clinicalQuery(factors = "Gene", table = "mutation", dataset = input$dataset, type = "Gene", unique = TRUE, sort = TRUE)
          mutation_choices <- dbGetQuery(database, query)
          updateSelectizeInput(session, "mutation_status", choices = mutation_choices$Gene, selected = NULL, server = TRUE)
        }
        else if(input$subtype != "Mutations" && input$subtype != "") {
          shinyjs::show(id = "gene")
          shinyjs::show(id = "subtype_options")
          query <- clinicalQuery(factors=c(input$subtype), type=input$subtype, dataset=input$dataset, unique=TRUE, sort=TRUE)
          subtype_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
          updateCheckboxGroupInput(session,
                                   "subtype_options",
                                   label = "Subtypes",
                                   choices = subtype_choices,
                                   selected = subtype_choices)
        }
        # render median-line checkbox + raw-value checkbox
        output$toggle_options <- renderUI({
          tagList(
            tags$b("Plotting options:"),
            checkboxInput(inputId = "toggle_median",
                          label = "Show Median Line",
                          value = TRUE),
            checkboxInput(inputId = "toggle_log",
                          label = "Log2 scale y-axis",
                          value = TRUE),
            checkboxInput(inputId = "toggle_boxplot",
                          label = "Toggle boxplot",
                          value = FALSE)
          )
        })
      }
    }
  })

  # Toggle interactive median line
  show.median <- reactiveVal(TRUE)
  observeEvent(input$toggle_median, {
    show.median(input$toggle_median)
  })

  # Toggle interactive log2
  show.log2 <- reactiveVal(TRUE)
  observeEvent(input$toggle_log, {
    show.log2(input$toggle_log)
  })

  # Toggle interactive boxplot
  show.boxplot <- reactiveVal(FALSE)
  observeEvent(input$toggle_boxplot, {
    show.boxplot(input$toggle_boxplot)
  })


  # Initialize reactive values for Multiplot to store past queried genes and their expression
  prev_genes <- reactiveValues(genes = character(0),
                               expression = data.frame())

  # Initialize reactive values for Fusion/FAB/Cyto_risk to store past queried subtype options and their expression
  prev_subtypes <- reactiveValues(subtype_options = character(0),
                                  expression = data.frame())

  # Initialize reactive values for Mutation to store past queried gene
  prev_mutation_gene <- reactive({
    query <- geneQuery(genes = input$gene,
                       table = input$dataset)
    return(dbGetQuery(database, query))
  })

  # Initialize reactive clinical data
  clinicalData <- reactive({
    if (input$subtype == "Multiplot") {
      query <- clinicalQuery(factors = "*",
                             dataset = input$dataset)
    }
    else if (input$subtype != "Multiplot" && input$subtype != "Mutations") {
      query <- geneQuery(genes = input$gene,
                         table = input$dataset)
    }
    else if (input$subtype == "Mutations") {
      query <- clinicalQuery(factors = "*",
                             table = "clinical",
                             dataset = input$dataset)
    }
    return(dbGetQuery(database, query))
  })
  # Interactive query data
  queryData <- reactive({
    # default value
    clinical <- clinicalData()

    if (input$subtype == "Multiplot") {
      # Determine the new genes to be queried
      new_genes <- setdiff(input$genes, prev_genes$genes)
      # If there are any new genes, query the database for them
      if (length(new_genes) > 0) {
        query <- geneQuery(genes = new_genes,
                           table = input$dataset)
        expression_new <- dbGetQuery(database,query)
        # Add the new expression to the previously queried expression
        prev_genes$expression <- rbind(prev_genes$expression, expression_new)
        # Update the list of previously queried genes
        prev_genes$genes <- c(prev_genes$genes, new_genes)
      }
      # Determine any genes that have been removed from input$genes,
      removed_genes <- setdiff(prev_genes$genes, input$genes)
      if (length(removed_genes) > 0) {
        # if some genes are unselecteed, remove their expression from prev_genes$expression
        # and the list of previously queried genes: prev_genes$genes
        prev_genes$expression <- prev_genes$expression[!prev_genes$expression$Gene %in% removed_genes, ]
        prev_genes$genes <- setdiff(prev_genes$genes, removed_genes)
      }

      # Make sure expression only contains latest selected genes
      expression <- prev_genes$expression[prev_genes$expression$Gene %in% input$genes,]
      clinical <- merge(expression, clinical, by="UPN")
    }
    else if (input$subtype != "Multiplot" && input$subtype != "Mutations") {
      # Determine the new subtype options to be queried
      new_subtype_options <- setdiff(input$subtype_options, prev_subtypes$subtype_options)

      # If there are any new subtype options, query the database for them
      if (length(new_subtype_options) > 0) {
        query <- clinicalQuery(factors= "*",
                               type=input$subtype,
                               subtypes=new_subtype_options,
                               dataset=input$dataset)
        clinical_new <- dbGetQuery(database,query)
        # Add the new expression to the previously queried expression
        prev_subtypes$expression <- rbind(prev_subtypes$expression, clinical_new)
        # Update the list of previously queried subtype options
        prev_subtypes$subtype_options <- c(prev_subtypes$subtype_options, new_subtype_options)
      }

      # Determine any options that have been removed from input$subtype_options
      removed_subtype_options <- setdiff(prev_subtypes$subtype_options, input$subtype_options)
      if (length(removed_subtype_options) > 0) {
        # If there are any subtype options that have been removed from input$subtype_options,
        # remove their data from prev_subtypes$data and the list of previously queried subtype options
        prev_subtypes$expression <- prev_subtypes$expression[!prev_subtypes$expression[[input$subtype]] %in% removed_subtype_options, ]
        prev_subtypes$subtype_options <- setdiff(prev_subtypes$subtype_options, removed_subtype_options)
      }

      # Make sure expression only contains latest selected genes
      expression <- prev_subtypes$expression[prev_subtypes$expression[[input$subtype]] %in% input$subtype_options,]
      clinical <- merge(expression, clinical, by="UPN")
    }
    else if (input$subtype == "Mutations") {
      expression <- prev_mutation_gene()
      query <- clinicalQuery(factors = c("UPN", "Mutation", "Mutation_type"),
                             table = "mutation",
                             dataset = input$dataset,
                             type = "Gene",
                             subtypes = input$mutation_status,
                             unique = TRUE)
      upns_with_mut <- dbGetQuery(database,query)

      expression$Group <- paste(input$mutation_status, "WT")
      expression$Mutation <- paste(input$mutation_status,"WT")
      expression$Mutation_type <- NA
      for(i in upns_with_mut$UPN) {
        expression$Group[expression$UPN == i] <- paste(input$mutation_status, "MT")
        expression$Mutation[expression$UPN == i] <- upns_with_mut$Mutation[upns_with_mut$UPN == i]
        expression$Mutation_type[expression$UPN == i] <- upns_with_mut$Mutation_type[upns_with_mut$UPN == i]
      }
      expression$Gene <- input$gene

      clinical <- merge(expression, clinical, by = "UPN")
      clinical$Group <- factor(clinical$Group, levels = c(paste(input$mutation_status, "WT"), paste(input$mutation_status, "MT")))
    }

    # Return clinical data
    clinical
  })


  # Handles output for plot
  output$plot <- renderPlotly({
    plotReady <- FALSE
    # Multiplot
    if(input$subtype == "Multiplot" && length(input$genes) > 0) {
      clinical <- queryData()
      # Conditionally define the 'y' aesthetic and y-axis label
      if (show.log2()) {
        y_aes <- aes(y = Expression)
        y_label <- "Log2 Expression"
        plot_title <- paste0("Log2 Expression for Multiple gene view")
      } else {
        y_aes <- aes(y = 2^Expression)
        y_label <- "Expression"
        plot_title <- paste0("Expression for Multiple gene view")

      }
      # Define the common parts of the plot
      g <- ggplot(clinical, aes(fill=Gene, x=UPN,
                                text=paste0("UPN ID: ", UPN, "<br />Dataset: ", input$dataset))) +
        y_aes +
        geom_bar(position="dodge", stat="identity")

      # Set the plot ready flag
      plotReady <- TRUE
    }

    # Subtype
    else if(length(input$subtype_options) > 0 && input$subtype != "" && input$gene != "" && input$subtype != "Multiplot" && input$subtype != "Mutations") {
      clinical <- queryData()
      # Conditionally define the 'y' aesthetic and y-axis label
      if (show.log2()) {
        y_aes <- aes(y = Expression)
        y_label <- "Log2 Expression"
        plot_title <- paste0("Log2 Expression for ", input$gene)
      } else {
        y_aes <- aes(y = 2^Expression)
        y_label <- "Expression"
        plot_title <- paste0("Expression for ", input$gene)
      }

      # Define the common parts of the plot
      g <- ggplot(clinical, aes(eval(as.name(input$subtype)), text=paste0("UPN ID: ", UPN, "<br />Dataset: ", input$dataset))) +
        y_aes +
        geom_quasirandom(size=0.8)

      # Set the plot ready flag
      plotReady <- TRUE
    }

    # Mutations
    else if(input$subtype == "Mutations" && input$gene != "" && input$mutation_status != "") {
      clinical <- queryData()
      # Conditionally define the 'y' aesthetic and y-axis label
      if (show.log2()) {
        y_aes <- aes(y = Expression)
        y_label <- "Log2 Expression"
        plot_title <- paste0("Log2 Expression for ", input$gene, " with ", input$mutation_status, ": WT|MT")
      } else {
        y_aes <- aes(y = 2^Expression)
        y_label <- "Expression"
        plot_title <- paste0("Expression for ", input$gene, " with ", input$mutation_status, ": WT|MT")
      }

      # Define the common parts of the plot
      g <- ggplot(clinical, aes(Group, text = paste0("UPN ID: ", UPN, "<br />Mutation: ", Mutation, "<br />Mutation type: ", Mutation_type))) +
        y_aes +
        geom_quasirandom(size = 0.8)
        # geom_violin() +

      # Set the plot ready flag
      plotReady <- TRUE
    }

    # Plotting
    if(plotReady) {
      # Final configuration
      g <- g + theme_bw() +
        ggtitle(plot_title) +
        theme(
          text = element_text(size=12, family="avenir", face="bold"),
          axis.title = element_text(size=12, family="avenir", face="bold"),
          axis.text = element_text(size=12, family="avenir", face="bold"),
          axis.text.x = element_text(angle=45, hjust=1)
        ) +
        ylab(y_label) + xlab("")

      if(show.median()) {
        g <- g + stat_summary(fun="median", geom="errorbar", color="red", aes(group=1))
      }
      if (show.boxplot()){
        g <- g + geom_boxplot()
      }
      # View(clinical)
      ggplotly(g, tooltip="text")
    }
  })
}




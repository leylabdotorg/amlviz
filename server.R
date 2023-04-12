server <- function(input, output,session) {
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
    if(input$subtype == "Multiplot") {
      shinyjs::show(id = "genes")
    }
    else if(input$subtype != "Multiplot" && input$subtype != "Mutations" && input$subtype != "") {
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
    else if(input$subtype == "Mutations") {
      shinyjs::show(id = "gene")
      shinyjs::show(id = "mutation_status")

      query <- clinicalQuery(factors = "Gene", table = "mutation", dataset = input$dataset, type = "Gene", unique = TRUE, sort = TRUE)
      mutation_choices <- dbGetQuery(database, query)
      updateSelectizeInput(session, "mutation_status", choices = mutation_choices$Gene, selected = NULL, server = TRUE)
    }
  })

  # Handles output for plot
  output$plot <- renderPlotly({
    plotReady <- FALSE
    # Multiplot
    if(input$subtype == "Multiplot" && length(input$genes) > 0) {
      query <- geneQuery(genes = input$genes, table = input$dataset)
      tcga <- dbGetQuery(database,query)

      query <- clinicalQuery(factors = "*", dataset = input$dataset)
      clinical <- dbGetQuery(database,query)
      clinical <- merge(tcga, clinical, by="UPN")

      g <- ggplot(clinical,aes(fill=Gene, y=Expression, x=UPN, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", input$dataset))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"),
              axis.title=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Multiple gene view") +
        ylab("Log2 Expression") + xlab("")
      plotReady <- TRUE
    }

    # Subtype
    else if(length(input$subtype_options) > 0 && input$subtype != "" && input$gene != "" && input$subtype != "Multiplot" && input$subtype != "Mutations") {
      query <- geneQuery(genes = input$gene, table = input$dataset)
      tcga <- dbGetQuery(database,query)

      query <- clinicalQuery(factors="*",
                             type=input$subtype,
                             subtypes=input$subtype_options,
                             dataset=input$dataset)
      clinical <- dbGetQuery(database,query)
      clinical <- merge(tcga, clinical, by="UPN")


      g <- ggplot(clinical,aes(eval(as.name(input$subtype)), Expression, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", input$dataset))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),
              axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")
      plotReady <- TRUE
    }

    # Mutations
    else if(input$subtype == "Mutations" && input$gene != "" && input$mutation_status != "") {
      query <- geneQuery(genes = input$gene, table = input$dataset)
      tcga <- dbGetQuery(database,query)

      query <- clinicalQuery(factors = c("UPN", "Mutation", "Mutation_type"), table = "mutation", dataset = input$dataset, type = "Gene", subtypes = input$mutation_status, unique = TRUE)
      upns_with_mut <- dbGetQuery(database,query)

      tcga$Group <- paste(input$mutation_status, "WT")
      tcga$Mutation <- paste(input$mutation_status,"WT")
      tcga$Mutation_type <- NA
      for(i in upns_with_mut$UPN) {
        tcga$Group[tcga$UPN == i] <- paste(input$mutation_status, "MT")
        tcga$Mutation[tcga$UPN == i] <- upns_with_mut$Mutation[upns_with_mut$UPN == i]
        tcga$Mutation_type[tcga$UPN == i] <- upns_with_mut$Mutation_type[upns_with_mut$UPN == i]
      }

      tcga$Gene <- input$gene
      query <- clinicalQuery(factors = "*", table = "clinical", dataset = input$dataset)
      all_clinical <- dbGetQuery(database,query)
      clinical <- merge(tcga,all_clinical, by = "UPN")

      clinical$Group <- factor(clinical$Group, levels = c(paste(input$mutation_status, "WT"), paste(input$mutation_status, "MT")))

      g <- ggplot(clinical,aes(Group, Expression, text = paste0("UPN ID: ",UPN,"<br />Mutation: ",Mutation,"<br />Mutation type: ",Mutation_type))) +
        geom_quasirandom(size = 0.8) +
        theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene," with ",input$mutation_status, ": WT|MT")) +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")
      plotReady <- TRUE
    }

    # Plotting
    if(plotReady) {
      ggplotly(g, tooltip="text")
    }
  })
}

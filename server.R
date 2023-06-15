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

    # render median-line checkbox + raw-value checkbox
    if (input$dataset != "" && input$subtype != "Multiplot" && input$subtype != "") {
      output$toggle_options <- renderUI({
        tagList(
          tags$h4("Plotting options:"),
          checkboxInput(inputId = "toggle_median",
                        label = "Show Median Line",
                        value = TRUE),
          checkboxInput(inputId = "toggle_log",
                        label = "Log2 scale y-axis",
                        value = TRUE)
        )
      })
    } else if (input$subtype == "Multiplot") {
      output$toggle_options <- renderUI({
        tagList(
          tags$h4("Plotting options:"),
          checkboxInput(inputId = "toggle_log",
                        label = "Log2 scale y-axis",
                        value = TRUE)
        )
      })
    }
  })

  # Reactive value to store the state of the median line
  show.median <- reactiveVal(TRUE)
  # Handle button clicks
  observeEvent(input$toggle_median, {
    # Toggle the value of show.median
    show.median(input$toggle_median)
  })
  # Reactive value to store the state of the raw value
  show.log2 <- reactiveVal(TRUE)
  # Handle button clicks
  observeEvent(input$toggle_log, {
    # Toggle the value of show.log2
    show.log2(input$toggle_log)
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

      # Conditionally define the 'y' aesthetic and y-axis label
      if (show.log2()) {
        y_aes <- aes(y = Expression)
        y_label <- "Log2 Expression"
      } else {
        y_aes <- aes(y = 2^Expression)
        y_label <- "Expression"
      }
      # Define the common parts of the plot
      g <- ggplot(clinical, aes(fill=Gene, x=UPN,
                                text=paste0("UPN ID: ", UPN, "<br />Dataset: ", input$dataset))) +
        y_aes +
        geom_bar(position="dodge", stat="identity") + theme_bw() +
        theme(
          text = element_text(size=12, family="avenir", face="bold"),
          axis.text = element_text(size=10, family="avenir", face="bold"),
          axis.title = element_text(size=12, family="avenir", face="bold"),
          axis.text.x = element_text(angle = 90, hjust = 1)
        ) +
        ggtitle("Multiple gene view") +
        ylab(y_label) + xlab("")

      # Set the plot ready flag
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
        geom_quasirandom(size=0.8) + theme_bw() +
        ggtitle(plot_title) +
        theme(
          text = element_text(size=12, family="avenir", face="bold"),
          axis.text = element_text(size=12, family="avenir", face="bold"),
          axis.text.x = element_text(angle=45, hjust=1)
        ) +
        ylab(y_label) + xlab("")

      # Set the plot ready flag
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
        geom_quasirandom(size = 0.8) +
        theme_bw() +
        ggtitle(plot_title) +
        theme(
          text = element_text(size=12, family="avenir", face="bold"),
          axis.text = element_text(size=12, family="avenir", face="bold"),
          axis.text.x = element_text(angle=45, hjust=1)
        ) +
        ylab(y_label) + xlab("")
      # Set the plot ready flag
      plotReady <- TRUE
    }

    # Plotting
    if(plotReady) {
      # check if we need to add median line
      if(show.median()) {
        g <- g + stat_summary(fun="median", geom="errorbar", color="red", aes(group=1))
      }
      ggplotly(g, tooltip="text")
    }
  })
}

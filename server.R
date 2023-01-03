server <- function(input, output,session) {
  # Handle event when user selects dataset
  # Changes gene list and available types of plots based on the dataset
  observeEvent(input$dataset, {
    if(input$dataset != "") {
      shinyjs::show(id = "subtype") # Show subtype when dataset is selected
      hideAllElements() # Hide old options when new dataset is selected

      # Update available plots based on datasets
      updateSelectInput(session = session, inputId = "subtype",choices = c('', available_plots[[input$dataset]]), selected = "")

      # Update gene and genes
      updateSelectizeInput(session = session, inputId = "genes", choices = c("", geneList[[input$dataset]]$Gene), selected = "", server = TRUE)
      updateSelectizeInput(session = session, inputId = "gene", choices = c("", geneList[[input$dataset]]$Gene), selected = "", server = TRUE)
    }
  })

  # Handle event when user selects subtype
  # Toggles several ui elements based on the dataset and subtype
  observeEvent(input$subtype, {
    if(input$subtype != "") {
      hideAllElements()

      if(input$subtype != "Multiplot" && input$subtype != "Mutations") {
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
      else if(input$subtype == "Multiplot") {
        shinyjs::show(id = "genes")
      }

      else if(input$subtype == "Mutations") {
        shinyjs::show(id = "gene")
        shinyjs::show(id = "mutation_status")

        query <- paste0("SELECT DISTINCT Mutation FROM master_mutation WHERE Short_hand_code ='",input$dataset, "';")
        mutation_choices <- dbGetQuery(database, query)
        print(mutation_choices)
        updateSelectizeInput(session, "mutation_status", choices = c("",as.character(mutation_choices$Mutation)),selected = NULL, server = TRUE)
      }
    }
  })

  # Handles output for plot
  output$plot <- renderPlotly({
    plotReady <- FALSE
    if(length(input$genes) > 1 || input$gene != "") {
      query <- geneQuery(genes = input$genes, table = input$dataset)
      tcga <- dbGetQuery(database,query)

      # Multiplot
      if(input$subtype == "Multiplot" && length(input$genes) > 0) {
        query <- geneQuery(genes = input$genes, table = input$dataset)
        tcga <- dbGetQuery(database,query)

        query <- clinicalQuery(factors = "*", dataset = input$dataset)
        print(query)
        clinical <- dbGetQuery(database,query)
        clinical <- merge(tcga, clinical, by="UPN")

        g <- ggplot(clinical,aes(fill=Gene, y=Expression, x=UPN, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", Short_hand_code))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
          theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"),
                axis.title=element_text(size=12, family="avenir", face="bold"),
                axis.text.x = element_text(angle = 90, hjust = 1)) +
          ggtitle("Multiple gene view") +
          ylab("Log2 Expression") + xlab("")
        plotReady <- TRUE
      }

      # Subtype
      else if(length(input$subtype_options) > 0 && input$gene != "") {
        query <- geneQuery(genes = input$gene, table = input$dataset)
        tcga <- dbGetQuery(database,query)

        query <- clinicalQuery(factors="*",
                               type=input$subtype,
                               subtypes=input$subtype_options,
                               dataset=input$dataset)
        clinical <- dbGetQuery(database,query)
        clinical <- merge(tcga, clinical, by="UPN")


        g <- ggplot(clinical,aes(eval(as.name(input$subtype)), Expression, text = paste0("UPN ID: ",UPN,"<br />Dataset: ", Short_hand_code))) + geom_quasirandom(size = 0.8) + theme_bw() +
          ggtitle(paste0("Log2 Expression for ",input$gene)) +
          theme(text=element_text(size=12, family="avenir", face="bold"),
                axis.text=element_text(size=12, family="avenir", face="bold"),
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          ylab("Log2 Expression") + xlab("")
        plotReady <- TRUE
      }

      # Mutations
      else if(input$subtype == "Mutations") {
        query <- geneQuery(genes = input$gene, table = input$dataset)
        tcga <- dbGetQuery(database,query)
        tcga$Group <- "WT"

        query <- paste0("SELECT DISTINCT UPN FROM master_mutation WHERE Short_hand_code='",input$dataset,"' AND Mutation='",input$mutation_status,"' AND Gene='",input$gene,"';")
        print(query)
        upns_with_mut <- dbGetQuery(database,query)
        #upns_with_mut <- as.character(mutation_table$UPN[which(mutation_table$Gene %in% input$mutation_status)])

        query <- paste0("SELECT * FROM master_clinical WHERE Short_hand_code='",input$dataset,"';")
        all_clinical <- dbGetQuery(database,query)

        tcga$Group[tcga$UPN %in% upns_with_mut] <- input$mutation_status

        tcga$Mutation <- paste(input$mutation_status,"WT")
        query <- paste0("SELECT UPN,Mutation,Mutation_type FROM master_mutation WHERE Short_hand_code ='",input$dataset, "';")
        mutations <- dbGetQuery(database,query)
        tcga$Mutation[tcga$UPN %in% upns_with_mut] <- mutations$Mutation_type[tcga$UPN %in% upns_with_mut]
        tcga$Gene <- input$gene

        clinical <- merge(tcga,all_clinical, by = "UPN")

        # Todo Readd factors
        print(clinical$Group)

        g <- ggplot(clinical,aes(Group, Expression, text = paste0("UPN ID: ",UPN,"<br />Mutation type: ", Mutation))) +
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
    }

  })
}

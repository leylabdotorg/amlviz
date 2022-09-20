server <- function(input, output,session) {
  observeEvent(input$dataset, {
    # Handle event when user selects dataset
    # Changes gene list and available types of plots based on the dataset

    if(input$dataset != "Proteomics") {
      print(input$dataset)
      subtype2 <- c("Multiplot","Subtype", "Cytogenetics", "Fusion", "Mutations")
      updateSelectizeInput(session, 'subtype', choices = subtype2, server = TRUE)
      geneChoices <- unique(dbGetQuery(database, paste("SELECT Gene FROM", input$dataset,";"))) # TODO: Sort this
      updateSelectizeInput(session, "genes", choices = geneChoices$Gene, server = TRUE)
      updateSelectizeInput(session, "gene", choices = geneChoices$Gene, server = TRUE)
      itemPlot <<- "Gene"
    }
  })


  # Handle event when user selects subset
  # Changes gene list and available types of plots based on the dataset
  observeEvent(input$subset, {
    itemPlot <<- "Protein"
    if(input$subset == "TMT") {
      updateSelectizeInput(session, "genes", choices = TMT_choices$Gene, server = TRUE)
      updateSelectizeInput(session, "gene", choices = TMT_choices$Gene, server = TRUE)
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusion", "Mutations")
      mutationLevels <<- c("WT", "Healthy Lin-")
    }
    else if(input$subset == "LFQ") {
      updateSelectizeInput(session, "genes", choices = LFQ_choices$Gene, server = TRUE)
      updateSelectizeInput(session, "gene", choices = LFQ_choices$Gene, server = TRUE)
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusion", "Mutations")
      mutationLevels <<- c("WT","Healthy CD34", "Healthy Lin-")
    }
    else if(input$subset == "Phosphosite") {
      updateSelectizeInput(session, "gene", choices = phosphosite_choices$Gene, server = TRUE)
      subtype <- c("Subtype", "Cytogenetics", "Fusion", "Mutations")
      mutationLevels <<- c("WT", "Healthy Lin-")
    }
    else if(input$subset == "mRNA") {
      updateSelectizeInput(session, "genes", choices = mRNA_choices$Gene, server = TRUE)
      updateSelectizeInput(session, "gene", choices = mRNA_choices$Gene, server = TRUE)
      itemPlot <<- "Gene"
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusion", "Mutations")
      mutationLevels <<- c("WT","Healthy Donor CD19", "Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
    }
    else if(input$subset == "Protein vs mRNAs") {
      updateSelectizeInput(session, "genes", label = "Enter Gene/Protein to plot", choices = genenames_corr$V1, server = TRUE)
      subtype <- NULL
    }

    updateSelectizeInput(session, 'subtype', choices = subtype, server = TRUE)
  })

  # Handle event when user selects subtype
  # Toggles several ui elements based on the dataset and subtype
  observeEvent(input$subtype, {
    if(input$subtype == "Multiplot") {
      updateSelectizeInput(session, "genes", label = paste("Multi",tolower(itemPlot)," plot",sep=""))
    }
    else if(input$subtype == "Subtype") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      query <- paste0("SELECT DISTINCT FAB FROM master_clinical;")
      subtype_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
      subtype_choices <- str_sort(subtype_choices)
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Subtypes",
                               choices = subtype_choices,
                               selected = subtype_choices)
    }
    else if(input$subtype == "Cytogenetics") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      query <- paste0("SELECT DISTINCT Cyto_Risk FROM master_clinical;")
      cyto_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
      cyto_choices <- str_sort(cyto_choices)
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Criteria",
                               choices = cyto_choices,
                               selected = cyto_choices)
    }
    else if(input$subtype == "Fusion") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      query <- paste0("SELECT DISTINCT Fusion FROM master_clinical;")
      fusion_choices <- unlist(dbGetQuery(database,query),use.names = FALSE)
      fusion_choices <- str_sort(fusion_choices)
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Criteria",
                               choices = fusion_choices,
                               selected = fusion_choices)
    }
    else if(input$subtype == "Mutations") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
    }
  })

  # Handles output for plot
  output$plot <- renderPlotly({
    plotReady <- FALSE
    # Multiplot
    if(input$subtype == "Multiplot" && length(input$genes) > 0) {
      query <- geneQuery(genes = input$genes,subset = input$subset)
      tcga <- dbGetQuery(database,query)

      query <- clinicalQuery(factors="*", table=paste(input$subset,"_Clinical",sep=""))
      clinical <- dbGetQuery(database,query)

      clinical <- merge(tcga, clinical, by="UPN")

      g <- ggplot(clinical,aes(fill=Gene, y=Value, x=Name, text = paste0("UPN ID: ",UPN,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"),
              axis.title=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Multiple protein view") +
        ylab("Log2 Expression") + xlab("")


      plotReady <- TRUE
    }
    # Subtype
    else if(input$subtype == "Subtype" && length(input$subtype_options) > 0) {
      if(input$subset == "Phosphosite") {
        query <- paste("SELECT UPN,Gene,Value FROM Genes WHERE Gene GLOB '",input$gene,"*' AND Type='Phosphosite';",sep="")
      }
      else {
        query <- geneQuery(genes = input$gene, subset = input$subset)
      }
      tcga <- dbGetQuery(database,query)
      query <- clinicalQuery(factors=c("UPN","Name","TCGA_ID","TCGA_Name","FAB"),
                             table=paste(input$subset,"_Clinical",sep=""),
                             subtypes=input$subtype_options,
                             type="FAB")
      clinical <- dbGetQuery(database,query)

      clinical <- merge(clinical,tcga, by="UPN")
      facet <- NULL
      if(input$subset == "Phosphosite") {
        setnames(clinical, "Gene", "Phosphosite")
        facet <- facet_wrap(~ Phosphosite)
      }
      clinical$Gene <- input$gene

      g <- ggplot(clinical,aes(FAB, Value, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),
              axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")

      g <- g + facet
      plotReady <- TRUE
    }
    # Cytogenetics
    else if(input$subtype == "Cytogenetics" && length(input$subtype_options) > 0) {
      if(input$subset == "Phosphosite") {
        query <- paste("SELECT UPN,Gene,Value FROM Genes WHERE Gene GLOB '",input$gene,"*' AND Type='Phosphosite';",sep="")
      }
      else {
        query <- geneQuery(genes = input$gene, subset = input$subset)
      }
      tcga <- dbGetQuery(database,query)
      query <- clinicalQuery(factors=c("UPN","Name","TCGA_ID","TCGA_Name","Cyto_Risk"),
                             table=paste(input$subset,"_Clinical",sep=""),
                             subtypes=input$subtype_options,
                             type="Cyto_Risk")
      clinical <- dbGetQuery(database,query)
      clinical <- merge(clinical,tcga, by="UPN")

      facet <- NULL
      if(input$subset == "Phosphosite") {
        setnames(clinical, "Gene", "Phosphosite")
        facet <- facet_wrap(~ Phosphosite)
      }
      clinical$Gene <- input$gene

      g <- ggplot(clinical,aes(Cyto_Risk, Value, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")

      g <- g + facet
      plotReady <- TRUE
    }
    # Fusion
    else if(input$subtype == "Fusion" && length(input$subtype_options) > 0) {
      if(input$subset == "Phosphosite") {
        query <- paste("SELECT UPN,Gene,Value FROM Genes WHERE Gene GLOB '",input$gene,"*' AND Type='Phosphosite';",sep="")
      }
      else {
        query <- geneQuery(genes = input$gene, subset = input$subset)
      }
      tcga <- dbGetQuery(database,query)
      query <- clinicalQuery(factors=c("UPN","Name","TCGA_ID","TCGA_Name","Fusion"),
                             table=paste(input$subset,"_Clinical",sep=""),
                             subtypes=input$subtype_options,
                             type="Fusion")
      clinical <- dbGetQuery(database,query)
      clinical <- merge(clinical,tcga, by="UPN")

      facet <- NULL
      if(input$subset == "Phosphosite") {
        setnames(clinical, "Gene", "Phosphosite")
        facet <- facet_wrap(~ Phosphosite)
      }

      clinical$Gene <- input$gene

      g <- ggplot(clinical,aes(Fusion, Value, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),axis.text=element_text(size=12, family="avenir", face="bold"), axis.text.x = element_text(angle = 45)) +
        ylab("Log2 Expression") + xlab("")

      g <- g + facet
      plotReady <- TRUE
    }
    # Mutations
    else if(input$subtype == "Mutations") {
      query <- geneQuery(factors = c("UPN","Value"),genes = input$gene, subset = input$subset)
      tcga <- dbGetQuery(database,query)
      tcga$Group <- "WT"

      upns_with_mut <- as.character(mutation_table$UPN[which(mutation_table$Gene %in% input$mutation_status)])

      query <- clinicalQuery(factors = "*",
                             table = paste0(input$subset,"_Clinical"))
      all_clinical <- dbGetQuery(database,query)

      upns_with_health <- as.character(all_clinical$UPN[which(all_clinical$TCGA_Name %like% "Healthy")])

      tcga$Group[tcga$UPN %in% upns_with_mut] <- input$mutation_status
      tcga$Group[tcga$UPN %in% upns_with_health] <- all_clinical$FAB[tcga$UPN %in% upns_with_health]

      tcga$Mutation <- paste(input$mutation_status,"WT")
      tcga$Mutation[tcga$UPN %in% upns_with_mut] <- mutation_table$Mutation[tcga$UPN %in% upns_with_mut]
      tcga$Mutation[tcga$UPN %in% upns_with_health] <- all_clinical$FAB[tcga$UPN %in% upns_with_health]
      tcga$Gene <- input$gene

      clinical <- merge(tcga,all_clinical[,c("UPN","Name","TCGA_ID")], by = "UPN")
      clinical <- clinical[,c(1,6,7,3,4,2,5)]
      #TODO ADD Phosphosite

      factorLevels <- unique(clinical$Group)
      factorLevels <- factorLevels[! factorLevels %in% mutationLevels]
      factorLevels <- append(factorLevels,mutationLevels)
      clinical$Group <- factor(clinical$Group,levels=factorLevels)

      g <- ggplot(clinical,aes(Group, Value, text = paste0("UPN ID: ",Name,"<br />Mutation type: ", Mutation,"<br />TCGA Sample ID: ", TCGA_ID))) +
        geom_quasirandom(size = 0.8) +
        theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene," with ",input$mutation_status, ": WT|MT")) +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("")
      plotReady <- TRUE
    }
    # Protein vs mRNAS
    else if(input$subset == "Protein vs mRNAs") {
      query <- geneQuery(genes = input$genes, subset = input$subset)
      tcga <- dbGetQuery(database,query)
      print(input$genes)

    }

    # Plotting
    if(plotReady) {
      ggplotly(g, tooltip="text")
    }

  })
}

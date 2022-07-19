library(shiny)
library(shinyjs)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggbeeswarm)
library(DBI)
library(data.table)
library(hash)

source("functions.R")


# database
database <- dbConnect(RSQLite::SQLite(), "Proteomics.db")
print("Connected to database")

# database vars
# more effcient this way... choices are only loaded once per server rather than every client 
tmtChoices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='TMT';"))
lfqChoices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='LFQ';"))
phosphoChoices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='Phosphosite';"))
#phosphoChoices <- lapply(phosphoChoices, function (x) {gsub("_.*$", "", x)})
mrnaChoices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='mRNA';"))
# gene correlations file for vs

# local files
# May remove in favor of database
dropdowngenes <- read.table(file = "Data_files/mutation_specific_genes.txt", sep="\t", skipNul=T, encoding="UTF-8", quote = "")
mutation_table <- read.table(file = "Data_files/mutations.txt", header=T, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
genenames_corr = read.table("Data_files/correlation_genes.txt", stringsAsFactors=F, header=F, sep = "\t", skipNul=T, encoding="UTF-8", quote = "")


ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select dataset
      selectInput(inputId = "type",
                  label = "Select a dataset",
                  choices = c("TMT","LFQ","Phosphosite","mRNA","Protein vs mRNAs")),
      
      # Dropdown to select which plot you want based on dataset
      selectizeInput(inputId = "subtype",
                     label = "Select an option",
                     choices = NULL,
                     multiple = FALSE),
      
      # Dropdown to select genes
      selectizeInput(inputId = "genes",
                     label = NULL,
                     choices = NULL,
                     multiple = TRUE,
                     selected = NULL),
      
      # Dropdown to select gene
      selectizeInput(inputId = "gene",
                     label = NULL,
                     choices = NULL,
                     multiple = FALSE,
                     selected = NULL),
      
      # Checkbox to select subtype options
      checkboxGroupInput(inputId = "subtype_options",
                         label = NULL,
                         choices = NULL),
      
      # Dropdown specifically for mutations 
      selectInput(inputId = "mutation_status",
                  label = "Mutation Status",
                  choices = dropdowngenes$V1)
      
    ),
    mainPanel(
      plotlyOutput("plot",height = "500px")
    )
  )
)

server <- function(input, output,session) {
  # Handle event when user selects dataset
  observeEvent(input$type, {
    itemPlot <<- "Protein"
    if(input$type == "TMT") {
      updateSelectizeInput(session, "genes", choices = tmtChoices$Gene, server = TRUE)
      updateSelectizeInput(session, "gene", choices = tmtChoices$Gene, server = TRUE)
      
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusion", "Mutations")
      subtypeChoices <<- list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy Lin-"="Healthy Lin-")
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy Lin-")
      
      cytoChoices <<- list("Favorable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor", "Healthy Lin-" = "Healthy Lin-")
      cytoSelected <<- c("Good","Intermediate","Poor","Healthy Lin-")
      
      fusionChoices <<- list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy Donor Lin-" = "Healthy Lin-")
      fusionSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")
      
      mutationLevels <<- c("WT", "Healthy Lin-")
    }
    else if(input$type == "LFQ") {
      updateSelectizeInput(session, "genes", choices = lfqChoices$Gene, server = TRUE)
      updateSelectizeInput(session, "gene", choices = lfqChoices$Gene, server = TRUE)
      
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusion", "Mutations")
      
      subtypeChoices <<- list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy CD34"="Healthy CD34","Healthy Lin-"="Healthy Lin-")
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy CD34","Healthy Lin-")
      
      cytoChoices <<- list("Favourable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor","Healthy CD34" = "Healthy CD34" ,"Healthy Lin-" = "Healthy Lin-")
      cytoSelected <<- c("Good","Intermediate","Poor","Healthy CD34","Healthy Lin-")
      
      fusionChoices <<- list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy CD34"="Healthy CD34","Healthy Donor Lin-" = "Healthy Lin-")
      fusionSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy CD34","Healthy Lin-")
      
      mutationLevels <<- c("WT","Healthy CD34", "Healthy Lin-")
    }
    else if(input$type == "Phosphosite") {
      # TODO: Fix when you change the database
      updateSelectizeInput(session, "gene", choices = phosphoChoices$Gene, server = TRUE)
      
      subtype <- c("Subtype", "Cytogenetics", "Fusion", "Mutations")
      
      subtypeChoices <<- list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy Lin-"="Healthy Lin-")
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy Lin-")
      
      cytoChoices <<- list("Favorable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor", "Healthy Lin-" = "Healthy Lin-")
      cytoSelected <<- c("Good","Intermediate","Poor","Healthy Lin-")
      
      fusionChoices <<- list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy Donor Lin-" = "Healthy Lin-")
      fusionSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")
      
      mutationLevels <<- c("WT", "Healthy Lin-")
    }
    else if(input$type == "mRNA") {
      updateSelectizeInput(session, "genes", choices = mrnaChoices$Gene, server = TRUE)
      updateSelectizeInput(session, "gene", choices = mrnaChoices$Gene, server = TRUE)
      
      
      itemPlot <<- "Gene"
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusion", "Mutations")
      
      subtypeChoices <<- list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro")
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
      
      cytoChoices <<- list("Favourable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor", "Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro")
      cytoSelected <<- c("Good","Intermediate","Poor","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
      
      fusionChoices <<- list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro")
      fusionSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
      
      mutationLevels <<- c("WT","Healthy Donor CD19", "Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
    }
    else if(input$type == "Protein vs mRNAs") {
      shinyjs::hide(id = "subtype")
      subtype <- NULL
      updateSelectizeInput(session, "gene", label = "Enter Gene/Protein to plot")
    }
    
    updateSelectizeInput(session, 'subtype', choices = subtype, server = TRUE)
    if(input$type != "Protein vs mRNAs") {shinyjs::show(id = "subtype")}
    
  })
  
  # Handle event when user selects subtype
  observeEvent(input$subtype, {
    # Phosphosite doesn't allow you to toggle clincal options
    if(input$type == "Phosphosite") {
      shinyjs::hide(id = "subtype_options")
    }
    else {
      shinyjs::show(id = "subtype_options")
    }
    shinyjs::show(id = "gene")
    shinyjs::hide(id = "genes")
    shinyjs::hide(id = "mutation_status")
    
    
    if(input$subtype == "Multiplot") {
      shinyjs::show(id = "genes")
      shinyjs::hide(id = "subtype_options")
      shinyjs::hide(id = "gene")
      updateSelectizeInput(session, "genes", label = paste("Multi",tolower(itemPlot)," plot",sep=""))
    }
    else if(input$subtype == "Subtype") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Subtypes",
                               choices = subtypeChoices, 
                               selected = subtypeSelected)
    }
    else if(input$subtype == "Cytogenetics") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Criteria",
                               choices = cytoChoices,
                               selected = cytoSelected)
    }
    else if(input$subtype == "Fusion") {
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Criteria",
                               choices = fusionChoices,
                               selected = fusionSelected)
    }
    else if(input$subtype == "Mutations") {
      shinyjs::hide(id = "subtype_options")
      updateSelectizeInput(session, "gene", label = paste(itemPlot,"to plot"))
      shinyjs::show(id = "mutation_status")
    }
    
    else {
      shinyjs::hide(id = "subtype_options")
    }
  })
  
  # TODO: Handle changing of datasets when different type is selected (input$type)
  #updateSelectizeInput(session, "genes", choices = tmtChoices$Gene, server = TRUE)
  #updateSelectizeInput(session, "gene", choices = tmtChoices$Gene, server = TRUE)
  
  output$plot <- renderPlotly({
    plotReady <- FALSE
    # Multiplot
    if(input$subtype == "Multiplot" && length(input$genes) > 0) {
      query <- geneQuery(genes = input$genes,type = input$type)
      tcga <- dbGetQuery(database,query)
      
      query <- clinicalQuery(factors="*", table=paste(input$type,"_Clinical",sep=""))
      clinical <- dbGetQuery(database,query)
      
      clinical <- merge(tcga, clinical, by="UPN")
      
      # Plot
      g <- ggplot(clinical,aes(fill=Gene, y=Value, x=Name, text = paste0("UPN ID: ",UPN,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"),
              axis.title=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Multiple protein view") +
        ylab("Log2 Expression") + xlab("")
      
      
      plotReady <- TRUE
    }
    else if(input$subtype == "Subtype" && length(input$subtype_options) > 0) {
      if(input$type == "Phosphosite") {
        query <- paste("SELECT UPN,Gene,Value FROM Genes WHERE Gene GLOB '",input$gene,"*' AND Type='Phosphosite';",sep="")
      }
      else {
        query <- geneQuery(genes = input$gene, type = input$type)
      }
      tcga <- dbGetQuery(database,query)
      
      query <- clinicalQuery(factors=c("UPN","Name","TCGA_ID","TCGA_Name","FAB"),
                             table=paste(input$type,"_Clinical",sep=""),
                             subtypes=input$subtype_options,
                             type="FAB")
      clinical <- dbGetQuery(database,query)
      
      clinical <- merge(clinical,tcga, by="UPN")
      clinical$Gene <- input$gene
      clinical$FAB <- factor(clinical$FAB,levels = subtypeSelected)
      clinical <- subset(clinical, select = -c(UPN) )
      
      # Plot
      g <- ggplot(clinical,aes(FAB, Value, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),
              axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("") + scale_x_discrete(labels = as.list(invert(hash(subtypeChoices))))
      
      plotReady <- TRUE
    }
    
    else if(input$subtype == "Cytogenetics" && length(input$subtype_options) > 0) {
      if(input$type == "Phosphosite") {
        query <- paste("SELECT UPN,Gene,Value FROM Genes WHERE Gene GLOB '",input$gene,"*' AND Type='Phosphosite';",sep="")
      }
      else {
        query <- geneQuery(genes = input$gene, type = input$type)
      }
      tcga <- dbGetQuery(database,query)
      
      query <- clinicalQuery(factors=c("UPN","Name","TCGA_ID","TCGA_Name","Cyto_Risk"),
                             table=paste(input$type,"_Clinical",sep=""),
                             subtypes=input$subtype_options,
                             type="Cyto_Risk")
      clinical <- dbGetQuery(database,query)
      
      clinical <- merge(clinical,tcga, by="UPN")
      clinical$Gene <- input$gene
      clinical <- subset(clinical, select = -c(UPN) )
      # TODO Change db instead of making this change
      #clinical$Cyto_Risk[clinical$Cyto_Risk == "Good"] <- "Favorable"
      #clinical$Cyto_Risk[clinical$Cyto_Risk == "Poor"] <- "Adverse"
      clinical$Cyto_Risk <- factor(clinical$Cyto_Risk,levels = cytoSelected)
      
      # Plot
      g <- ggplot(clinical,aes(Cyto_Risk, Value, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Log2 Expression") + xlab("") + scale_x_discrete(labels = as.list(invert(hash(cytoChoices))))
      plotReady <- TRUE
    }
    
    else if(input$subtype == "Fusion" && length(input$subtype_options) > 0) {
      if(input$type == "Phosphosite") {
        query <- paste("SELECT UPN,Gene,Value FROM Genes WHERE Gene GLOB '",input$gene,"*' AND Type='Phosphosite';",sep="")
      }
      else {
        query <- geneQuery(genes = input$gene, type = input$type)
      }
      tcga <- dbGetQuery(database,query)
      query <- clinicalQuery(factors=c("UPN","Name","TCGA_ID","TCGA_Name","Fusion"),
                             table=paste(input$type,"_Clinical",sep=""),
                             subtypes=input$subtype_options,
                             type="Fusion")
      clinical <- dbGetQuery(database,query)
      
      clinical <- merge(clinical,tcga, by="UPN")
      clinical$Gene <- input$gene
      clinical <- subset(clinical, select = -c(UPN) )
      clinical$Fusion <- factor(clinical$Fusion,levels = fusionSelected)
      
      g <- ggplot(clinical,aes(Fusion, Value, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
        ggtitle(paste0("Log2 Expression for ",input$gene)) +
        theme(text=element_text(size=12, family="avenir", face="bold"),axis.text=element_text(size=12, family="avenir", face="bold"), axis.text.x = element_text(angle = 45)) +
        ylab("Log2 Expression") + xlab("") + scale_x_discrete(labels = as.list(invert(hash(fusionChoices))))
      plotReady <- TRUE
    }
    # TODO add length greater than 0 for options
    else if(input$subtype == "Mutations") {
      query <- geneQuery(factors = c("UPN","Value"),genes = input$gene, type = input$type)
      tcga <- dbGetQuery(database,query)
      tcga$Group <- "WT"
      
      upns_with_mut <- as.character(mutation_table$UPN[which(mutation_table$Gene %in% input$mutation_status)])
      
      query <- clinicalQuery(factors = "*",
                             table = paste0(input$type,"_Clinical"))
      all_clinical <- dbGetQuery(database,query)
      print(all_clinical)
      
      upns_with_health <- as.character(all_clinical$UPN[which(all_clinical$TCGA_Name %like% "Healthy")])
      
      tcga$Group[tcga$UPN %in% upns_with_mut] <- input$mutation_status
      tcga$Group[tcga$UPN %in% upns_with_health] <- all_clinical$FAB[tcga$UPN %in% upns_with_health]
      
      tcga$Mutation <- paste(input$mutation_status,"WT")
      tcga$Mutation[tcga$UPN %in% upns_with_mut] <- mutation_table$Mutation[tcga$UPN %in% upns_with_mut]
      tcga$Mutation[tcga$UPN %in% upns_with_health] <- all_clinical$FAB[tcga$UPN %in% upns_with_health]
      tcga$Gene <- input$gene
      
      clinical <- merge(tcga,all_clinical[,c("UPN","Name","TCGA_ID")], by = "UPN")
      clinical <- clinical[,c(1,6,7,3,4,2,5)]
      
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
    
    else if(input$type == "Protein vs mRNAs") {
      
    }
    
    # Plotting
    if(plotReady) {
      ggplotly(g, tooltip="text")
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)
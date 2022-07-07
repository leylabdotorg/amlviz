library(shiny)
library(shinyjs)
library(shinythemes)
library(plotly)
library(ggplot2)
library(DBI)

source("functions.R")


# database
database <- dbConnect(RSQLite::SQLite(), "Proteomics.db")
print("Connected to database")

# database vars
# more effcient this way... choices are only loaded once per server rather than every client 
tmt_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='TMT';"))
lfq_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='LFQ';"))
# phospho_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='Phosphosite';"))
mrna_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='mRNA';"))
# gene correlations file for vs

# local files
# May remove in favor of database
dropdowngenes <- read.table(file = "Data_files/mutation_specific_genes.txt", sep="\t", skipNul=T, encoding="UTF-8", quote = "")

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
      
      # Dropdown to select gene
      selectizeInput(inputId = "genes",
                     label = "Protein(s) to plot",
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
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusions", "Mutations")
      subtypeChoices <<- list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy Lin-"="Healthy Lin-")
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy Lin-")
      
      cytoChoices <<- list("Favorable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor", "Healthy Lin-" = "Healthy Lin-")
      cytoSelected <<- c("Poor","Good","Intermediate","Healthy Lin-")
      
      fusionsChoices <<- list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy Donor Lin-" = "Healthy Lin-")
      fusionsSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")
    }
    else if(input$type == "LFQ") {
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusions", "Mutations")
      
      subtypeChoices <<- list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy CD34"="Healthy CD34","Healthy Lin-"="Healthy Lin-")
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy CD34","Healthy Lin-")
      
      cytoChoices <<- list("Favourable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor","Healthy CD34" = "Healthy CD34" ,"Healthy Lin-" = "Healthy Lin-")
      cytoSelected <<- c("Poor","Good","Intermediate","Healthy CD34","Healthy Lin-")
      
      fusionsChoices <<- list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy CD34"="Healthy CD34","Healthy Donor Lin-" = "Healthy Lin-")
      fusionsSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy CD34","Healthy Lin-")
    }
    else if(input$type == "Phosphosite") {
      subtype <- c("Subtype", "Cytogenetics", "Fusions", "Mutations")
      
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy Lin-")
      cytoSelected <<- c("Poor","Good","Intermediate","Healthy Lin-")
      fusionsSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")
    }
    else if(input$type == "mRNA") {
      itemPlot <<- "Gene"
      subtype <- c("Multiplot","Subtype", "Cytogenetics", "Fusions", "Mutations")
      
      subtypeChoices <<- list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro")
      subtypeSelected <<- c("M0","M1","M2","M3","M4","M5","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")

      cytoChoices <<- list("Favourable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor", "Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro")
      cytoSelected <<- c("Poor","Good","Intermediate","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
      
      fusionsChoices <<- list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro")
      fusionsSelected <<- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")  
    }
    else if(input$type == "Protein vs mRNAs") {
      shinyjs::hide(id = "subtype")
      subtype <- NULL
      updateSelectizeInput(session, "genes", label = "Enter Gene/Protein to plot")
    }
    
    updateSelectizeInput(session, 'subtype', choices = subtype, server = TRUE)
    if(input$type != "Protein vs mRNAs") {shinyjs::show(id = "subtype")}
    
  })
  # Handle event when user selects subtype
  observeEvent(input$subtype, {
    # Phosphosite doesn't alllow to toggle clincal options
    if(input$type == "Phosphosite") {
      shinyjs::hide(id = "subtype_options")
    }
    else {
      shinyjs::show(id = "subtype_options")
    }
    shinyjs::hide(id = "mutation_status")
  
    
    if(input$subtype == "Multiplot") {
      shinyjs::hide(id = "subtype_options")
      updateSelectizeInput(session, "genes", label = paste("Multi",tolower(itemPlot)," plot",sep=""))
    }
    else if(input$subtype == "Subtype") {
      updateSelectizeInput(session, "genes", label = paste(itemPlot,"to plot"))
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Subtypes",
                               choices = subtypeChoices, 
                               selected = subtypeSelected)
    }
    else if(input$subtype == "Cytogenetics") {
      updateSelectizeInput(session, "genes", label = paste(itemPlot,"to plot"))
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Criteria",
                               choices = cytoChoices,
                               selected = cytoSelected)
    }
    else if(input$subtype == "Fusions") {
      updateSelectizeInput(session, "genes", label = paste(itemPlot,"to plot"))
      updateCheckboxGroupInput(session,
                               "subtype_options",
                               label = "Criteria",
                               choices = fusionsChoices,
                               selected = fusionsSelected)
    }
    else if(input$subtype == "Mutations") {
      shinyjs::hide(id = "subtype_options")
      updateSelectizeInput(session, "genes", label = paste(itemPlot,"to plot"))
      shinyjs::show(id = "mutation_status")
    }
    
    else {
      shinyjs::hide(id = "subtype_options")
    }
  })
  
  # TODO: Handle changing of datasets when different type is selected (input$type)
  updateSelectizeInput(session, "genes", choices = tmt_choices$Gene, server = TRUE)
  
  output$plot <- renderPlotly({
    # plotReady <- FALSE
    # # Multiplot
    # if(input$subtype == "Multiplot" && length(input$genes) > 0) {
    #   query <- geneQuery(genes = input$genes,type = input$type)
    #   tcga <- dbGetQuery(database,query)
    #   
    #   query <- clinicalQuery(factors="*", table=paste(input$type,"_Clinical",sep=""))
    #   clinical <- dbGetQuery(database,query)
    #   
    #   clinical <- merge(tcga, clinical, by="UPN")
    #   plotReady <- TRUE
    # }
    # else if(input$subtype == "Subtype" && length(input$subtype_options)) {}
    # 
    # else if(input$subtype == "Cytogenetics" && length(input$subtype_options)) {}
    # 
    # else if(input$subtype == "Fusions" && length(input$subtype_options)) {}
    # 
    # else if(input$subtype == "Mutations") {}
    # 
    # else {}
    # 
    # # Plotting
    # if(plotReady) {
    #   g <- ggplot(clinical,aes(fill=Gene, y=Value, x=Name, text = paste0("UPN ID: ",UPN,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
    #     theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"),
    #           axis.title=element_text(size=12, family="avenir", face="bold"),
    #           axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     ggtitle("Multiple protein view") +
    #     ylab("Log2 Expression") + xlab("")
    #   ggplotly(g, tooltip="text")
    # }
    # 
  })
  
  
}

shinyApp(ui = ui, server = server)
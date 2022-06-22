# REQUIRED LIBRARIES #
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(ggbeeswarm)
library(reshape2)
library(shinycssloaders)
library(dplyr)
library(beeswarm)
library(RCurl)
library(data.table)
library(RMariaDB)

# database
database <- dbConnect(RMariaDB::MariaDB(), user='payton', password='password', dbname='test', host='localhost')
print("Connected to database")


# database vars
genenames_choices <- dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='TMT';")


#Required files
#Common files
dropdowngenes <- read.table(file = "Data_files/mutation_specific_genes.txt", sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - drop-down mutation gene")
mutation_table <- read.table(file = "Data_files/mutations.txt", header=T, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - mutation file")
# TMT Files
genenames_tmt = read.table(file = "Data_files/TMT/genenames.txt", stringsAsFactors=F, header=F, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - gene file for TMT")
clinicaldata_tmt <- read.table(file = "Data_files/TMT/clinical.txt", header=T, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - clinical data for TMT")
# LFQ Files
genenames_lfq = read.table(file = "Data_files/LFQ/genenames.txt",stringsAsFactors=F, header=F, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - gene file for LFQ")
clinicaldata_lfq <- read.table(file = "Data_files/LFQ/clinical.txt", header=T, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - clinical data for LFQ")
# Phospho Files
genenames_pho = read.table(file = "Data_files/Phospho/genenames.txt", stringsAsFactors=F, header=F, skipNul=T, encoding="UTF-8", quote = "")
print("Read - gene file for Phospho")
clinicaldata_pho <- read.table(file = "Data_files/Phospho/clinical.txt", header=T, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - clinical data for Phospho")
# mRNA Files
genenames_mrna = read.table(file = "Data_files/mRNA/genenames.txt", stringsAsFactors=F, header=F, sep="\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - gene file for mRNA")
clinicaldata_mrna <- read.table(file = "Data_files/mRNA/clinical.txt", header=T, sep = "\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - clinical data for mRNA")
# correlation files
genenames_corr = read.table("Data_files/correlation_genes.txt", stringsAsFactors=F, header=F, sep = "\t", skipNul=T, encoding="UTF-8", quote = "")
print("Read - correlation gene matrix")

# Shinny app stars here
shinyApp (
  
  # UI part of the app
  ui <- navbarPage(title = "Proteomic and Phosphoproteomic Landscapes of AML", theme = shinytheme("flatly"), collapsible = TRUE, selected = "Home", 
                   tags$head(includeCSS("Data_files/style.css"), tags$style(HTML("a {font-weight: bold;font-size: 15px;}"))),
                   # Home page
                   tabPanel(title="Home", icon = icon("home"), 
                            tags$div(
                              tags$h4("Introduction"), "These data represent the proteome and phosphoproteome of de novo, Acute Myeloid Leukemia (AML) as well as matching mRNA data from the same patients. Please click the links above to explore these datasets.", tags$br(),tags$br(),
                              tags$h4("Sample information"), "Bone marrow samples were collected at presentation from 44 adult patients with de novo AML. All 44 patients included in this study were also part of the Cancer Genome Atlas (TCGA) LAML study (NEJM 2013, DOI: ",tags$a(href="https://www.nejm.org/doi/full/10.1056/nejmoa1301689",target="_blank", "10.1056/NEJMoa1301689"),"). The 44 patients were selected to represent major cytogenetic and mutational landscapes amongst the 200 TCGA LAML patients with adequate remaining diagnosis cryovials. Cryovials were thawed in the presence of the cell-permeable, irreversible serine protease inhibitor diisopropyl fluorophosphate (DFP) to inactivate neutrophil serine proteases. Healthy control bone marrow samples were lineage-depleted (3 samples) or CD34-selected (3 samples) using autoMACS prior to analysis. Samples were analyzed using tandem-mass-tag (TMT) or label-free-quantification (LFQ) mass spectrometry to obtain protein abundance. A fraction of each sample was IMAC enriched for phosphopeptide analysis also using TMT mass spectrometry. Each of these datasets (TMT, LFQ and phosphopeptides) along with matching RNA-seq from the same patients (previously published by the TCGA LAML study) are made available here for public analysis and download. Paper currently under review; citation and link pending publication.", tags$br(),tags$br(),
                              tags$h4("Data download"), "Data are available freely for download and use. These data represent the proteomic, phosphoproteomic and mRNA measurements from 44 adult patients with de novo Acute Myeloid Leukemia. Each row in the spreadsheet represents one protein, gene, or phosphosite as indicated. Each column represents one sample identified by the UPN (patients) or an appropriate identifier (healthy controls). Please refer to the following paper for further information and please cite this paper for any use of these data: LINK AND CITATION PENDING PUBLICATION.", tags$br(),
                              tags$h5("Datasets included:"),
                              tags$ul(
                                tags$li("Tandem-mass-tag (TMT) mass spectrometry-based protein abundance measurements from the bone marrow of 44 patients and 3 healthy, lineage-depleted controls. Values are log2 transformed and median centered at 0."), 
                                tags$li("Label-free quantification (LFQ) mass spectrometry protein abundance measurements from the bone marrow of 44 patients, 3 healthy, lineage-depleted controls and 3 healthy, CD34-selected controls."),
                                tags$li("Phosphosite abundance measurements after IMAC enrichment and aggregation of phosphopeptide abundances as measured by tandem-mass-tage (TMT) mass spectrometry from the bone marrow of 44 patients and 3 healthy, lineage-depleted controls. Values are log2 transformed and median centered at 0."),
                                tags$li("mRNA sequencing for the 44 patients included in this study as initially measured by the TCGA LAML study. Also included are data from healthy control marrows which were CD19 selected (B-cells), CD3 selected (T-cells), CD34-selected (enriched for hematopoietic stem and progenitor cells), as well as monocytes, neutrophils and promyelocytes. Values represent TPMs for each gene."),
                                tags$li("Clinical annotations and information for each included patient including mutational status, survival data, treatment information and demographics."),
                                tags$li("Spearman correlation of protein abundance vs. mRNA abundance for 10,125 genes/proteins detected in at least 20% of the 44 AML samples. Protein abundance is measured by tandem-mass-tag (TMT) mass spectrometry and mRNA sequencing is performed on matched cryovials from the same patients.")
                                ), tags$br(),
                              tags$a(href="https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Supplemental Table 3.xlsx", "Download the TMT, LFQ, and mRNA abundance data"),tags$br(),
                              tags$a(href="https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Supplemental Table 5.xlsx", "Download the phosphosite abundance data"),tags$br(),
                              tags$a(href="https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Supplemental Table 2.xlsx", "Download the clinical patient data"),tags$br(),
                              tags$a(href="https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Supplemental Table 4.xlsx", "Download protein abundance vs. mRNA abundance correlations"),tags$br(),tags$br(),
                              "See also: ",tags$a(href="https://leylab.shinyapps.io/NPMc_TurboID/", "NPMc Turbo ID dataset", target="_blank")," also from this publication",
                              tags$br(),tags$br()
                              )),
                   
                   # TMT App
                    tabPanel("TMT", tabsetPanel(
                      # MULTIPLOT #
                      tabPanel("Multi protein Plot", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_1_t", "Multiprotein plot", choices=NULL, multiple = TRUE, selected = NULL), downloadButton("downloadData_t_m", "Download matrix"),width = 4), mainPanel(fluidRow(br(), column(width = 12, plotlyOutput("multigene_plot_tmt",height = "500px")))),position = c("left"),fluid = TRUE)),
                      # SUBTYPE # 
                      tabPanel("Single protein by FAB subtype", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_2_t", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("subtypes_t", label = h5("Subtypes"), choices = list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy Lin-"="Healthy Lin-"), selected = c("M0","M1","M2","M3","M4","M5","Healthy Lin-")), downloadButton("downloadData_t_s", "Download matrix") , width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_subtype_tmt",height = "500px")))))),
                      # CYTOGENETICS #
                      tabPanel("Single protein by ELN2017 criteria", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_3_t", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("cytogenetics_t", label = h5("Criteria"),  choices = list("Favourable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor", "Healthy Lin-" = "Healthy Lin-"),  selected = c("Poor","Good","Intermediate","Healthy Lin-")), downloadButton("downloadData_t_c", "Download matrix") , width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_cytogenetics_tmt",height = "500px")))))),
                      # FUSIONS #
                      tabPanel("Single protein by Key AML Fusions", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_4_t", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("fusion_t", label = h5("Key AML Fusions"), choices = list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy Donor Lin-" = "Healthy Lin-"), selected = c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")), downloadButton("downloadData_t_f", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_fusion_tmt",height = "550px")))))),
                      # MUTATION #
                      tabPanel("Single protein by Mutations", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_5_t", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), selectInput("single_gene_name_6_t", "Mutation status", choices=dropdowngenes$V1), downloadButton("downloadData_t_w", "Download matrix"), width = 4), mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_mutation_tmt",height = "500px"))))))
                      )),
                   
                   # LFQ App
                   tabPanel("LFQ", tabsetPanel(
                     # MULTIPLOT #
                     tabPanel("Multi protein Plot", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_1_l", "Multiprotein plot", choices=NULL, multiple = TRUE, selected = NULL), downloadButton("downloadData_l_m", "Download matrix"), width = 4),mainPanel(fluidRow(br(), column(12, plotlyOutput("multigene_plot_lfq",height = "500px")))))),
                     # SUBTYPE #
                     tabPanel("Single protein by FAB subtype", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_2_l", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("subtypes_l", label = h5("Subtypes"), choices = list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy CD34"="Healthy CD34","Healthy Lin-"="Healthy Lin-"), selected = c("M0","M1","M2","M3","M4","M5","Healthy CD34","Healthy Lin-")), downloadButton("downloadData_l_s", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_subtype_lfq",height = "500px")))))),
                     # CYTOGENETICS #
                     tabPanel("Single protein by ELN2017 criteria", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_3_l", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("cytogenetics_l", label = h5("Cytogenetics"),  choices = list("Favourable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor","Healthy CD34" = "Healthy CD34" ,"Healthy Lin-" = "Healthy Lin-"), selected = c("Poor","Good","Intermediate","Healthy CD34","Healthy Lin-")), downloadButton("downloadData_l_c", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_cytogenetics_lfq",height = "500px")))))),
                     # FUSIONS #
                     tabPanel("Single protein by Key AML Fusions", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_4_l", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("fusion_l", label = h5("Key AML Fusions"), choices = list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy CD34"="Healthy CD34","Healthy Donor Lin-" = "Healthy Lin-"), selected = c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy CD34","Healthy Lin-")), downloadButton("downloadData_l_f", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_fusion_lfq",height = "500px")))))),
                     # MUTATION #
                     tabPanel("Single protein by Mutations", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_5_l", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL), selectInput("single_gene_name_6_l", "Mutation status", choices=dropdowngenes$V1), downloadButton("downloadData_l_w", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_mutation_lfq",height = "500px"))))))
                     )),
                   
                   # Phospho App
                   tabPanel("Phosphosite", tabsetPanel(
                     # SUBTYPE #
                     tabPanel("Single protein by FAB subtype", fluid = TRUE, sidebarLayout(mainPanel(),sidebarPanel(selectizeInput("single_gene_name_1_p", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL),downloadButton("downloadData_p_s", "Download matrix"), width = 12, plotlyOutput("singlegene_plot_subtype_phospho",height = "700px")))),
                     # CYTOGENETICS #
                     tabPanel("Single protein by ELN2017 criteria", fluid = TRUE, sidebarLayout(mainPanel(),sidebarPanel(selectizeInput("single_gene_name_2_p", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL),downloadButton("downloadData_p_c", "Download matrix"), width = 12, plotlyOutput("singlegene_plot_cytogenetics_phospho",height = "700px")))),
                     # FUSIONS #
                     tabPanel("Single protein by Key AML Fusions", fluid = TRUE, sidebarLayout(mainPanel(),sidebarPanel(selectizeInput("single_gene_name_3_p", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL),downloadButton("downloadData_p_f", "Download matrix"), width = 12, plotlyOutput("singlegene_plot_fusion_phospho",height = "700px")))),
                     # MUTATION #
                     tabPanel("Single protein by Mutations", fluid = TRUE, sidebarLayout(mainPanel(),sidebarPanel(fluidRow(column(6, selectizeInput("single_gene_name_4_p", "Protein to plot", choices=NULL, multiple = FALSE, selected = NULL)), column(6, selectInput("single_gene_name_5_p", "Mutation status", choices=dropdowngenes$V1))), downloadButton("downloadData_p_w", "Download matrix"), width = 12, plotlyOutput("singlegene_plot_mutation_phospho",height = "700px"))))
                     )),
                   
                   # mRNA App
                   tabPanel("mRNA", tabsetPanel(
                     # MULTIPLOT #
                     tabPanel("Multi gene Plot", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_1_m", "Multigene plot", choices=NULL, multiple = TRUE, selected = NULL), downloadButton("downloadData_m_m", "Download matrix"), width = 4), mainPanel(fluidRow(br(), column(width = 12, plotlyOutput("multigene_plot_mrna",height = "500px")))),position = c("left"),fluid = TRUE)),
                     # SUBTYPE #
                     tabPanel("Single gene by FAB subtype", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_2_m", "Gene to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("subtypes_m", label = h5("Subtypes"), choices = list("M0" = "M0", "M1" = "M1", "M2" = "M2","M3" = "M3","M4" = "M4", "M5" = "M5","Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro"), selected = c("M0","M1","M2","M3","M4","M5","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")), downloadButton("downloadData_m_s", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_subtype_mrna",height = "500px")))))),
                     # CYTOGENETICS #
                     tabPanel("Single gene by ELN2017 criteria", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_3_m", "Gene to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("cytogenetics_m", label = h5("Criteria"),  choices = list("Favourable" = "Good", "Intermediate" = "Intermediate", "Adverse" = "Poor", "Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro"), selected = c("Poor","Good","Intermediate","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")), downloadButton("downloadData_m_c", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_cytogenetics_mrna",height = "500px")))))),
                     # FUSIONS #
                     tabPanel("Single gene by Key AML Fusions", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_4_m", "Gene to plot", choices=NULL, multiple = FALSE, selected = NULL), checkboxGroupInput("fusion_m", label = h5("Key AML Fusions"), choices = list("CBFB-MYH11" = "CBFB","RUNX1-RUNX1T1" = "RUNX1","PML-RARA" = "PML","MLL-X" = "MLL","NUP98-NSD1" = "NSD1","BCR-ABL" = "BCR-ABL1","AML without Fusion" = "Normal","Healthy Donor CD19"="Healthy Donor CD19", "Healthy Donor CD3"="Healthy Donor CD3","Healthy Donor CD34"="Healthy Donor CD34","Healthy Donor Monocytes"="Healthy Donor Mono","Healthy Donor Neutrophils"="Healthy Donor Neu","Healthy Donor Promylocytes"="Healthy Donor Pro"), selected = c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")), downloadButton("downloadData_m_f", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_fusion_mrna",height = "550px")))))),
                     # MUTATION #
                     tabPanel("Single gene by Mutations", fluid = TRUE, sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_5_m", "Gene to plot", choices=NULL, multiple = FALSE, selected = NULL), selectInput("single_gene_name_6_m", "Mutation status", choices=dropdowngenes$V1), downloadButton("downloadData_m_w", "Download matrix"), width = 4),mainPanel(fluidRow(br(),column(12, plotlyOutput("singlegene_plot_mutation_mrna",height = "500px"))))))
                     )),
                   
                   # Protein vs. mRNA app
                   tabPanel("Protein vs mRNA", sidebarLayout(sidebarPanel(selectizeInput("single_gene_name_1_pm", "Enter Gene/Protein to plot", choices=NULL, multiple = TRUE, selected = NULL), downloadButton("downloadData_pm_m", "Download Table of Correlations"), plotlyOutput("multigene_plot_protein",height = "500px"), width = 12), mainPanel(), position = c("left"), fluid = TRUE)
                            )
                   ),
  
  
  ## ENTIRE SEVER WORKING GOES HERE
  server = function(input, output, session) 
  {
    ###### TMT APP STUFF ######
    # Updating the selectize options
    updateSelectizeInput(session, 'single_gene_name_1_t', choices = genenames_choices$Gene, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_2_t', choices = genenames_choices$Gene, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_3_t', choices = genenames_choices$Gene, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_4_t', choices = genenames_choices$Gene, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_5_t', choices = genenames_choices$Gene, server = TRUE)
    
    
    
    
    # MULTIPLOT #
    output$multigene_plot_tmt <- renderPlotly({
      # Getting actual genes to plot
      #genesToPlot <- input$single_gene_name_1_t
      genesToPlot <- c("DNMT3A","TP53")
      if(length(genesToPlot) > 0)
      {
        tcga = NULL
        for(g in genesToPlot){
          tcga = rbind(tcga,read.table(paste0("/home/payton/research/washu/TCGA_Proteomics_App/Prep/TCGA_Proteomics_app/Gene_files/TMT/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
          #tcga = rbind(tcga,read.table(paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
          #SELECT Gene,UPN,Value FROM Genes WHERE (Gene='DNMT3A' OR Gene='TP53') AND Type='TMT';
        }
        
        # Data cleanup and preparing for output
        df = melt(tcga,id.vars="gene")
        names(df) <- c("Gene", "UPN", "Log2.Expression")
        df <- merge(df, clinicaldata_tmt, by="UPN") # SELECT * FROM TMT_Clinical WHERE (UPN="ND8" OR ...);
        dmtm <<- df
        
        g <- ggplot(df,aes(fill=Gene, y=Log2.Expression, x=Name, text = paste0("UPN ID: ",UPN,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
          theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"), 
                axis.title=element_text(size=12, family="avenir", face="bold"),
                axis.text.x = element_text(angle = 90, hjust = 1)) +
          ggtitle("Multiple protein view") +
          ylab("Log2 Expression") + xlab("")
        ggplotly(g, tooltip="text")
        
      }
    })
    
    # Output file
    output$downloadData_t_m <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmtm, file, quote = F, row.names = F, sep = ",")}
    )
    
    
    
    
    
    
    
    
    # SUBTYPE #
    output$singlegene_plot_subtype_tmt <- renderPlotly({
      # Getting actual genes to plot
      #genesToPlot <- toupper(input$single_gene_name_2_t)
      #subtypesToPlot <- input$subtypes_t
      genesToPlot <- "TP53"
      subtypesToPlot <- c("M0","M1","M2","M3","M4","M5","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            #tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            tcga = as.data.frame(read.table(file = paste0("/home/payton/research/washu/TCGA_Proteomics_App/Prep/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1] # SELECT Value FROM Genes WHERE Gene="TP53"; (Replace TP53 with all genes selected)
            
            
            bysubtype <- filter(clinicaldata_tmt, clinicaldata_tmt$FAB %in% subtypesToPlot) # Selects only observations with FAB that is in subtypesToPlot (just clinical data)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            # plottinggene shape is || Values | UPN ||
            
            
            # Data cleanup and preparing for output
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(2,3,4,5,6)]
            colnames(finalmatrix) <- c("Log2.Expression","Name","TCGA_ID","TCGA_Name","FAB")
            finalmatrix$FAB <- factor(finalmatrix$FAB,levels=c("M0","M1","M2","M3","M4","M5","Healthy Lin-"))
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(2,3,4,5,1,6)]
            dmts <<- finalmatrix
            
            g <- ggplot(finalmatrix,aes(FAB, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Log2 Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), 
                    axis.text=element_text(size=12, family="avenir", face="bold")) +
              ylab("Log2 Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    
    # Output file
    output$downloadData_t_s <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmts, file, quote = F, row.names = F, sep = "\t")}
    )
    
    
    
    
    
    
    
    # CYTOGENETICS #
    output$singlegene_plot_cytogenetics_tmt <- renderPlotly({
      #genesToPlot <- toupper(input$single_gene_name_3_t)
      #subtypesToPlot <- input$cytogenetics_t
      genesToPlot <- "DNMT3A"
      subtypesToPlot <- c("Poor","Good","Intermediate","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            
            
            bysubtype <- filter(clinicaldata_tmt, clinicaldata_tmt$Cyto_Risk %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            
            # Data cleanup and preparing for output
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(2,3,4,5,7)]
            colnames(finalmatrix) <- c("Log2.Expression","Name","TCGA_ID","TCGA_Name","Cyto_risk")
            levels(finalmatrix$Cyto_risk) <- c("Favourable","Healthy Lin-","Intermediate","Adverse")
            finalmatrix$Cyto_risk <- factor(finalmatrix$Cyto_risk,levels=c("Favourable","Intermediate","Adverse","Healthy Lin-"))
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(2,3,4,5,1,6)]
            dmtc <<- finalmatrix
            
            g <- ggplot(finalmatrix,aes(Cyto_risk, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Log2 Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold")) +
              ylab("Log2 Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    
    # Output file
    output$downloadData_t_c <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmtc, file, quote = F, row.names = F, sep = "\t")}
    )
    
    
    
    
    
    
    
    
    
    
    # FUSION #
    output$singlegene_plot_fusion_tmt <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_4_t)
      subtypesToPlot <- input$fusion_t
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            
            bysubtype <- filter(clinicaldata_tmt, clinicaldata_tmt$Fusion %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(2,3,4,5,8)]
            colnames(finalmatrix) <- c("Log2.Expression","Name","TCGA_ID","TCGA_Name","Fusion")
            finalmatrix$Fusion <- factor(finalmatrix$Fusion,levels=c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-"))
            levels(finalmatrix$Fusion) <- c("CBFB-MYH11","RUNX1-RUNX1T1","PML-RARA","MLL-X","NUP98-NSD1","BCR-ABL","AML without Fusion","Healthy Donor Lin-")
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(2,3,4,5,1,6)]
            dmtf <<- finalmatrix
            
            g <- ggplot(finalmatrix,aes(Fusion, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Log2 Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"),axis.text=element_text(size=12, family="avenir", face="bold"), axis.text.x = element_text(angle = 45)) +
              ylab("Log2 Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_t_f <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmtf, file, quote = F, row.names = F, sep = "\t")}
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    # MUTATION #
    output$singlegene_plot_mutation_tmt <- renderPlotly({
      normalgenes <- toupper(input$single_gene_name_5_t)
      genesToPlot <- input$single_gene_name_6_t
      #normalgenes <- "A1BG"
      #genesToPlot <- "BCOR"
      tcga = NULL
      if(normalgenes != "")
      {
        #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",normalgenes,".tsv")
        destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",normalgenes,".tsv")
        if(url.exists(destfile))
        {
          tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/TMT/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          tcga <- melt(tcga, id.vars=c("gene"))
          colnames(tcga) <- c("Gene","UPN","Expression")
          tcga$Gene <- genesToPlot
          tcga$Group <- "WT"
          upns_with_mut <- as.character(mutation_table$UPN[which(mutation_table$Gene %in% genesToPlot)])
          upns_with_health <- as.character(clinicaldata_tmt$UPN[which(clinicaldata_tmt$TCGA_Name %like% "Healthy")])
          tcga$Group[tcga$UPN %in% upns_with_mut] <- "MT"
          for(i in upns_with_health)
          {
            newvalue <- as.character(clinicaldata_tmt$FAB[clinicaldata_tmt$UPN == i])
            tcga$Group[tcga$UPN == i] <- newvalue
          }
          tcga_wt <- tcga[which(tcga$Group == "WT" ),c(2,1,3,4)]
          tcga_wt$Mutation <- paste0(genesToPlot," WT", sep = "")
          tcga_he <- tcga[!tcga$Group %in% c("WT","MT"),c(2,1,3,4)]
          tcga_he$Mutation <- tcga_he$Group
          tcga_mt <- tcga[which(tcga$Group == "MT" ),c(1,2,3)]
          finalmatrix <- merge(tcga_mt,mutation_table, by=c("UPN","Gene"))
          finalmatrix <- rbind(finalmatrix,tcga_wt,tcga_he)
          finaldata <- merge(finalmatrix, clinicaldata_tmt, by=c("UPN"))
          finaldata <- finaldata[,c(1,2,3,4,5,6,7)]
          names(finaldata) <- c("UPN","Gene","Log2.Expression","Group","Mutation","Name","TCGA")
          finaldata <- finaldata[,c(1,6,7,4,5,3,2)]
          finaldata$Gene <- normalgenes
          dmtw <<- finaldata
          g <- ggplot(finaldata,aes(Group, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />Mutation type: ", Mutation,"<br />TCGA Sample ID: ", TCGA))) + 
            geom_quasirandom(size = 0.8) + 
            theme_bw() +
            ggtitle(paste0("Log2 Expression for ",normalgenes," with ",genesToPlot, ": WT|MT")) + 
            theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
            ylab("Log2 Expression") + xlab("")
          ggplotly(g, tooltip="text")
        }
      }
    })
    output$downloadData_t_w <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmtw, file, quote = F, row.names = F, sep = "\t")}
    )
    
    
    
    
    
    
    
    
    
    ###### LFQ APP STUFF ######
    # Updating the selectize options
    updateSelectizeInput(session, 'single_gene_name_1_l', choices = genenames_lfq$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_2_l', choices = genenames_lfq$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_3_l', choices = genenames_lfq$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_4_l', choices = genenames_lfq$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_5_l', choices = genenames_lfq$V1, server = TRUE)
    # MULTIPLOT #
    output$multigene_plot_lfq <- renderPlotly({
      genesToPlot <- input$single_gene_name_1_l
      #genesToPlot <- "DNMT3A"
      if(length(genesToPlot) > 0)
      {
        tcga = NULL
        for(g in genesToPlot){
          #tcga = rbind(tcga,read.table(paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/LFQ/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
          tcga = rbind(tcga,read.table(paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
        }
        df = melt(tcga,id.vars="gene")
        names(df) <- c("Gene", "UPN", "Expression")
        df <- merge(df, clinicaldata_lfq, by="UPN")
        dmlm <<- df
        g <- ggplot(df,aes(fill=Gene, y=Expression, x=Name, text = paste0("UPN ID: ",UPN,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
          theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"), axis.title=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 90, hjust = 1)) +
          ggtitle("Multiple protein view") + ylab("Expression") + xlab("")
        ggplotly(g, tooltip="text")
      }
    })
    output$downloadData_l_m <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmlm, file, quote = F, row.names = F, sep = "\t")}
    )
    # SUBTYPE #
    output$singlegene_plot_subtype_lfq <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_2_l)
      subtypesToPlot <- input$subtypes_l
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("M0","M1","M2","M3","M4","M5","Healthy CD34","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_lfq, clinicaldata_lfq$FAB %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(1,2,3,4,6)]
            colnames(finalmatrix) <- c("UPN","Expression","Name","TCGA_Name","FAB")
            finalmatrix$FAB <- factor(finalmatrix$FAB,levels=c("M0","M1","M2","M3","M4","M5","Healthy CD34","Healthy Lin-"))
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(1,3,4,5,2,6)]
            dmls <<- finalmatrix
            g <- ggplot(finalmatrix,aes(FAB, Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_Name))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"), axis.text.x = element_text(angle = 45)) + ylab("Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_l_s <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmls, file, quote = F, row.names = F, sep = "\t")}
    )
    # CYTOGENETICS #
    output$singlegene_plot_cytogenetics_lfq <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_3_l)
      subtypesToPlot <- input$cytogenetics_l
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("Poor","Good","Intermediate","Healthy CD34","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_lfq, clinicaldata_lfq$Cyto_Risk %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(1,2,3,4,7)]
            colnames(finalmatrix) <- c("UPN","Expression","Name","TCGA_Name","Cyto_risk")
            levels(finalmatrix$Cyto_risk) <- c("Favourable","Healthy CD34","Healthy Lin-","Intermediate","Adverse")
            finalmatrix$Cyto_risk <- factor(finalmatrix$Cyto_risk,levels=c("Favourable","Intermediate","Adverse","Healthy CD34","Healthy Lin-"))
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(1,3,4,5,2,6)]
            dmlc <<- finalmatrix
            g <- ggplot(finalmatrix,aes(Cyto_risk, Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_Name))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"), axis.text.x = element_text(angle = 45)) + ylab("Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_l_c <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmlc, file, quote = F, row.names = F, sep = "\t")}
    )
    # FUSION #
    output$singlegene_plot_fusion_lfq <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_4_l)
      subtypesToPlot <- input$fusion_l
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy CD34","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/LFQ/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_lfq, clinicaldata_lfq$Fusion %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(1,2,3,4,8)]
            colnames(finalmatrix) <- c("UPN","Expression","Name","TCGA_Name","Fusion")
            finalmatrix$Fusion <- factor(finalmatrix$Fusion,levels=c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy CD34","Healthy Lin-"))
            levels(finalmatrix$Fusion) <- c("CBFB-MYH11","RUNX1-RUNX1T1","PML-RARA","MLL-X","NUP98-NSD1","BCR-ABL","AML without Fusion","Healthy CD34","Healthy Donor Lin-")
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(1,3,4,5,2,6)]
            dmlf <<- finalmatrix
            g <- ggplot(finalmatrix,aes(Fusion, Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_Name))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"),axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45)) + ylab("Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_l_f <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmlf, file, quote = F, row.names = F, sep = "\t")}
    )
    # MUTATION #
    output$singlegene_plot_mutation_lfq <- renderPlotly({
      normalgenes <- toupper(input$single_gene_name_5_l)
      genesToPlot <- input$single_gene_name_6_l
      #normalgenes <- "DNMT3A"
      #genesToPlot <- "IDH"
      tcga = NULL
      if(normalgenes != "")
      {
        #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",normalgenes,".tsv")
        destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",normalgenes,".tsv")
        if(url.exists(destfile))
        {
          tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/LFQ/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/LFQ/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          tcga <- melt(tcga, id.vars=c("gene"))
          colnames(tcga) <- c("Gene","UPN","Expression")
          tcga$Gene <- genesToPlot
          tcga$Group <- "WT"
          upns_with_mut <- as.character(mutation_table$UPN[which(mutation_table$Gene %in% genesToPlot)])
          upns_with_health <- as.character(clinicaldata_lfq$UPN[which(clinicaldata_lfq$TCGA_Name %like% "Healthy")])
          tcga$Group[tcga$UPN %in% upns_with_mut] <- "MT"
          for(i in upns_with_health)
          {
            newvalue <- as.character(clinicaldata_lfq$FAB[clinicaldata_lfq$UPN == i])
            tcga$Group[tcga$UPN == i] <- newvalue
          }
          tcga_wt <- tcga[which(tcga$Group == "WT" ),c(2,1,3,4)]
          tcga_wt$Mutation <- paste0(genesToPlot," WT", sep = "")
          tcga_he <- tcga[!tcga$Group %in% c("WT","MT"),c(2,1,3,4)]
          tcga_he$Mutation <- tcga_he$Group
          tcga_mt <- tcga[which(tcga$Group == "MT" ),c(1,2,3)]
          finalmatrix <- merge(tcga_mt,mutation_table, by=c("UPN","Gene"))
          finalmatrix <- rbind(finalmatrix,tcga_wt,tcga_he)
          finaldata <- merge(finalmatrix, clinicaldata_lfq, by=c("UPN"))
          finaldata <- finaldata[,c(1,2,3,4,5,6,7)]
          names(finaldata) <- c("UPN","Gene","Expression","Group","Mutation","Name","TCGA")
          finaldata <- finaldata[,c(1,6,7,4,5,3,2)]
          finaldata$Gene <- normalgenes
          dmlw <<- finaldata
          g <- ggplot(finaldata,aes(Group, Expression, text = paste0("UPN ID: ",Name,"<br />Mutation type: ", Mutation,"<br />TCGA Sample ID: ", TCGA))) + 
            geom_quasirandom(size = 0.8) + 
            theme_bw() +
            ggtitle(paste0("Expression for ",normalgenes," with ",genesToPlot, ": WT|MT")) + 
            theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Expression") + xlab("")
          ggplotly(g, tooltip="text")
        }
      }
    })
    output$downloadData_l_w <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmlw, file, quote = F, row.names = F, sep = "\t")}
    )
    
    
    ###### Phospho APP STUFF ######
    # Updating the selectize options
    updateSelectizeInput(session, 'single_gene_name_1_p', choices = genenames_pho$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_2_p', choices = genenames_pho$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_3_p', choices = genenames_pho$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_4_p', choices = genenames_pho$V1, server = TRUE)
    # SUBTYPE #
    output$singlegene_plot_subtype_phospho <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_1_p)
      subtypesToPlot <- c("M0","M1","M2","M3","M4","M5","Healthy Lin-")
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("M0","M1","M2","M3","M4","M5","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$Phosphosite
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_pho, clinicaldata_pho$FAB %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            plottinggene <- melt(plottinggene, id.vars=c("UPN"))
            colnames(plottinggene) <- c("UPN","Phosphite","Expression")
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix$FAB <- factor(finalmatrix$FAB,levels=c("M0","M1","M2","M3","M4","M5","Healthy Lin-"))
            finalmatrix$Gene <- genesToPlot
            dmps <<- finalmatrix
            g <- ggplot(finalmatrix,aes(FAB, Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID,"<br />TCGA Sample Name: ", TCGA_Name))) + 
              geom_quasirandom(size = 0.8) + facet_wrap(~ Phosphite) +
              theme_bw() +
              ggtitle(paste0("Log2 Expression for the ",genesToPlot," phosphosite sites")) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
              ylab("Log2 Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_p_s <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmps, file, quote = F, row.names = F, sep = "\t")}
    )
    # CYTOGENETICS #
    output$singlegene_plot_cytogenetics_phospho <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_2_p)
      subtypesToPlot <- c("Poor","Good","Intermediate","Healthy Lin-")
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("Poor","Good","Intermediate","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$Phosphosite
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_pho, clinicaldata_pho$Cyto_Risk %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            plottinggene <- melt(plottinggene, id.vars=c("UPN"))
            colnames(plottinggene) <- c("UPN","Phosphite","Expression")
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            levels(finalmatrix$Cyto_Risk) <- c("Favourable","Healthy Lin-","Intermediate","Adverse")
            finalmatrix$Cyto_Risk <- factor(finalmatrix$Cyto_Risk,levels=c("Favourable","Intermediate","Adverse","Healthy Lin-"))
            finalmatrix$Gene <- genesToPlot
            dmpc <<- finalmatrix
            g <- ggplot(finalmatrix,aes(Cyto_Risk, Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID,"<br />TCGA Sample Name: ", TCGA_Name))) + 
              geom_quasirandom(size = 0.8) + facet_wrap(~ Phosphite) + 
              theme_bw() +
              ggtitle(paste0("Log2 Expression for the ",genesToPlot," phosphosite sites")) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
              ylab("Log2 Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_p_c <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmpc, file, quote = F, row.names = F, sep = "\t")}
    )
    # FUSION #
    output$singlegene_plot_fusion_phospho <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_3_p)
      subtypesToPlot <- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/Phospho/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$Phosphosite
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_pho, clinicaldata_pho$Fusion %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            plottinggene <- melt(plottinggene, id.vars=c("UPN"))
            colnames(plottinggene) <- c("UPN","Phosphite","Expression")
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix$Fusion <- factor(finalmatrix$Fusion,levels=c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Lin-"))
            levels(finalmatrix$Fusion) <- c("CBFB-MYH11","RUNX1-RUNX1T1","PML-RARA","MLL-X","NUP98-NSD1","BCR-ABL","AML without Fusion","Healthy Donor Lin-")
            finalmatrix$Gene <- genesToPlot
            dmpf <<- finalmatrix
            g <- ggplot(finalmatrix,aes(Fusion, Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID,"<br />TCGA Sample Name: ", TCGA_Name))) + 
              geom_quasirandom(size = 0.8) + facet_wrap(~ Phosphite) +
              theme_bw() +
              ggtitle(paste0("Log2 Expression for the ",genesToPlot," phosphosite sites")) + 
              theme(text=element_text(size=12, family="avenir", face="bold"),axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 90, hjust = 1)) +
              ylab("Log2 Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_p_f <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmpf, file, quote = F, row.names = F, sep = "\t")}
    )
    # MUTATION #
    output$singlegene_plot_mutation_phospho <- renderPlotly({
      normalgenes <- toupper(input$single_gene_name_4_p)
      genesToPlot <- input$single_gene_name_5_p
      #normalgenes <- "DNMT3A"
      #genesToPlot <- "IDH"
      tcga = NULL
      if(normalgenes != "")
      {
        #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",normalgenes,".tsv")
        destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",normalgenes,".tsv")
        if(url.exists(destfile))
        {
          tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/Phospho/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/Phospho/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          tcga <- melt(tcga, id.vars=c("Phosphosite"))
          colnames(tcga) <- c("Phosphosite","UPN","Expression")
          tcga$Group <- "WT"
          upns_with_mut <- mutation_table$UPN[which(mutation_table$Gene %in% genesToPlot)]
          upns_with_health <- as.character(clinicaldata_pho$UPN[which(clinicaldata_pho$TCGA_Name %like% "Healthy")])
          tcga$Group[tcga$UPN %in% upns_with_mut] <- "MT"
          for(i in upns_with_health)
          {
            newvalue <- as.character(clinicaldata_pho$FAB[clinicaldata_pho$UPN == i])
            tcga$Group[tcga$UPN == i] <- newvalue
          }
          tcga_wt <- tcga[which(tcga$Group == "WT" ),c(2,1,3,4)]
          tcga_wt$Mutation <- paste0(genesToPlot," WT", sep = "")
          tcga_wt$Gene <- genesToPlot
          tcga_wt <- tcga_wt[,c(1,6,2,3,4,5)]
          tcga_he <- tcga[!tcga$Group %in% c("WT","MT"),c(2,1,3,4)]
          tcga_he$Mutation <- tcga_he$Group
          tcga_he$Gene <- genesToPlot
          tcga_he <- tcga_he[,c(1,6,2,3,4,5)]
          tcga_mt <- tcga[which(tcga$Group == "MT" ),c(1,2,3)]
          tcga_mt$Gene <- genesToPlot
          finalmatrix <- merge(tcga_mt,mutation_table, by=c("UPN","Gene"))
          finalmatrix <- rbind(finalmatrix,tcga_wt,tcga_he)
          finaldata <- merge(finalmatrix, clinicaldata_pho, by=c("UPN"))
          dmpw <<- finaldata
          g <- ggplot(finaldata,aes(Group, Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID,"<br />TCGA Sample Name: ", TCGA_Name,"<br />Mutation: ", Mutation))) + 
            geom_quasirandom(size = 0.8) + facet_wrap(~ Phosphosite) + 
            theme_bw() +
            ggtitle(paste0("Expression for the ",genesToPlot," WT|MT at ",normalgenes," phosphosite sites")) + 
            theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
            ylab("Log2 Expression") + xlab("")
          ggplotly(g, tooltip="text")
        }
      }
    })
    output$downloadData_p_w <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmpw, file, quote = F, row.names = F, sep = "\t")}
    )
    
    ###### mRNA APP STUFF ######
    # Updating the selectize options
    updateSelectizeInput(session, 'single_gene_name_1_m', choices = genenames_mrna$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_2_m', choices = genenames_mrna$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_3_m', choices = genenames_mrna$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_4_m', choices = genenames_mrna$V1, server = TRUE)
    updateSelectizeInput(session, 'single_gene_name_5_m', choices = genenames_mrna$V1, server = TRUE)
    # MULTIPLOT #
    output$multigene_plot_mrna <- renderPlotly({
      genesToPlot <- input$single_gene_name_1_m
      #genesToPlot <- c("DNMT3A","DNMT1")
      if(length(genesToPlot) > 0)
      {
        tcga = NULL
        for(g in genesToPlot){
          #tcga = rbind(tcga,read.table(paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/mRNA/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
          tcga = rbind(tcga,read.table(paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/mRNA/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
        }
        df = melt(tcga,id.vars="gene")
        names(df) <- c("Gene", "UPN", "Log2.Expression")
        df <- merge(df, clinicaldata_mrna, by="UPN")
        dmmm <<- df
        g <- ggplot(df,aes(fill=Gene, y=Log2.Expression, x=UPN, text = paste0("UPN ID: ",UPN,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_bar(position="dodge", stat="identity") + theme_bw() +
          theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"), axis.title=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 90, hjust = 1)) +
          ggtitle("Multiple gene view") +
          ylab("Expression") + xlab("")
        ggplotly(g, tooltip="text")
      }
    })
    output$downloadData_m_m <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmmm, file, quote = F, row.names = F, sep = "\t")}
    )
    # SUBTYPE #
    output$singlegene_plot_subtype_mrna <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_2_m)
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("M0","M1","M2","M3","M4","M5","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
      subtypesToPlot <- input$subtypes_m
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/mRNA/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/mRNA/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_mrna, clinicaldata_mrna$FAB %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(2,3,4,5,6)]
            colnames(finalmatrix) <- c("Log2.Expression","Name","TCGA_ID","TCGA_Name","FAB")
            levels(finalmatrix$FAB) <- c("Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Monocytes","Healthy Donor Neutrophils","Healthy Donor Promylocytes","M0","M1","M2","M3","M4","M5")
            finalmatrix$FAB <- factor(finalmatrix$FAB,levels=c("M0","M1","M2","M3","M4","M5","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Monocytes","Healthy Donor Neutrophils","Healthy Donor Promylocytes"))
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(2,3,4,5,1,6)]
            dmms <<- finalmatrix
            g <- ggplot(finalmatrix,aes(FAB, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
              ylab("Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_m_s <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmms, file, quote = F, row.names = F, sep = "\t")}
    )
    # CYTOGENETICS #
    output$singlegene_plot_cytogenetics_mrna <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_3_m)
      subtypesToPlot <- input$cytogenetics_m
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("Poor","Good","Intermediate","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/mRNA/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/mRNA/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_mrna, clinicaldata_mrna$Cyto_Risk %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(2,3,4,5,7)]
            colnames(finalmatrix) <- c("Log2.Expression","Name","TCGA_ID","TCGA_Name","Cyto_risk")
            levels(finalmatrix$Cyto_risk) <- c("Favourable","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Monocytes","Healthy Donor Neutrophils","Healthy Donor Promylocytes","Intermediate","Adverse")
            finalmatrix$Cyto_risk <- factor(finalmatrix$Cyto_risk,levels=c("Favourable","Intermediate","Adverse","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Monocytes","Healthy Donor Neutrophils","Healthy Donor Promylocytes"))
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(2,3,4,5,1,6)]
            dmmc <<- finalmatrix
            g <- ggplot(finalmatrix,aes(Cyto_risk, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
              ylab("Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_m_c <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmmc, file, quote = F, row.names = F, sep = "\t")}
    )
    # FUSION #
    output$singlegene_plot_fusion_mrna <- renderPlotly({
      genesToPlot <- toupper(input$single_gene_name_4_m)
      subtypesToPlot <- input$fusion_m
      #genesToPlot <- "DNMT3A"
      #subtypesToPlot <- c("CBFB","RUNX1","PML","MLL","NSD1","BCR-ABL1","Normal","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Mono","Healthy Donor Neu","Healthy Donor Pro")
      tcga = NULL
      if(length(subtypesToPlot) > 0)
      {
        if(nchar(genesToPlot) > 0)
        {
          destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",genesToPlot,".tsv")
          #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",genesToPlot,".tsv")
          if(url.exists(destfile))
          {
            tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/mRNA/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/mRNA/",genesToPlot,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
            rownames(tcga) = tcga$gene
            tcga <- tcga[,-1]
            bysubtype <- filter(clinicaldata_mrna, clinicaldata_mrna$Fusion %in% subtypesToPlot)
            plottinggene <- tcga[,colnames(tcga) %in% bysubtype$UPN]
            plottinggene <- t(plottinggene)
            plottinggene <- as.data.frame(plottinggene)
            plottinggene$UPN <- rownames(plottinggene)
            rownames(plottinggene) <- NULL
            finalmatrix <- merge(plottinggene, bysubtype, by="UPN")
            finalmatrix <- finalmatrix[,c(2,3,4,5,8)]
            colnames(finalmatrix) <- c("Log2.Expression","Name","TCGA_ID","TCGA_Name","Fusion")
            levels(finalmatrix$Fusion) <- c("BCR-ABL","CBFB-MYH11","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Monocytes","Healthy Donor Neutrophils","Healthy Donor Promylocytes","MLL-X","AML without Fusion","NUP98-NSD1","PML-RARA","RUNX1-RUNX1T1")
            finalmatrix$Fusion <- factor(finalmatrix$Fusion,levels=c("CBFB-MYH11","RUNX1-RUNX1T1","PML-RARA","MLL-X","NUP98-NSD1","BCR-ABL","AML without Fusion","Healthy Donor CD19","Healthy Donor CD3","Healthy Donor CD34","Healthy Donor Monocytes","Healthy Donor Neutrophils","Healthy Donor Promylocytes"))
            finalmatrix$Gene <- genesToPlot
            finalmatrix <- finalmatrix[,c(2,3,4,5,1,6)]
            dmmf <<- finalmatrix
            g <- ggplot(finalmatrix,aes(Fusion, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />TCGA Sample ID: ", TCGA_ID))) + geom_quasirandom(size = 0.8) + theme_bw() +
              ggtitle(paste0("Expression for ",genesToPlot)) + 
              theme(text=element_text(size=12, family="avenir", face="bold"),axis.text=element_text(size=12, family="avenir", face="bold"), axis.text.x = element_text(angle = 45, hjust = 1)) +
              ylab("Expression") + xlab("")
            ggplotly(g, tooltip="text")
          }
        }
      }
    })
    output$downloadData_m_f <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmmf, file, quote = F, row.names = F, sep = "\t")}
    )
    # MUTATION #
    output$singlegene_plot_mutation_mrna <- renderPlotly({
      normalgenes <- toupper(input$single_gene_name_5_m)
      genesToPlot <- input$single_gene_name_6_m
      #normalgenes <- "A1BG"
      #genesToPlot <- "TP53"
      tcga = NULL
      if(normalgenes != "")
      {
        #destfile = paste0("../Preps for the apps/TCGA Proteomics data/Latest_dataset_nolog/",normalgenes,".tsv")
        destfile = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",normalgenes,".tsv")
        if(url.exists(destfile))
        {
          tcga = as.data.frame(read.table(file = paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/mRNA/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          #tcga = as.data.frame(read.table(file = paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/mRNA/",normalgenes,".tsv"), header = TRUE, stringsAsFactors = F,check.names=FALSE))
          tcga <- melt(tcga, id.vars=c("gene"))
          colnames(tcga) <- c("Gene","UPN","Expression")
          tcga$Gene <- genesToPlot
          tcga$Group <- "WT"
          upns_with_mut <- as.character(mutation_table$UPN[which(mutation_table$Gene %in% genesToPlot)])
          upns_with_health <- as.character(clinicaldata_mrna$UPN[which(clinicaldata_mrna$TCGA_Name %like% "Healthy")])
          tcga$Group[tcga$UPN %in% upns_with_mut] <- "MT"
          for(i in upns_with_health)
          {
            newvalue <- as.character(clinicaldata_mrna$FAB[clinicaldata_mrna$UPN == i])
            tcga$Group[tcga$UPN == i] <- newvalue
          }
          tcga_wt <- tcga[which(tcga$Group == "WT" ),c(2,1,3,4)]
          tcga_wt$Mutation <- paste0(genesToPlot," WT", sep = "")
          tcga_he <- tcga[!tcga$Group %in% c("WT","MT"),c(2,1,3,4)]
          tcga_he$Mutation <- tcga_he$Group
          tcga_mt <- tcga[which(tcga$Group == "MT" ),c(1,2,3)]
          finalmatrix <- merge(tcga_mt,mutation_table, by=c("UPN","Gene"))
          finalmatrix <- rbind(finalmatrix,tcga_wt,tcga_he)
          finaldata <- merge(finalmatrix, clinicaldata_mrna, by=c("UPN"))
          finaldata <- finaldata[,c(1,2,3,4,5,6,7)]
          names(finaldata) <- c("UPN","Gene","Log2.Expression","Group","Mutation","Name","TCGA")
          finaldata$Gene <- normalgenes
          dmmw <<- finaldata
          g <- ggplot(finaldata,aes(Group, Log2.Expression, text = paste0("UPN ID: ",Name,"<br />Mutation type: ", Mutation,"<br />TCGA Sample Name: ", TCGA))) + 
            geom_quasirandom(size = 0.8) + 
            theme_bw() +
            ggtitle(paste0("Expression for ",normalgenes," with ",genesToPlot, ": WT|MT")) + 
            theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=12, family="avenir", face="bold"),axis.text.x = element_text(angle = 45, hjust = 1)) +
            ylab("Expression") + xlab("")
          ggplotly(g, tooltip="text")
        }
      }
    })
    output$downloadData_m_w <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmmw, file, quote = F, row.names = F, sep = "\t")}
    )
    
    ###### TMT vs mRNA STUFF ######
    # Updating the selectize options
    updateSelectizeInput(session, 'single_gene_name_1_pm', choices = genenames_corr$V1, server = TRUE)
    # MULTIPLOT #
    output$multigene_plot_protein <- renderPlotly({
      genesToPlot <- input$single_gene_name_1_pm
      #genesToPlot <- c("DNMT3A","DNMT1")
      #genesToPlot <- c("DNMT3A")
      #genesToPlot <- c("U2AF1")
      #genesToPlot <- c("TP53")
      if(length(genesToPlot) > 0)
      {
        tcga1 = NULL
        tcga2 = NULL
        for(g in genesToPlot){
          #tcga1 = rbind(tcga1,read.table(paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/TMT/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
          tcga1 = rbind(tcga1,read.table(paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/TMT/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
        }
        for(g in genesToPlot){
          #tcga2 = rbind(tcga2,read.table(paste0("../AML_proteomics-tmp/TCGA_Proteomics_app/Gene_files/mRNA/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
          tcga2 = rbind(tcga2,read.table(paste0("https://storage.googleapis.com/tcga_shiny/TCGA_Proteomics_app/Gene_files/mRNA/",g,".tsv"),sep="\t",stringsAsFactors=F,header=T,check.names=FALSE))
        }
        names1 <- colnames(tcga1)[-1]
        names2 <- colnames(tcga2)[-1]
        unique_names <- intersect(names1,names2)
        df1 = melt(tcga1,id.vars="gene")
        names(df1) <- c("Gene", "UPN", "Value")
        df1$study <- "TMT"
        df2 = melt(tcga2,id.vars="gene")
        names(df2) <- c("Gene", "UPN", "Value")
        df2$study <- "mRNA"
        df1 <- df1[df1$UPN %in% unique_names,]
        df2 <- df2[df2$UPN %in% unique_names,]
        main_df1 <- merge(df1, clinicaldata_tmt, by="UPN")[,c(1:4,8:10)]
        names(main_df1) <- c("UPN_tmt","Gene_tmt","Value_tmt","study_tmt","FAB_tmt","Cyto_Risk_tmt","Fusion_tmt")
        main_df2 <- merge(df2, clinicaldata_mrna, by="UPN")[,c(1:4,8:10)]
        names(main_df2) <- c("UPN_mrna","Gene_mrna","Value_mrna","study_mrna","FAB_mrna","Cyto_Risk_mrna","Fusion_mrna")
        main_df <- cbind(main_df1,main_df2)
        main_df$correlation <- 0
        for (i in genesToPlot)
        {
          main_df$correlation[main_df$Gene_tmt == i] <- genenames_corr$V2[genenames_corr$V1 == i]
        }
        main_df$facet <- paste(main_df$Gene_tmt," (Spearman correlation: ",main_df$correlation,")",sep = "")
        main_df <- main_df[,c(1,2,3,10,5,6,7,15,16)]
        names(main_df) <- c("UPN","Gene","Log2_Expression","Abundance","FAB","Cyto_Risk","Fusion","Correlation","Facet")
        main_df <- as.data.frame(main_df)
        sp_correlation <- genenames_corr[genenames_corr$V1 %in% genesToPlot,]
        names(sp_correlation) <- c("Gene","Correlation")
        rownames(sp_correlation) <- NULL
        dmab <<- sp_correlation
        g1 <- ggplot(main_df,aes(x=Log2_Expression, y=Abundance)) + geom_point() + 
          theme_bw() + theme(text=element_text(size=12, family="avenir", face="bold"), axis.text=element_text(size=10, family="avenir", face="bold"),  axis.title=element_text(size=12, family="avenir", face="bold"), axis.text.x = element_text(angle = 0, hjust = 1)) +
          ylab("mRNA Abundance") + xlab("Protein Abundance (TMT: log2)") +
          facet_wrap(~ Facet, scales = "free")
        ggplotly(g1, tooltip="text")
        
      }
    })
    output$downloadData_pm_m <- downloadHandler(
      filename = function() {paste0("Output.csv")},
      content = function(file) {write.csv(dmab, file, quote = F, row.names = F, sep = ",")}
    )
  }
)
library(shiny)
library(shinyjs)
# library(shinythemes)
library(plotly)
library(ggplot2)
library(DBI)


# database
database <- dbConnect(RSQLite::SQLite(), "Proteomics.db")
print("Connected to database")

# database vars
# more effcient this way... choices are only loaded once per server rather than every client 
tmt_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='TMT';"))
# lfq_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='LFQ';"))
# phospho_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='Phosphosite';"))
# mrna_choices <- unique(dbGetQuery(database, "SELECT Gene FROM Genes WHERE Type='mRNA';"))
# gene correlations file for vs 

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
      
      selectizeInput(inputId = "genes",
                     label = "Protein(s) to plot",
                     choices = NULL)
      
    ),
    mainPanel(
      plotlyOutput("plot",height = "500px")
    )
  )
)

server <- function(input, output,session) {
  observeEvent(input$type, {
    if(input$type == "Protein vs mRNAs") {
      shinyjs::hide(id = "subtype")
    }
    else {
      shinyjs::show(id = "subtype")
      subtype <- switch(
        input$type,
        "Protein vs mRNAs" = NULL,
        "Phosphosite" = c("Subtype", "Cytogenetics", "Fusions", "Mutation"),
        c("Multiplot","Subtype", "Cytogenetics", "Fusions", "Mutation")
      )
      updateSelectizeInput(session, 'subtype', choices = subtype, server = TRUE)
    }
  })
  updateSelectizeInput(session, 'genes', choices = tmt_choices$Gene, server = TRUE)
  
  output$plot <- renderPlotly({
    # Multiplot
    
  })
  
  
}

shinyApp(ui = ui, server = server)
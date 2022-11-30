server <- function(input, output,session) {

  # Handle event when user selects dataset
  # Changes gene list and available types of plots based on the dataset
  observeEvent(input$dataset, {
    # TODO: Add update subtype when we get other subtypes
    source(paste0("configs/",input$dataset,".R"), local = TRUE)
    print(paste0("configs/",input$dataset,".R"))
    updateSelectInput(session, "subtype",choices = available_plots)
  })
}

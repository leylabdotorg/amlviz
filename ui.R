ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Select a dataset",
        choices = c("",dataset$Study_ID),
        selected = NULL,
        multiple = FALSE
      ),
      # All elements are hidden at start because they will vary based on dataset
      shinyjs::hidden(
        # Dropdown to select which plot you want based on dataset
        selectizeInput(
          inputId = "subtype",
          label = "Select an option",
          choices = NULL,
          multiple = FALSE,
          selected = ""
        ),
        # Dropdown to select genes
        selectizeInput(
          inputId = "genes",
          label = "Multigene plot",
          choices = NULL,
          multiple = TRUE,
          selected = NULL
        ),
        # Dropdown to select gene
        selectizeInput(
          inputId = "gene",
          label = "Gene to plot",
          choices = NULL,
          multiple = FALSE,
          selected = NULL
        ),
        # Checkbox to select subtype options
        checkboxGroupInput(
          inputId = "subtype_options",
          label = NULL,
          choices = NULL
        ),
        # Dropdown specifically for Mutation
        selectizeInput(
          inputId = "mutation_status",
          label = "Mutation Status",
          choices = NULL,
          multiple = FALSE,
          selected = NULL
        )
      )
      ),
    mainPanel(
      plotlyOutput("plot", height = "500px")
    )
  )
)

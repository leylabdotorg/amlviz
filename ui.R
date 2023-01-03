ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Select a dataset",
        choices = c("",dataset$Short_hand_code),
        selected = NULL,
        multiple = FALSE
      ),
      # All elements are hidden at start because they will vary based on dataset
      shinyjs::hidden(
        # Dropdown to select which plot you want based on dataset
        selectInput(
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
          selected = ""
        ),
        # Dropdown to select gene
        selectizeInput(
          inputId = "gene",
          label = "Gene to plot",
          choices = NULL,
          multiple = FALSE,
          selected = ""
        ),
        # Checkbox to select subtype options
        checkboxGroupInput(
          inputId = "subtype_options",
          label = NULL,
          choices = character(0)
        ),
        # Dropdown specifically for Mutation
        selectizeInput(
          inputId = "mutation_status",
          label = "Mutation Status",
          choices = character(0)
        )
      )
      ),
    mainPanel(
      plotlyOutput("plot", height = "500px")
    )
  )
)

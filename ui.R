ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Select a dataset",
        choices = dataset$Short_hand_code
      ),
      # Dropdown to select which plot you want based on dataset
      selectizeInput(
        inputId = "subtype",
        label = "Select an option",
        choices = c("Multiplot","FAB", "Cyto_risk", "Fusion", "Mutations"),
        multiple = FALSE
      ),
      conditionalPanel(
        condition = "input.subtype == 'Multiplot'",
        # Dropdown to select genes
        selectizeInput(
          inputId = "genes",
          label = "Multigene plot",
          choices = NULL,
          multiple = TRUE,
          selected = NULL
        )
      ),
      conditionalPanel(
        condition = "!(input.subtype == 'Multiplot')",
        # Dropdown to select gene
        selectizeInput(
          inputId = "gene",
          label = "Gene to plot",
          choices = NULL,
          multiple = FALSE,
          selected = NULL
        )
      ),
      conditionalPanel(
        condition = "!(input.subtype == 'Multiplot' || input.subtype == 'Mutations')",
        # Checkbox to select subtype options
        checkboxGroupInput(
          inputId = "subtype_options",
          label = NULL,
          choices = NULL
        )
      ),
      conditionalPanel(
        condition = "input.subtype == 'Mutations'",
        # Dropdown specifically for mutations
        selectInput(
          inputId = "mutation_status",
          label = "Mutation Status",
          choices = mutation_specific_genes$V1
        )
      )
    ),
    mainPanel(
      plotlyOutput("plot", height = "500px")
    )
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Select a dataset",
        choices = c("",as.character(dataset$Short_hand_code)),
        selected = NULL,
        multiple = FALSE
      ),
      # Dropdown to select which plot you want based on dataset
      conditionalPanel(
        condition = "input.dataset != ''",
        selectInput(
          inputId = "subtype",
          label = "Select an option",
          choices = NULL,
          multiple = FALSE,
          selected = character(0)
        )
      ),
      conditionalPanel(
        condition = "input.subtype == 'Multiplot' && input.subtype != ''",
        # Dropdown to select genes
        selectizeInput(
          inputId = "genes",
          label = "Multigene plot",
          choices = NULL,
          multiple = TRUE,
          selected = character(0)
        )
      ),
      conditionalPanel(
        condition = "!(input.subtype == 'Multiplot') && input.subtype != ''",
        # Dropdown to select gene
        selectizeInput(
          inputId = "gene",
          label = "Gene to plot",
          choices = NULL,
          multiple = FALSE,
          selected = character(0)
        )
      ),
      conditionalPanel(
        condition = "!(input.subtype == 'Multiplot' || input.subtype == 'Mutation') && input.subtype != ''",
        # Checkbox to select subtype options
        checkboxGroupInput(
          inputId = "subtype_options",
          label = NULL,
          choices = character(0)
        )
      ),
      conditionalPanel(
        condition = "input.subtype == 'Mutations'",
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

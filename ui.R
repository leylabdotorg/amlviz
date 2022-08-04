ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select dataset
      selectInput(
        inputId = "type",
        label = "Select a dataset",
        choices = c("TMT", "LFQ", "Phosphosite", "mRNA", "Protein vs mRNAs")
      ),
      conditionalPanel(
        condition = "!(input.type == 'Protein vs mRNAs')",
        # Dropdown to select which plot you want based on dataset
        selectizeInput(
          inputId = "subtype",
          label = "Select an option",
          choices = NULL,
          multiple = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.subtype == 'Multiplot' || input.type == 'Protein vs mRNAs'",
        # Dropdown to select genes
        selectizeInput(
          inputId = "genes",
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          selected = NULL
        )
      ),
      conditionalPanel(
        condition = "!(input.subtype == 'Multiplot' || input.type == 'Protein vs mRNAs')",
        # Dropdown to select gene
        selectizeInput(
          inputId = "gene",
          label = NULL,
          choices = NULL,
          multiple = FALSE,
          selected = NULL
        )
      ),
      conditionalPanel(
        condition = "!(input.subtype == 'Multiplot' || input.subtype == 'Mutations' || input.type == 'Phosphosite' ||input.type == 'Protein vs mRNAs')",
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

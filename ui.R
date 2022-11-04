ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Select a dataset",
        choices = c("beat_aml", "tcga_aml", "Proteomics") # TODO: Make dynamic based on db structure
      ),
      conditionalPanel(
        #condition = "!(input.subset == 'Protein vs mRNAs')",
        condition = "(input.subset != 'Protein vs mRNAs') || (input.dataset != 'Proteomics')",
        # Dropdown to select which plot you want based on dataset
        selectizeInput(
          inputId = "subtype",
          label = "Select an option",
          choices = NULL,
          multiple = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.subtype == 'Multiplot' || input.subset == 'Protein vs mRNAs'",
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
        condition = "!(input.subtype == 'Multiplot' || input.subset == 'Protein vs mRNAs')",
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
        condition = "!(input.subtype == 'Multiplot' || input.subtype == 'Mutations' || input.subset == 'Phosphosite' ||input.subset == 'Protein vs mRNAs')",
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

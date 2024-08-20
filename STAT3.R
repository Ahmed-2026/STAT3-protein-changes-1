# Load necessary libraries
library(shiny)
library(dplyr)
library(maftools)

# Define the initial dataset with equal lengths for each column
id <- c('NKWGS:B02', 'NKWGS:F02', 'NKWGS:F02', 'NKWGS:G01', 'PMC5419584:1235', 'PMC5419584:1256', 'PMC5419584:1265', 'PMC5419584:126', 'PMC5419584:246', 'TEMPUS:1474',
        'TEMPUS:1499', 'TEMPUS:1544', 'TEMPUS:1715', 'TEMPUS:1718', 'TEMPUS:1746', 'TEMPUS:1796', 'TEMPUS:1798', 'TEMPUS:1849', 'TEMPUS:1888', 'TEMPUS:1917',
        'TEMPUS:1918', 'TEMPUS:1970', 'TEMPUS:1974', 'TEMPUS:1985', 'TEMPUS:2003', 'TEMPUS:2012', 'TEMPUS:2020', 'TEMPUS:2022', 'TEMPUS:2037', 'TEMPUS:2042',
        'TEMPUS:2049', 'TEMPUS:2051', 'TEMPUS:2056', 'TEMPUS:2062', 'TEMPUS:2074', 'TEMPUS:2077', 'TEMPUS:2081', 'TEMPUS:2082', 'TEMPUS:2084', 'TEMPUS:2090',
        'TEMPUS:2090', 'TEMPUS:2095', 'TEMPUS:2100', 'TEMPUS:2103', 'TEMPUS:2105', 'TEMPUS:2108', 'TEMPUS:2110', 'TEMPUS:2126', 'TEMPUS:2127', 'TEMPUS:2139',
        'TEMPUS:2139', 'TEMPUS:2144', 'TEMPUS:2147', 'TEMPUS:2151', 'TEMPUS:2152', 'TEMPUS:2152', 'TEMPUS:2157', 'TEMPUS:2157', 'TEMPUS:2164', 'TEMPUS:2174',
        'TEMPUS:2180', 'TEMPUS:2190', 'TEMPUS:2203', 'TEMPUS:2205', 'TEMPUS:2207', 'TEMPUS:2207', 'TEMPUS:2208', 'TEMPUS:2214', 'TEMPUS:2225', 'TEMPUS:2227',
        'TEMPUS:2228', 'TEMPUS:2249', 'TEMPUS:2251', 'TWGS:A04', 'TWGS:A05', 'TWGS:A06', 'TWGS:B03', 'TWGS:B04', 'TWGS:B04', 'TWGS:B05', 'TWGS:C03',
        'TWGS:C03', 'TWGS:D03', 'TWGS:D04', 'TWGS:D06', 'TWGS:E01', 'TWGS:E03', 'TWGS:E04', 'TWGS:F04', 'TWGS:F05', 'TWGS:G02', 'TWGS:G04', 'TWGS:H02',
        'TWGS:H04', 'TWGS:H05', 'TWGS:H06', 'TWGS:H06')
Hugo_Symbol <- rep('STAT3', length(id))
Variant_Classification <- rep('Missense_Mutation', length(id))
Protein_Change <- c(
  'p.N647I', 'p.D661V', 'p.D661N', 'p.K658R', 'p.H410R', 'p.Y640F', 'p.Y640F', 'p.Y640F', 'p.D661V', 'p.Y640F',
  'p.Y640F', 'p.V667L', 'p.Y640F', 'p.N647I', 'p.Y640F', 'p.Y640F', 'p.Y640F', 'p.Y640F', 'p.D661Y', 'p.D661Y',
  'p.D661Y', 'p.N647I', 'p.N647I', 'p.Y640F', 'p.Y640F', 'p.D661Y', 'p.Y640F', 'p.Y640F', 'p.Y640F', 'p.Y640F',
  'p.Y640F', 'p.D661Y', 'p.Y640F', 'p.D661Y', 'p.Y640F', 'p.N647I', 'p.H410R', 'p.Y640F', 'p.Y640F', 'p.658_659KI>NL',
  'p.I653I', 'p.Y640F', 'p.S614R', 'p.Y640F', 'p.Y640F', 'p.D661V', 'p.Q160P', 'p.Y640F', 'p.Y640F', 'p.D661V',
  'p.I659L', 'p.N647I', 'p.D661Y', 'p.D661Y', 'p.Y640F', 'p.D170Y', 'p.K658R', 'p.L287F', 'p.Y640F', 'p.Y640F',
  'p.N647I', 'p.Y640F', 'p.S614R', 'p.Y640F', 'p.D661Y', 'p.D661Y', 'p.F174S', 'p.N647I', 'p.Y640F', 'p.D661Y',
  'p.Y640F', 'p.Q643H', 'p.Y640F', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y',
  'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y',
  'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y', 'p.D661Y')
# Ensure Protein_Change matches the length of id
Protein_Change <- rep(Protein_Change, length.out = length(id))
disease <- c('NK', 'NK', 'NK', 'NK', 'T', 'T', 'T', 'NA', 'T', 'T',
             'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'GD', 'NA',
             'T', 'T', 'T', 'NK', 'NK', 'T', 'T', 'T', 'T', 'T', 'T',
             'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T',
             'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T',
             'T', 'T', 'T', 'T', 'T', 'T', 'NK', 'T', 'T', 'T',
             'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T',
             'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T',
             'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T')
# Ensure disease matches the length of id
disease <- rep(disease, length.out = length(id))
# Create the data frame
data <- data.frame(
  id = id,
  Hugo_Symbol = Hugo_Symbol,
  Variant_Classification = Variant_Classification,
  Protein_Change = Protein_Change,
  disease = disease
)

# Add random values for the MAF fields
set.seed(123) # For reproducibility
data <- data %>%
  mutate(
    Chromosome = sample(c('2', '17'), n(), replace = TRUE),
    Start_Position = sample(1000000:5000000, n(), replace = TRUE),
    End_Position = Start_Position + sample(1:100, n(), replace = TRUE),
    Reference_Allele = sample(c('A', 'T', 'C', 'G'), n(), replace = TRUE),
    Tumor_Seq_Allele2 = sample(c('A', 'T', 'C', 'G'), n(), replace = TRUE),
    Variant_Type = ifelse(Variant_Classification == 'Missense_Mutation', 'SNP', 'InDel'), # Infer Variant_Type
    Tumor_Sample_Barcode = id # Use the id as Tumor_Sample_Barcode
  )

# Define the user interface
ui <- fluidPage(
  titlePanel("STAT3 Protein Changes"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("allow_multiple", "Allow multiple disease selection", value = FALSE),
      uiOutput("disease_ui"),
      fluidRow(
        column(6, downloadButton("download_data", "Download Data")),
        column(6, actionButton("show_tutorial", "Show Tutorial"))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("lollipopPlot", height = "600px")),
        tabPanel("Data", tableOutput("data_table"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Show tutorial modal when the button is clicked
  observeEvent(input$show_tutorial, {
    showModal(modalDialog(
      title = "Tutorial",
      p("Welcome to the STAT3 Protein Changes app"),
      p("This app allows you to explore protein changes across different diseases."),
      p("Use the sidebar to select diseases"),
      p("You can choose multiple diseases if you enable the checkbox."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Render the UI for disease selection based on the checkbox
  output$disease_ui <- renderUI({
    if (input$allow_multiple) {
      selectizeInput("disease", "Select Disease:",
                     choices = c("All", unique(data$disease)),
                     selected = "All",
                     multiple = TRUE,
                     options = list(plugins = list('remove_button')))
    } else {
      selectInput("disease", "Select Disease:",
                  choices = c("All", unique(data$disease)),
                  selected = "All",
                  multiple = FALSE)
    }
  })
  
  # Reactive expression to filter data based on selected diseases
  filtered_data <- reactive({
    req(input$disease)
    if ("All" %in% input$disease) {
      data
    } else {
      data[data$disease %in% input$disease, ]
    }
  })
  
  # Convert filtered data to MAF object
  maf_data <- reactive({
    read.maf(maf = filtered_data())
  })
  
  # Render the lollipop plot with mutation counts
  output$lollipopPlot <- renderPlot({
    # Create the lollipop plot
    lollipopPlot(
      maf = maf_data(),
      gene = "STAT3",
      AACol = "Protein_Change",
      showDomainLabel = TRUE
    )
    
    # Calculate mutation counts
    mutation_counts <- table(data$Protein_Change)
    positions <- as.numeric(gsub("p\\.(\\d+)[A-Z]", "\\1", names(mutation_counts)))
    
    # Normalize the text size based on mutation counts
    max_count <- max(mutation_counts)
    text_sizes <- 0.8 * (mutation_counts / max_count)
    
    # Add text labels above each mutation position
    text(x = positions, y = mutation_counts + 1, labels = mutation_counts, cex = text_sizes, col = "red")
  })
  
  # Render the data table
  output$data_table <- renderTable({
    filtered_data()
  })
  
  # Download handler for the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("stat3_protein_changes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

library(shiny)
library(shinydashboard)

##v3+

# Define UI using fluidPage
ui <- dashboardPage(
  
  skin = "green",
  dashboardHeader(title = "Conservation Priorities Tool", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      id="tabs",
      menuItem("Parcel Selection", tabName="parcels", icon=icon("tasks")),
      menuItem("Data Selection", tabName = "data", icon = icon("tasks")),
      menuItem("Model Weights", tabName = "weights", icon = icon("dashboard")),
      menuItem("Results", tabName = "parcel", icon = icon("th")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("stats", lib = "glyphicon")),
      menuItem("Environmental Layers", tabName = "inputsel", icon = icon("creative-commons-share"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      ##Parcel selection page---------------------
      tabItem(tabName= "parcels",
              
              fluidRow(
                tags$div(style = "margin-left: 20px;", h3("Parcel Options")),
                hr(style = "border-top: 1px solid #000000;"),
              ),
              
              
              fluidRow(
                column(4,
                       h4("File Upload"),
                       fileInput("shp", "Input Parcel shapefile (.shp, .gpkg)",
                                 width="100%", 
                                 accept=c(".shp",".gpkg"), multiple=F),   
                       actionButton("printParcels2", h5(strong("Generate Parcels from File"))),
                ),
              ),
              
              
              ###Leaflet
              fluidRow(
                br(),
                br(),
                br(),
                leafletOutput("m"),
                br(),
                actionButton("printParcels", h5(strong("Generate Parcels from Shape"))),
              ),
              
          
      ),
      
      
      ##Data Selection page-----------------------
      tabItem(
        tabName = "data",
        fluidRow(
          column(12,
                 box(title = "Raster Options", status = "success", solidHeader = TRUE, width = 12,
                     uiOutput("rasterOptions")),
          ),
          actionButton("goButton", "Go!", class = "btn-success"),
        ),
      ),
      
     
      
      ##Model Weights page--------------------------
      tabItem(
        tabName = "weights",
        navset_card_underline(
          nav_panel(
            tags$b("Positive Influence Layers"), 
            ##
            fluidRow(
              column(4, uiOutput("positives")), # Adjust the width as needed
                column(4, plotOutput("betashapeplotpos", height = posplotsize)),
              column(4, plotOutput("minirasterzpos", height = negplotsize)) # Adjust the width as needed
            )
          ),
          
          nav_panel(
            tags$b("Negative Influence Layers"), 
            fluidRow(
              column(4, uiOutput("negatives")), # Adjust the width as needed
              column(4, plotOutput("betashapeplotneg", height = negplotsize)), # Adjust the width as needed
              column(4, plotOutput("minirasterzneg", height = negplotsize)) # Adjust the width as needed
             )
        ),
        ),
        fluidRow(
          br(),
          br(),
          br(),
          column(
            12,
            actionButton("makemodel", label = "Run Model Scenario", class = "btn-success"),
            
            br(),
            br(),
            verbatimTextOutput("value")
          )
        )
      ),
      
      
      
      
      ## Results page-----------------------------
      tabItem(
        tabName = "parcel",
        fluidRow(
          column(
            2,
            checkboxInput("showparcels", "Show Parcels", TRUE)
          ),
          column(
            2,
            sliderInput(
              "sitewt",
              "Minimum Model Weight",
              min = -1,
              max = 1,
              step = 0.1,
              value = 0
            )
          )
        ),
        fluidRow(
          column(
            2,
            downloadButton("dls", label = "Download Selected", class = NULL, icon = shiny::icon("download"))
          ),
          column(
            2,
            downloadButton("dlm", label = "Download Model", class = NULL, icon = shiny::icon("download"))
          ),
          column(
            2,
            downloadButton("dlp", label = "Download Parameters", class = NULL, icon = shiny::icon("download"))
          )
        ),
        # fluidRow(plotOutput('plot1', height = 700))
        leafletOutput("modelp1")
        
      ),
      
      ##Diagnostics page-------------------------------
      tabItem(
        tabName = "diagnostics",
        plotOutput('plot2')
      ),
      
      ##Environmental Layers page-------------------------
      ##Environmental Layers page-------------------------
      tabItem(
        tabName = "inputsel",
        fluidRow(
          column(12,
                 selectInput(
                   inputId = 'whichlayer',  # Ensure the inputId is correctly set
                   label = 'Environmental Layer',
                   choices = NULL  # Initialized with NULL, updated by server
                 )
          ),
          leafletOutput('plot3')  # Ensure the plot is correctly set to render based on the selected layer
        )
      )
      
      
      
    )
  )
)

# server <- function(input, output) {
#   # Your server logic goes here
# }

# shinyApp(ui, server)


################################################################################
#                                                                              #
#              UNFPA CENCUS TRACKER APP By Derrick DEMEVENG                    #
#                                                                              #
################################################################################

# SUMMARY -----------------------------------------------------------------


# LOAD PACKAGES -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # install pacman if not yet

pacman::p_load(
  shiny,
  bslib,
  sf,
  lwgeom
)

source("helper_functions.R")

# LOAD DATA ---------------------------------------------------------------
census_tracker_data <- readxl::read_xlsx("data/UNFPA_global_census_2022_round.xlsx")
admin0 <- sf::read_sf("data/UN_Geodata_simplified/admin0.shp")

# APP UI ------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Customize as needed
  
  # HEADER ----------------------------------------------------------------
  fluidRow(
    column(
      width = 2,
      tags$img(src = "UNFPA_logo.png", height = "70px")  # Place your logo in www/
    ),
    column(
      width = 10,
      h2("CENSUS TRACKER", style = "margin-top: 10px;")
    )
  ),
  tags$hr(),
  
  # BODY ------------------------------------------------------------------
  sidebarLayout(
    #sidebar
    sidebarPanel(
      selectInput("country", "Country", choices = c("Kenya", "Ghana", "Nigeria")),
      selectInput("year", "Year", choices = c("2020", "2021", "2022")),
      width = 2  # Sidebar width
    ),
    # map and table
    mainPanel(
      # key indicators
      fluidRow(
      ),
      # tabs
      tabsetPanel(
        # map
        tabPanel("Census Implementation",
                 div(leaflet::leafletOutput("mapPlot", height = "800px")),
                 div(shiny::plotOutput("barPlot", height = "800px")),
                 div(shiny::plotOutput("heatPlot", height = "800px")),
                 style = "overflow-y: auto; height: 100%;"
                 ),
        # table
        tabPanel("Civil Registration", 
                 div(DT::dataTableOutput("dataTable")),
                 style = "overflow-y: auto; height: 800px;"  # Scroll only within the table tab
                 )
      ),
      width = 10,  # Main panel width
      #style = "height: 900px; overflow: hidden;"  # Fixed height panel
    )
  )
)


# APP SERVER --------------------------------------------------------------
server <- function(input, output){
  # ------------- Census Section
  # Map
  output$mapPlot <- renderLeaflet({
    interactive_map(census_tracker_data, admin0)
  })
  
  output$barPlot <- renderPlot({
    stack_bar_census(census_tracker_data)
  })
  
  output$heatPlot <- renderPlot({
    census_heatmap(census_tracker_data)
  })
  # ------------- Civil Registration Section
  # DT table
  output$dataTable <- DT::renderDataTable({
    interactive_table(census_tracker_data)
  })
}

# APP RUN -----------------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)


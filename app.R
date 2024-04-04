library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
#library(leaflet.esri)
library(shinydashboard)
#options(shiny.maxRequestSize=100*1024^2) 
source('functions/validate_features.R')
spot = "https://mapservices.gov.yk.ca/imagery/rest/services/Satellites/Satellites_MedRes_Update/ImageServer"
google = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G"

# Define UI for application
ui = dashboardPage(
  dashboardHeader(title = "Disturbance Validation"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("View disturbances", tabName = "fri", icon = icon("th"))
    ),
         fileInput("gpkgFile", "Upload geopackage file:", accept = c(".gpkg")),
         #selectInput("layer", "Choose layer:", choices = NULL),
         #textInput("bnd", "Study area layer:", value=NULL),
         selectInput("line", "Linear disturbances:", choices = NULL),
         selectInput("poly", "Areal disturbances:", choices = NULL),
         textInput("output", "Output directory:", value = 'output'),
         hr(),
         actionButton("goButton", "Validate results")
         #hr(),
         #selectInput("fill", "Choose color:", choices=c('orange','red','blue','green'), selected='orange'),
         #sliderInput("alpha", label = "Choose opacity:", min = 0, max = 1, value = 0.5)
      ),
  dashboardBody(
    tabItems(
        tabItem(tabName="fri",
            fluidRow(
                tabBox(id = "one", width="12",
                  tabPanel("Linear summary", verbatimTextOutput("linearText")),
                  tabPanel("Linear errors", DTOutput("linearTable")),
                  tabPanel("Areal summary", verbatimTextOutput("arealText")),
                  tabPanel("Areal errors", DTOutput("arealTable"))
                )
            )
        )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$gpkgFile, {
    file <- input$gpkgFile$datapath
    gpkg <- st_read(file)
    lyrs <- st_layers(file)$name
    updateSelectInput(session, "layer", choices = lyrs)
    updateSelectInput(session, "bnd", selected = lyrs[1])
    updateSelectInput(session, "line", choices = lyrs, selected = 'linear_disturbance')
    updateSelectInput(session, "poly", choices = lyrs, selected = 'areal_disturbance')
  })

  output$map <- renderLeaflet({
    if (!is.null(input$gpkgFile)) {
      if(!is.null(input$layer)) {
         bnd <- st_read(input$gpkgFile$datapath, layer = input$bnd) %>%
          st_transform(4326)
         lyr <- st_read(input$gpkgFile$datapath, layer = input$layer) %>%
          st_transform(4326)
         m <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
            addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
            addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
            addTiles(google, group="Google.Imagery") %>%
            addEsriImageMapLayer(url=spot, group="SPOT.Imagery") %>%
            addScaleBar(position='bottomleft') %>%
            addPolygons(data = bnd, color='black', fill=F, weight=2, group="Study area")
            if (input$layer=='linear_disturbance') {
              m <- m %>% addPolylines(data=lyr, color=input$fill, weight=2, group=input$layer)
            } else {
              m <- m %>% addPolygons(data = lyr, stroke=T, weight=1, color='black', fillColor=input$fill, fillOpacity=input$alpha, group=input$layer)
            }
          m <- m %>% addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
            overlayGroups = c("Study area", input$layer),
            options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup('')
      }
  }
  })

  output$table <- renderDT({
    if (!is.null(input$gpkgFile)) {
      layer <- input$layer
      if(!is.null(layer)) {
         data <- st_read(input$gpkgFile$datapath, layer = layer) %>%
            st_drop_geometry()
         datatable(data)
      }
  }
  })

  observeEvent(input$goButton, {
    validate_features(data_pkg=input$gpkgFile$datapath, lyr_line=input$line, lyr_poly=input$poly, output_dir=input$output)
  })

  output$linearTable <- renderDT({
    #validate_project(data_pkg=input$gpkgFile$datapath, lyr_line=input$line, lyr_poly=input$poly, output_dir=input$output)
    req(input$goButton)
    x <- readr::read_csv(paste0(input$output,'/linear_types_validation.csv'))
    datatable(x)
  })

  output$arealTable <- renderDT({
    req(input$goButton)
    x <- readr::read_csv(paste0(input$output,'/areal_types_validation.csv'))
    datatable(x)
  })

  output$linearText <- renderPrint({
    req(input$goButton)
    x <- readLines(paste0(input$output,'/linear_attributes_summary.txt'))
    for (i in x) {
      cat(i, '\n')
    }
  })

  output$arealText <- renderPrint({
    req(input$goButton)
    x <- readLines(paste0(input$output,'/areal_attributes_summary.txt'))
    for (i in x) {
      cat(i, '\n')
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)

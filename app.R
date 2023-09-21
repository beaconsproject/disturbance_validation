# Import packages listed in req.txt
#req <- scan(file.path(dirname(getwd()), "req.txt"), character(), quiet = T)
#invisible(lapply(req, library, character.only = T))

library(sf)
library(DT)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(leaflet.esri)

fda_list <- c("10AA", "10AB", "10AD") #, "10BC")
spot = "https://mapservices.gov.yk.ca/imagery/rest/services/Satellites/Satellites_MedRes_Update/ImageServer"
google = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G"

bbc <- function(x){
    bb <- st_bbox(x)
    centroid <- c(
        0.5 * (bb[1] + bb[3]),
        0.5 * (bb[2] + bb[4]))
    names(centroid) <- NULL
    return(centroid)
}

ui = dashboardPage(
  dashboardHeader(title = "Disturbance Validation"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("View disturbances", tabName = "fri", icon = icon("th"))
    ),
    selectInput("fda", label="Select an FDA:", choices=fda_list, selected='10AB'),
    fileInput(inputId = "upload_poly", label = "Or upload a polygon:", multiple = FALSE, accept = ".gpkg"),
    selectInput("inv", label = "Feature type:", choices = c("Areal","Linear")),
    hr(),
    actionButton("goButton", "Select random feature")
  ),
  dashboardBody(
    tabItems(
        tabItem(tabName="fri",
            fluidRow(
                #box(title = "FRI Map", leafletOutput("map1", height=800), width=8),
                tabBox(
                    id = "one", width="8",
                    tabPanel("Polygon", leafletOutput("map1", height=750))#,
                    #tabPanel("Summary", pre(includeText("bc10.txt")))
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Attributes", DT::dataTableOutput("tab1", height=750))
                )
            )
        )
    )
  )
)

server = function(input, output, session) {

  ##############################################################################
  # Read input data
  ##############################################################################
  fda <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(paste0('www/fda_',tolower(input$fda),'.gpkg'), 'fda', quiet=T) %>% st_zm(drop=T) %>%
        st_transform(4326)
    } else {
      aoi_fda()
    }
  })

  linear <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(paste0('www/fda_',tolower(input$fda),'.gpkg'), 'sd_line', quiet=T) %>%
        st_transform(4326)
    } else {
      aoi_line()
    }
  })

  areal <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(paste0('www/fda_',tolower(input$fda),'.gpkg'), 'sd_poly', quiet=T) %>% 
        st_cast('MULTIPOLYGON') %>%
        st_transform(4326)
    } else {
      aoi_poly()
    }
  })

  grd <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(paste0('www/fda_',tolower(input$fda),'.gpkg'), 'nts_yt_9x9', quiet=T) %>% 
        #st_cast('MULTIPOLYGON') %>%
        st_transform(4326)
    } else {
      aoi_grd()
    }
  })

  n <- eventReactive(input$goButton, {
      if (input$inv=="Linear") {
          n = sample_n(linear(), 1)
      } else if (input$inv=="Areal") {
          n = sample_n(areal(), 1)
      }
  })

  ##############################################################################
  # Upload AOI layers
  ##############################################################################
  aoi_fda <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'fda') %>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  aoi_line <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'sd_line') %>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  aoi_poly <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'sd_poly') %>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  aoi_grd <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'nts_yt_9x9') %>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  ##############################################################################
  # View initial set of maps
  ##############################################################################
  output$map1 <- renderLeaflet({
    map_bounds <- fda() %>% st_bbox() %>% as.character()
    m <- leaflet(fda()) %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addTiles(google, group="Google.Imagery") %>%
      addEsriImageMapLayer(url=spot, group="SPOT.Imagery") %>%
      addScaleBar(position='bottomleft') %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>%
      addPolygons(data=grd(), color="grey", weight=1, fill=F, group="Grid")
      if (input$goButton) {
        if (input$inv=="Areal") {
          m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
            addPolygons(data=n(), fill=F, color='yellow', weight=2, group="Random feature")
        } else {
          m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
            addPolylines(data=n(), color='yellow', weight=2, group="Random feature")
        }
      }
      if (is.null(input$upload_poly)) {
        m <- m %>% addPolygons(data=fda(), fill=F, weight=2, color='grey', group="FDA") %>%
          addPolygons(data=areal(), fill=T, weight=1, color='red', fillOpacity=0.5, layerId=areal()$REF_ID, group="Areal disturbances") %>%
          addPolylines(data=linear(), weight=2, color='red', fillOpacity=1, layerId=linear()$REF_ID,group="Linear disturbances") %>%
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
            overlayGroups = c("FDA","Grid","Areal disturbances","Linear disturbances"),
            options = layersControlOptions(collapsed = F)) %>%
          hideGroup(c("Grid","Areal disturbances","Linear disturbances"))
      } else {
        m <- m %>% addPolygons(data=fda(), fill=F, weight=2, color='grey', group="FDA") %>%
          addPolygons(data=areal(), fill=T, weight=1, color='red', fillOpacity=0.5, layerId=areal()$REF_ID, group="Areal disturbances") %>%
          addPolylines(data=linear(), weight=2, color='red', fillOpacity=1, layerId=linear()$REF_ID,group="Linear disturbances") %>%
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
            overlayGroups = c("FDA","Grid","Areal disturbances","Linear disturbances"),
            options = layersControlOptions(collapsed = F)) %>%
          hideGroup(c("Grid","Areal disturbances","Linear disturbances"))
      }
    m
  })

  ##############################################################################
  # View random feature (map and table)
  ##############################################################################
  dta1 <- reactive({
      x1 = st_drop_geometry(n())
      x2 = as_tibble(x1) %>% slice(1) %>% unlist(., use.names=FALSE)
      x = bind_cols(Attribute=names(x1), Value=x2)
  })

  output$tab1 <- DT::renderDataTable({
      x = dta1() #%>% filter(!Attribute %in% casVars)
      datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
  })

  ##############################################################################
  # Click on a linear or areal feature and display attributes
  ##############################################################################
  observeEvent(input$map1_shape_click, {

    # Capture the info of the clicked feature
    click <- input$map1_shape_click

    # Subset table with the id of the clicked feature 
    if (input$inv=="Linear") {
      mydata <- linear()
    } else {
      mydata <- areal()
    }
    selected <- filter(mydata, REF_ID==click$id) %>%
      st_drop_geometry()

    #if click id isn't null render the table
    if(!is.null(click$id)){
      output$tab1 = DT::renderDataTable({
        x <- tibble(Attribute=names(selected), Value=selected[1, ,drop=T])
        datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
      }) 
    } 
  }) 

  #session$onSessionEnded(function() {
  #  stopApp()
  #})

}
shinyApp(ui, server)
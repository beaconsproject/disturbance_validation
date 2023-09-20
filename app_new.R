# Import packages listed in req.txt
#req <- scan(file.path(dirname(getwd()), "req.txt"), character(), quiet = T)
#invisible(lapply(req, library, character.only = T))

library(sf)
library(DT)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(leaflet.esri)

fda_list <- c("10AA", "10AB", "10AD", "10BC")
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

#fda <- st_read('data/fda_10ab.gpkg', 'fda', quiet=T) %>% st_zm(drop=T) %>% st_transform(4326)
#linear = st_read('data/fda_10ab.gpkg', 'sd_line', quiet=T) %>% st_transform(4326)
#areal = st_read('data/fda_10ab.gpkg', 'sd_poly', quiet=T) %>% st_transform(4326)

ui = dashboardPage(
  dashboardHeader(title = "Disturbance Validation"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("View disturbances", tabName = "fri", icon = icon("th"))
    ),
    #selectInput("fda", label="Select an FDA:", choices=fda_list, selected='10AB'),
    fileInput(inputId="fda", label = "Disturbance geopackage:", multiple = FALSE, accept = ".gpkg"),
    selectInput("inv", label = "Feature type:", choices = c("Areal","Linear"))
  ),
  dashboardBody(
    tabItems(
        tabItem(tabName="fri",
            fluidRow(
                tabBox(
                    id = "one", width="8",
                    tabPanel("Polygon", leafletOutput("map1", height=750))
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

    aoi_sf <- eventReactive(input$fda, {
      file <- input$fda$datapath
      ext <- tools::file_ext(file)
      if(ext == "gpkg"){
        aoi <- st_read(file) #%>% st_transform(4326)
      } else {
        showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
      }
    })
  
    fda <- reactive({
      if (!is.null(input$fda)) {
        st_read(aoi_sf(), 'fda', quiet=T) %>% st_zm(drop=T) %>%
          st_transform(4326)
      }
    })

    linear <- reactive({
      if (!is.null(input$fda)) {
        st_read(aoi_sf(), 'sd_line', quiet=T) %>%
          st_transform(4326)
      }
    })

    areal <- reactive({
      if (!is.null(input$fda)) {
        st_read(aoi_sf(), 'sd_poly', quiet=T) %>%
          st_cast('MULTIPOLYGON') %>%
          st_transform(4326)
      }
    })

    output$map1 <- renderLeaflet({
        m <- leaflet() %>%
          addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
          addTiles(google, group="Google.Imagery") %>%
          addEsriImageMapLayer(url=spot, group="SPOT.Imagery") %>%
          addScaleBar(position='bottomleft')
          if (!is.null(input$fda)) {
            map_bounds <- fda() %>% st_bbox() %>% as.character()
            m = m %>% fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>%
              addPolygons(data=fda(), fill=F, weight=2, color='grey', group="FDA") %>%
              addPolygons(data=areal(), fill=T, weight=1, color='red', fillOpacity=0.5, layerId=areal()$REF_ID, group="Areal disturbances") %>%
              addPolylines(data=linear(), weight=2, color='red', fillOpacity=1, layerId=linear()$REF_ID,group="Linear disturbances")
            }
            m = m %>% addLayersControl(position = "topright",
              baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
              overlayGroups = c("FDA","All FDAs","Grid 100km","Areal disturbances","Linear disturbances","Mapping status"),
              options = layersControlOptions(collapsed = F)) %>%
            hideGroup(c("All FDAs","Grid 100km","Areal disturbances","Linear disturbances","Mapping status"))
        m
    })

    observeEvent(input$map1_shape_click, {
      if(!is.null(input$fda)) {

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
      }
    }) 

  #session$onSessionEnded(function() {
  #  stopApp()
  #})

}
shinyApp(ui, server)
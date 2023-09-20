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
fda_all <- st_read('data/fda10.gpkg', 'fda', quiet=T) %>% st_transform(4326)
grd_all <- st_read('data/yt_priority_areas.gpkg', 'nhn_yt_grid', quiet=T) %>% st_transform(4326)

ui = dashboardPage(
  dashboardHeader(title = "Disturbance Validation"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("View disturbances", tabName = "fri", icon = icon("th"))
    ),
    selectInput("fda", label="Select an FDA:", choices=fda_list, selected='10AB'),
    selectInput("inv", label = "Feature type:", choices = c("Areal","Linear")),
    hr(),
    #checkboxInput("dist","Add all disturbances", value=T),
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

    fda <- reactive({
      st_read(paste0('data/fda_',input$fda,'.gpkg'), 'fda', quiet=T) %>% st_zm(drop=T) %>%
        st_transform(4326)
    })

    linear <- reactive({
      st_read(paste0('data/fda_',input$fda,'.gpkg'), 'sd_line', quiet=T) %>%
        st_transform(4326)
    })

    areal <- reactive({
      st_read(paste0('data/fda_',input$fda,'.gpkg'), 'sd_poly', quiet=T) %>% 
        st_cast('MULTIPOLYGON') %>%
        st_transform(4326)
    })

    grd <- reactive({
      #st_read('../../Data/yt_priority_areas.gpkg', 'nhn_yt_grid', quiet=T) %>%
      grd_all %>% filter(substr(DATASETNAM,1,4)==toupper(input$fda)) %>%
      mutate(status=as.factor(status)) #%>%
      #st_transform(4326)
    })

    n <- eventReactive(input$goButton, {
        if (input$inv=="Linear") {
            n = sample_n(linear(), 1)
        } else if (input$inv=="Areal") {
            n = sample_n(areal(), 1)
        }
    })

    output$map1 <- renderLeaflet({
        colStatus = colorFactor(palette=c("red","orange","blue"), domain=grd_all$status)
        map_bounds <- fda() %>% st_bbox() %>% as.character()
        m <- leaflet(fda()) %>%
          addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
          addTiles(google, group="Google.Imagery") %>%
          addEsriImageMapLayer(url=spot, group="SPOT.Imagery") %>%
          addScaleBar(position='bottomleft') %>%
          fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>%
          addPolygons(data=fda_all, fill=F, weight=1, color='blue', group="All FDAs") %>%
          addPolygons(data=fda(), fill=F, weight=2, color='grey', group="FDA") %>%
          addPolygons(data=grd(), color="grey", weight=1, fill=F, group="Grid 100km") %>%
          addPolygons(data=grd_all, color=~colStatus(status), stroke=T, weight=1, fillOpacity=0.5, group="Mapping status") %>%
          addLegend(position="bottomleft", pal=colStatus, values=grd()$status, opacity=0.5, title="Mapping status")
       if (input$goButton) {
            if (input$inv=="Areal") {
                m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
                    addPolygons(data=n(), fill=F, color='yellow', weight=2, group="Random feature")
            } else {
                m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
                    addPolylines(data=n(), color='yellow', weight=2, group="Random feature")
            }
        }
        m <- m %>% 
            addPolygons(data=areal(), fill=T, weight=1, color='red', fillOpacity=0.5, layerId=areal()$REF_ID, group="Areal disturbances") %>%
            addPolylines(data=linear(), weight=2, color='red', fillOpacity=1, layerId=linear()$REF_ID,group="Linear disturbances") %>%
            addLayersControl(position = "topright",
              baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
              overlayGroups = c("FDA","All FDAs","Grid 100km","Areal disturbances","Linear disturbances","Mapping status"),
              options = layersControlOptions(collapsed = F)) %>%
            hideGroup(c("All FDAs","Grid 100km","Areal disturbances","Linear disturbances","Mapping status"))
        m
    })

    dta1 <- reactive({
        x1 = st_drop_geometry(n())
        x2 = as_tibble(x1) %>% slice(1) %>% unlist(., use.names=FALSE)
        x = bind_cols(Attribute=names(x1), Value=x2)
    })

	output$tab1 <- DT::renderDataTable({
        x = dta1() #%>% filter(!Attribute %in% casVars)
		datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
    })

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
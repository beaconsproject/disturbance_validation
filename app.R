library(sf)
library(DT)
library(leaflet)
library(tidyverse)
library(leaflet.esri)
library(summarytools)
library(shinydashboard)
#source('functions/validate_features.R')
options(shiny.maxRequestSize=100*1024^2) 

types <- readr::read_csv('www/yg_industry_disturbance_types.csv')
errors <- readr::read_csv('www/yg_industry_disturbance_types_errors.csv')
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

ui = dashboardPage(skin = "green",
  dashboardHeader(title = "Disturbance Validation"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("Introduction", tabName="intro", icon=icon("th")),
      menuItem("Upload data", tabName = "get", icon = icon("th")),
      menuItem("Validate attributes", tabName = "val", icon = icon("th")),
      menuItem("View random features", tabName = "rnd", icon = icon("th"))
    ),
    conditionalPanel(
      condition="input.tabs=='get'",
      hr(),
      HTML("&nbsp;&nbsp; 1. UPLOAD DATA"),
      fileInput("gpkg", "Geopackage:", accept=".gpkg"),
      HTML("&nbsp;&nbsp; 2. SELECT LAYERS"),
      selectInput("bnd", "Study area:", choices = NULL),
      selectInput("line", "Linear disturbances:", choices = NULL),
      selectInput("poly", "Areal disturbances:", choices = NULL),
      br(),
      HTML("&nbsp;&nbsp; 3. VIEW DISTURBANCES"),
      actionButton("mapButton", "Map features")
      #HTML("&nbsp;&nbsp; VIEW RANDOM FEATURE"),
      #actionButton("rndButton", "Select random feature")
    ),
    conditionalPanel(
      condition="input.tabs=='val'",
      hr(), 
      HTML("&nbsp;&nbsp; VALIDATE ATTRIBUTES"),
      actionButton("valButton", "Run validation code")
    ),
    conditionalPanel(
      condition="input.tabs=='rnd'",
      hr(),
      HTML("&nbsp;&nbsp; VIEW DISTURBANCE"),
      actionButton("rndButton", "Select random feature")
    )
    ),

  dashboardBody(
    tabItems(
      tabItem(tabName="intro",
            fluidRow(
                tabBox(id = "one", width="12",
                    tabPanel("Overview", includeMarkdown("www/overview.md")),
                    tabPanel("Disturbances", DTOutput("types"))
                )
            )
        ),
        tabItem(tabName="get",
            fluidRow(
                tabBox(
                    id = "one", width="8",
                    tabPanel("Mapview", leafletOutput("map1", height=750))
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Areal features", DT::dataTableOutput("tab1a", height=750)),
                    tabPanel("Linear features", DT::dataTableOutput("tab1b", height=750))
                )
            )
        ),
        tabItem(tabName="val",
            fluidRow(
                tabBox(
                    id = "one", width="12",
                    #tabPanel("Mapview", leafletOutput("map2", height=750)),
                    tabPanel("Linear summary", verbatimTextOutput("linearText")),
                    tabPanel("Linear errors", DTOutput("linearTable")),
                    tabPanel("Areal summary", verbatimTextOutput("arealText")),
                    tabPanel("Areal errors", DTOutput("arealTable"))
                )#,
                #tabBox(
                #    id = "two", width="4",
                #    tabPanel("Random features", DT::dataTableOutput("tab2", height=750))
                #)
            )
        ),
        tabItem(tabName="rnd",
            fluidRow(
                tabBox(
                    id = "one", width="8",
                    tabPanel("Mapview", leafletOutput("map2", height=750))
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Random feature", DT::dataTableOutput("tab2", height=750))
                )
            )
        )
    )
  )
)

server = function(input, output, session) {

  output$types <- renderDataTable({
    datatable(types, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
  })

  observeEvent(input$gpkg, {
    file <- input$gpkg$datapath
    gpkg <- st_read(file)
    lyrs <- st_layers(file)$name
    updateSelectInput(session, "layer", choices = lyrs)
    updateSelectInput(session, "bnd", choices=lyrs, selected='studyarea')
    updateSelectInput(session, "line", choices=lyrs, selected='linear_disturbance')
    updateSelectInput(session, "poly", choices=lyrs, selected='areal_disturbance')
  })

  ##############################################################################
  # Upload AOI layers
  ##############################################################################
  bnd <- eventReactive(input$gpkg, {
    req(input$mapButton)
    file <- input$gpkg$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, input$bnd) %>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  line <- eventReactive(input$gpkg, {
    req(input$mapButton)
    file <- input$gpkg$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, input$line) %>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  poly <- eventReactive(input$gpkg, {
    req(input$mapButton)
    file <- input$gpkg$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, input$poly) %>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  #grd <- eventReactive(input$gpkg, {
  #  file <- input$gpkg$datapath
  #  ext <- tools::file_ext(file)
  #  if(ext == "gpkg"){
  #    aoi <- st_read(file, 'nts_yt_9x9') %>% st_transform(4326)
  #  } else {
  #    showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
  #  }
  #})

  ##############################################################################
  # Select a random feature
  ##############################################################################
  ntype <- eventReactive(input$rndButton, {
    choice <- sample(c("Linear","Areal"),1)
    if (choice=="Linear") {
      x = "Linear"
    } else if (choice=="Areal") {
      x = "Areal"
    }
  })
  
  n <- reactive({
    if (ntype()=="Linear") {
      n = sample_n(line(), 1)
    } else if (ntype()=="Areal") {
      n = sample_n(poly(), 1)
    }
  })

  ##############################################################################
  # View initial set of maps
  ##############################################################################
  output$map1 <- renderLeaflet({
    m <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      addTiles(google, group="Google.Imagery") %>%
      addEsriImageMapLayer(url=spot, group="SPOT.Imagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addScaleBar(position='bottomleft') #%>%
      #if (input$rndButton) {
      #  if (ntype()=="Linear") {
      #    m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
      #      addPolygons(data=n(), fill=F, color='yellow', weight=2, group="Random feature")
      #  } else if (ntype()=="Areal") {
      #    m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
      #      addPolylines(data=n(), color='yellow', weight=2, group="Random feature")
      #  }
      #}
      if (is.null(input$gpkg)) {
        m <- m %>% 
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
            options = layersControlOptions(collapsed = F))
      } else {
        req(input$mapButton)
        m <- m %>% addPolygons(data=bnd(), fill=F, weight=2, color='blue', group="Study area") %>%
          addPolygons(data=poly(), fill=T, weight=1, color='red', fillOpacity=0.5, group="Areal disturbances") %>%
          addPolylines(data=line(), weight=2, color='red', fillOpacity=1, group="Linear disturbances") %>%
          #addPolygons(data=grd(), color="grey", weight=1, fill=F, group="Grid") %>%
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
            overlayGroups = c("Study area","Areal disturbances","Linear disturbances"),
            options = layersControlOptions(collapsed = F)) #%>%
          #hideGroup(c("Study area","Areal disturbances","Linear disturbances"))
      }
    m
  })

  ##############################################################################
  # View attribute tables
  ##############################################################################
  output$tab1a <- DT::renderDataTable({
      req(input$mapButton)
      x = st_drop_geometry(poly()) %>%
        select(TYPE_INDUSTRY, TYPE_DISTURBANCE)
      datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
  })

  output$tab1b <- DT::renderDataTable({
      req(input$mapButton)
      x = st_drop_geometry(line()) %>%
        select(TYPE_INDUSTRY, TYPE_DISTURBANCE)
      datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
  })

  ##############################################################################
  # View random feature (map and table)
  ##############################################################################
  dta2 <- reactive({
      x1 = st_drop_geometry(n())
      x2 = as_tibble(x1) %>% slice(1) %>% unlist(., use.names=FALSE)
      x = bind_cols(Attribute=names(x1), Value=x2)
  })

  output$tab2 <- DT::renderDataTable({
      x = dta2() #%>% filter(!Attribute %in% casVars)
      datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
  })

  output$map2 <- renderLeaflet({
    m <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      addTiles(google, group="Google.Imagery") %>%
      addEsriImageMapLayer(url=spot, group="SPOT.Imagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addScaleBar(position='bottomleft') #%>%
      if (input$rndButton) {
        if (ntype()=="Linear") {
          m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
            addPolygons(data=n(), fill=F, color='yellow', weight=2, group="Random feature")
        } else if (ntype()=="Areal") {
          m = m %>% setView(lng=bbc(n())[1], lat=bbc(n())[2], zoom=15) %>%
            addPolylines(data=n(), color='yellow', weight=2, group="Random feature")
        }
        m <- m %>% addPolygons(data=bnd(), fill=F, weight=2, color='blue', group="Study area") %>%
          addPolygons(data=poly(), fill=T, weight=1, color='red', fillOpacity=0.5, group="Areal disturbances") %>%
          addPolylines(data=line(), weight=2, color='red', fillOpacity=1, group="Linear disturbances") %>%
          #addPolygons(data=grd(), color="grey", weight=1, fill=F, group="Grid") %>%
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldImagery","Google.Imagery","SPOT.Imagery","Esri.WorldTopoMap"),
            overlayGroups = c("Study area","Areal disturbances","Linear disturbances"),
            options = layersControlOptions(collapsed = F)) #%>%
          #hideGroup(c("Study area","Areal disturbances","Linear disturbances"))
      }
    m
  })

  ##############################################################################
  # Click on a linear or areal feature and display attributes
  ##############################################################################
  observeEvent(input$map1_shape_click, {

    # Capture the info of the clicked feature
    click <- input$map1_shape_click

    # Subset table with the id of the clicked feature 
    if (input$inv=="Linear") {
      mydata <- line()
    } else {
      mydata <- poly()
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

  ##############################################################################
  # Validate attributes
  ##############################################################################

  #observeEvent(input$valButton, {
  #  validate_features(data_pkg=input$gpkg$datapath, lyr_line=input$line, lyr_poly=input$poly, output_dir=input$output)
  #})

  output$linearTable <- renderDT({
    req(input$valButton)
    x_line <- st_read(input$gpkg$datapath, input$line, quiet=TRUE) |>
      st_drop_geometry()
    line_indu <- types |>
      filter(TYPE_FEATURE=='Linear') |>
      select(TYPE_INDUSTRY) |>
      unique() |>
      pull()
    line_dist <- types |>
      filter(TYPE_FEATURE=='Linear') |>
      select(TYPE_DISTURBANCE) |>
      unique() |>
      pull()
    # These are combinations that are theoretically not permitted but left as is for now
    line_combo <- types |>
      filter(TYPE_FEATURE=='Linear') |>
      select(TYPE_INDUSTRY, TYPE_DISTURBANCE) |>
      mutate(TYPE_COMBINED=paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE)) |>
      unique() |>
      pull()
    # These ones need to be fixed asap - they cannot occur (see fixit script)
    line_combo_error <- errors |>
      filter(TYPE_FEATURE=='Linear') |>
      select(TYPE_INDUSTRY_error, TYPE_DISTURBANCE_error) |>
      mutate(TYPE_COMBINED_error=paste0(TYPE_INDUSTRY_error,"***",TYPE_DISTURBANCE_error)) |>
      unique() |>
      pull()
    x_line_test = mutate(x_line, 
      industry_test=ifelse(TYPE_INDUSTRY %in% line_indu, 'ok', 'please fix'),
      disturbance_test=ifelse(TYPE_DISTURBANCE %in% line_dist, 'ok', 'please fix'),
      combination_test=ifelse(paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE) %in% line_combo, 'ok', 'not expected')) |>
      select(TYPE_INDUSTRY, industry_test, TYPE_DISTURBANCE, disturbance_test, combination_test) |>
      filter(industry_test=='please fix' | disturbance_test=='please fix' | combination_test=='not expected')
    datatable(x_line_test)
  })

  output$arealTable <- renderDT({
    req(input$valButton)
    x_poly <- st_read(input$gpkg$datapath, input$poly, quiet=TRUE) |>
      st_drop_geometry()
    poly_indu <- types |>
      filter(TYPE_FEATURE=='Areal') |>
      select(TYPE_INDUSTRY) |>
      unique() |>
      pull()
    poly_dist <- types |>
      filter(TYPE_FEATURE=='Areal') |>
      select(TYPE_DISTURBANCE) |>
      unique() |>
      pull()
    # These are combinations that are theoretically not permitted but left as is for now
    poly_combo <- types |>
      filter(TYPE_FEATURE=='Areal') |>
      select(TYPE_INDUSTRY, TYPE_DISTURBANCE) |>
      mutate(TYPE_COMBINED=paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE)) |>
      unique() |>
      pull()
    x_poly_test = mutate(x_poly, 
      industry_test=ifelse(TYPE_INDUSTRY %in% poly_indu, 'ok', 'please fix'),
      disturbance_test=ifelse(TYPE_DISTURBANCE %in% poly_dist, 'ok', 'please fix'),
      combination_test=ifelse(paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE) %in% poly_combo, 'ok', 'not expected')) |>
      select(TYPE_INDUSTRY, industry_test, TYPE_DISTURBANCE, disturbance_test, combination_test) |>
      filter(industry_test=='please fix' | disturbance_test=='please fix' | combination_test=='not expected')
    #readr::write_csv(x_poly_test, paste0(output_dir, '/areal_types_validation.csv'))
    datatable(x_poly_test)
  })

  output$linearText <- renderPrint({
    req(input$valButton)
    x_line <- st_read(input$gpkg$datapath, input$line, quiet=TRUE) |>
      st_drop_geometry()
    line_vars <- names(x_line)
    cat('Project: ', input$gpkg$datapath, '\n', sep="")
    cat('Date: ', format(Sys.time(), "%d %B %Y"),'\n', sep="")
    cat('\n\n# LINEAR DISTURBANCES\n', sep="")
    for (i in line_vars) {
      cat('\n## Attribute: ',toupper(i),'\n\n', sep="")
      print(dfSummary(x_line[i], graph.col=FALSE, max.distinct.values=20))
    }
  })

  output$arealText <- renderPrint({
    req(input$valButton)
    x_poly <- st_read(input$gpkg$datapath, input$poly, quiet=TRUE) |>
      st_drop_geometry()
    poly_vars <- names(x_poly)
    cat('Project: ', input$gpkg$datapath, '\n', sep="")
    cat('Date: ', format(Sys.time(), "%d %B %Y"),'\n', sep="")
    cat('\n\n# AREAL DISTURBANCES\n', sep="")
    for (i in poly_vars) {
      cat('\n## Attribute: ',toupper(i),'\n\n', sep="")
      print(dfSummary(x_poly[i], graph.col=FALSE, max.distinct.values=20))
    }
  })

  #session$onSessionEnded(function() {
  #  stopApp()
  #})

}
shinyApp(ui, server)
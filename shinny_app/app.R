# TO DO:
# Make it look better 
# Infor tabe/landing page 
# change colors 

#### Set Up ####
# library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shiny)
library(jsonlite)
library(leaflet.providers)

# setwd('shinny_app/')

# Load in config
conf <- jsonlite::fromJSON("config.json")

#### Launch App ####
# # Uncomment (do not save the script with this unncommented) and run this in 
# # the console to create initial connection to our shinyapps account
# rsconnect::setAccountInfo(name = 'internetofwater',
#                           token = conf$shinyapps_iow_token,
#                           secret = conf$shinyapps_iow_secret)

# # Uncomment (do not save the script with this unncommented) and run this in 
# # the console to deploy app. 
# rsconnect::deployApp(appName = 'iow_lead_dash',
#                      account = 'internetofwater')

#### Per processing/Get names ####

# Get the codes for all boundaries where data is avalible 
boundary_names <- str_remove(list.files('data/building_address_join_pretty/'), '[.]geojson')

# Set the fist selections for the app
default_boundary <- boundary_names[7]
default_highlight_year <- 1978
current_year <- as.numeric(str_split_fixed(Sys.Date(), '-', n = Inf)[,1])

# Convert the coumns to names to display in app 
building_layer_info <- read_csv('data/building_pretty_key.csv') %>%
    select(Column = col_name, pretty_name)
# mutate(column = tolower(column))

# Convert utility codes to names 
boundary_real_names <- read_csv('data/boundary_names_to_code.csv') %>%
    filter(pwsid %in% !!boundary_names)
boundary_names <- boundary_real_names$pwsid
names(boundary_names) <- boundary_real_names$pws_name

# justice 40 layers 
justice_40 <- st_read('data/justice_40_nc.geojson')

#### App UI ####
ui <- fluidPage(
    title = 'EPA Lead Rule Viewer',
    # Landing Page
    # Header
    headerPanel(
        fluidRow(
            column(2, div("Lead Rule Viewer")
            ),
            column(6, img(src='internet-of-water-coalition-logo_header.png', align = "left")
            )
        ),
    ),
    
    # Side tab to select water utility and highlight year and display structure 
    # information 
    sidebarLayout(
        # Site selection
        sidebarPanel(
            # Select water utility boundary 
            div("Utility Selection", class = "widget-title text-center", align = 'left'),
            selectizeInput(inputId = "BOUNDARY",
                           label = NULL, selected = default_boundary,
                           choices = boundary_names, multiple = FALSE
            ),
            # Year to highlight data
            div("Highlight Year", class = "widget-title text-center"),
            numericInput(inputId = "HIGHLIGHT", label = NULL,
                         value = default_highlight_year, min = 1700,
                         max = current_year
            ),
            div('Justice 40 Boundaries (Set Occupency)',
                div(
                    class = "ms-tooltip",
                    style = "display: inline-block; vertical-align: middle",
                    title = paste(
                        "Justice 40 show areas that are disadvantaged",
                        "for diffrent reasaons. See link at the bottom",
                        "of the app for more information. Slide this bar",
                        "to make the colors more or less transparent."
                    ),
                    enc2native("\U2753")
                ),
                class = 'widget-title text-center'),
            sliderInput(inputId = 'JUSTICE_OCUP',
                         label = NULL, value = 0,
                         min = 0, max = 1),
            # Table with building info
            div("Building Information (click on map marker)", class = "widget-title text-center"),
            tabPanel(
                'Building Information',
                div(
                    id = 'building_info',
                    class = 'well',
                    tableOutput('BUILDING_INFO')
                )
            )
        ),
        # Display map and generate inventory tab
        mainPanel(
            tabPanel(
                "Map",
                div(
                    id = "mapcontainer",
                    class = "well",
                    leafletOutput("MAP", height = 400),
                ),
                textOutput('percetage_over'),
                downloadButton('download',"Generate Report for Water Utility"),
                div(
                    class = 'text-well',
                    h1('Source Data:', style = "font-size:20px;"),
                    h1(HTML('<a href="https://www.hydroshare.org/resource/c9d8a6a6d87d4a39a4f05af8ef7675ad/">Utility Boundaries</a> (Updated Dec 21, 2022) <br/>'),
                       HTML('<a href="https://www.nconemap.gov/datasets/2d07f32b93184a758be29dc1f41344bd_0/explore">NC Building Footprints</a> (Updated Jan 5, 2021) <br/>'),
                       HTML('<a href="https://www.nconemap.gov/pages/addresses">NC Addresses</a> (Updated: Mar 2, 2023) <br/>'),
                       HTML('<a href="https://www.arcgis.com/home/item.html?id=ee9ddbc95520442482cd511f9170663a">Justice 40</a> (Updated: Dec 15, 2022) <br/>'),
                       style = "font-size:12px;")
                    # h1(HTML('Source data for each layer can be found here: <br/> Utility boundaries: https://www.hydroshare.org/resource/c9d8a6a6d87d4a39a4f05af8ef7675ad/ <br/> '), style = "font-size:12px;"),
                )
            ),
        )
    ),
    # Styling 
    # tags$head(tags$style(HTML('.row {background-color: #abb9ca;}',
    #                           '.well {background-color: #002855; border-color: #002855}',
    #                           '.text-well {background-color: #abb9ca;}')))
)

#### app server ####
server <- function(input, output, session) {
    
    
    #### Load in Layers ####
    # Create Trigger for year 
    highlight_year <- reactive({
        highlight_year <- input$HIGHLIGHT
    }) %>%
        debounce(1000)
    
    justice_occup <- reactive({
        map_occupency <- input$JUSTICE_OCUP
    }) %>%
        debounce(1000)
    
    # Read in water district boundary
    wd_bondary_react <- reactive({
        wd_boundary_name <- input$BOUNDARY
        
        wd_boundary <- read_sf(paste0('data/wd_boundaries/', wd_boundary_name, '.geojson'))
    })
    
    # Building centroids 
    boundary_buildings_react <- reactive({
        # Inputs 
        # boundary_id <- 'NC0106422'
        boundary_id <- input$BOUNDARY
        highlight_year <- highlight_year()
        
        # Read in data 
        boundary_buildings <- read_sf(paste0('data/building_address_join_pretty/', boundary_id, '.geojson')) %>%
            filter(YEAR_BUILT > 0) %>%
            arrange(YEAR_BUILT) %>%
            mutate(color = ifelse(YEAR_BUILT >= highlight_year, '#116D6E', '#CD1818')) 
        
        return(boundary_buildings)
    })
    
    #### Outputs ####
    #### Side bar report for individual buildings ####
    building_info_react <- reactive({
        
        # inputs 
        boundary_buildings <- boundary_buildings_react()
        building_id <- input$MAP_marker_click$id
        
        # Create empty table if nothing is there, otherwise make display table 
        id_in_current_data <- building_id %in% boundary_buildings$OBJECTID
        if(is.null(building_id) || ! id_in_current_data){
            building_table <- tibble()
        } else{
            building_table <- boundary_buildings %>%
                filter(OBJECTID == !!building_id) %>%
                as_tibble() %>%
                select(-geometry)
            
            building_table <- building_table %>%
                mutate(., across(names(building_table), as.character)) %>%
                pivot_longer(cols = names(building_table), names_to = 'Column', values_to = 'Value') %>%
                left_join(., building_layer_info, by = 'Column') %>%
                select(Column = pretty_name, Value) %>%
                filter(!is.na(Column))
            
            
        }
        
        return(building_table)
    })
    
    #### Final report for all buildings ####
    final_report <- reactive({
        # Inputs 
        dataset <- boundary_buildings_react() %>%
            as.data.frame() %>%
            select(-geometry, color)
        
        return(dataset)
    })
    
    #### Percentage over the highlihght year ####
    output$percetage_over <- renderText({
        
        # Inputs
        building_info <- boundary_buildings_react()
        highlight_year <- highlight_year()
        
        
        numb_older <- building_info %>%
            filter(color == '#CD1818') %>%
            nrow()
        
        percentage_over <- paste0('Percent of structures built before ', highlight_year, ': ', round((numb_older/nrow(building_info))*100), ' %')
        
        return(percentage_over)
    })
    
    
    #### MAP ####
    output$MAP <- renderLeaflet({
        # Inputs
        highlight_year <- isolate(input$HIGHLIGHT)
        boundary_buildings <- boundary_buildings_react()
        wd_boundary <- wd_bondary_react() 
        justice_occup <- justice_occup()
        
        # Display water utility boundary 
        
        if(justice_occup > 0){
            
            center <- substr(as.character(boundary_buildings[1,]$geometry), 3, nchar(as.character(boundary_buildings[1,]$geometry)))
            center <- substr(center, 1, nchar(center) - 1)
            center <- str_split_fixed(center, ', ', n = Inf)
            
            lat <- center[1,2]
            lng <- center[1,1]
            
            fin_map <- leaflet() %>%
                addPolygons(
                    data = justice_40,
                    color = justice_40$cols,
                    fillOpacity = justice_occup,
                    stroke = F
                ) %>%
                addPolygons(
                    data = wd_boundary,
                    color = '#000000',
                    weight = 3,
                    smooth = 0,
                    stroke = TRUE,
                    fillOpacity = 0.2,
                    layerId = 'boundary'
                ) %>%
                # Add buildings 
                addCircleMarkers(
                    data = boundary_buildings,
                    color = boundary_buildings$color,
                    layerId = boundary_buildings$OBJECTID,
                    group = 'buildings',
                    stroke = TRUE,
                    opacity = 0.5,
                    radius = 3,
                    weight = 10,
                    fillOpacity = 1
                ) %>%
                # add basemap 
                addProviderTiles("Esri.WorldTopoMap",
                                 group = 'Basemap'
                ) %>%
                # Add imagery basemap 
                addProviderTiles(providers$Esri.WorldImagery,
                                 group = 'Imagery') %>%
                # Add names and allow toggle between basemaps 
                addLayersControl(
                    position = "topright",
                    baseGroups = c("Basemap", "Imagery"),
                    options = layersControlOptions(
                        collapsed = TRUE,
                        autoZIndex = TRUE
                    )
                ) %>%
                # Add legend
                addLegend(colors = c('#116D6E', '#CD1818', '#000000'),
                          labels = c('Built After', 'Built Before', 'Water District')) %>%
                addLegend(title = 'Justice 40 Factors Exceeded',
                          colors = c("#F3F8FD", "#D7E6F4", "#B9D5EA", "#8BBFDD", "#5AA2CF", "#3080BD", "#125FA7", "#083D7E"),
                          labels = c(1:8)) %>%
                setView(lng = lng, lat = lat,
                        zoom = 10)
        } else{
            fin_map <- leaflet() %>%
                addPolygons(
                    data = wd_boundary,
                    color = '#000000',
                    weight = 3,
                    smooth = 0,
                    stroke = TRUE,
                    fillOpacity = 0.2,
                    layerId = 'boundary'
                ) %>%
                # Add buildings 
                addCircleMarkers(
                    data = boundary_buildings,
                    color = boundary_buildings$color,
                    layerId = boundary_buildings$OBJECTID,
                    group = 'buildings',
                    stroke = TRUE,
                    opacity = 0.5,
                    radius = 3,
                    weight = 10,
                    fillOpacity = 1
                ) %>%
                # add basemap 
                addProviderTiles("Esri.WorldTopoMap",
                                 group = 'Basemap'
                ) %>%
                # Add imagery basemap 
                addProviderTiles(providers$Esri.WorldImagery,
                                 group = 'Imagery') %>%
                # Add names and allow toggle between basemaps 
                addLayersControl(
                    position = "topright",
                    baseGroups = c("Basemap", "Imagery"),
                    options = layersControlOptions(
                        collapsed = TRUE,
                        autoZIndex = TRUE
                    )
                ) %>%
                # Add legend
                addLegend(colors = c('#116D6E', '#CD1818', '#000000'),
                          labels = c('Built After', 'Built Before', 'Water District')) 
        }
        
        
        return(fin_map)
    })
    
    #### TABLE ####
    output$BUILDING_INFO <- renderTable(building_info_react())
    
    #### Generate Report ####
    output$download <- downloadHandler(
        
        filename = function() {
            paste0(input$BOUNDARY, '_report', ".csv")
        },
        content = function(file) {
            write.csv(final_report(), file)
        }
    )
}

# Creates shiny app by joining ui and server 
shinyApp(ui, server)
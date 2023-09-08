
#### Set Up ####
# library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shiny)
library(shinydashboard)
library(jsonlite)
library(leaflet.providers)

# setwd('shinny_app/')

# Load in config
# conf <- jsonlite::fromJSON("config.json")

#### Launch App ####
# Uncomment (do not save the script with this unncommented) and run this in
# the console to create initial connection to our shinyapps account
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
justice_40 <- st_read('data/justice_40_nc_poly.geojson')

ui <- dashboardPage(
    title = 'EPA Lead Dash',
    # includeCSS("www/app.css"),
    # title = 'EPA Lead Rule Viewer',
    # headerPanel(
    #     fluidRow(
    #         column(2, div("Lead Rule Viewer")
    #         ),
    #         column(width = 2, img(src='internet-of-water-coalition-logo_header.png', align = "left")
    #         ),
    #         column(width = 8,
    #                div(
    #                    h1(paste('This was developed by the Internet of Water in partnership',
    #                             'with the North Carolina Rural Water Association. User can use this app',
    #                             ' to select a water utility and see what building were constructed',
    #                             ' before a slected year.'), style = "font-size:20px;"))
    #         )
    #     ),
    # ),
        dashboardHeader(
            tags$li(class = "dropdown",
                    # tags$style("min-height: 100px"),
                    tags$style(".main-header .logo {min-height: 100PX}")
            ),
            title = div(h1('Lead Service Line Inventory Tool',
                           style = "font-size:25px"))
            ),
        dashboardSidebar(
            # disable = TRUE,
            # collapsed = T,
            tags$li(class = "sidebar",
                    id = 'sidebarCollapsed',
                    # tags$style("min-height: 100px"),
                    tags$style(".main-sidebar {padding-top: 86px}")
            ),
            width = "400px",
            tabsetPanel(
                id = 'left-tab',
                tabPanel('About',
                         value = "about",
                         div(style = 'padding: 30px',
                         fluidRow(
                             column(
                                 width = 12,
                                 div(class = 'side-bar-text',
                                     h1('Welcome to the Lead Service Line Inventory Tool!',
                                        style = "font-size:30px; font-family: 'Montserrat', sans-serif; "),
                                     h1('This app was created to assist utilities in complying with new EPA policies on lead pipes in service lines, see', 
                                        HTML('<a href="https://www.deq.nc.gov/about/divisions/water-resources/drinking-water/lead-service-line-inventory">this</a>'),
                                        'link for details on the regulation.',
                                        style = "font-size:20px; font-family: 'Montserrat', sans-serif; "),
                                     h1('You can use the "Building Info" tab to select a water utility by scrolling through the list of boundaries or by typing in the name of a utility. The app will then display the utility boundary and all buildings within the service area.',
                                        style = "font-size:20px; font-family: 'Montserrat', sans-serif; "),
                                     h1('You can also set a year to highlight buildings before and after and overlay the Justice 40 layers on the map. Below the map, you can download a csv file with building information and year of construction.',
                                        style = "font-size:20px; font-family: 'Montserrat', sans-serif; "),
                                     h1('This app was created by the Internet of Water in partnership with the North Carolina Rural Water Association  ',
                                        style = "font-size:10px; font-family: 'Montserrat', sans-serif; ")
                                 )
                             )
                         ),
                         fluidRow(
                             column(
                                 width = 12,
                                 img(src='IoWCoalitionLogo_horizontal-whitetext-colorlogo.png',
                                     style="align: center")
                             ),
                         ),
                         # fluidRow(
                         #     column(
                         #         width = 12,
                         #         img(src='ncrwa-logo-with-address-tagline.png',
                         #             style="align: center")
                         #     )
                         # ),
                )
                ),
                tabPanel('Building Info',
                         value = 'Building_Info',
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
                                     "for diffrent reasons See link at the bottom",
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
                         div("Building Information (click on map marker)",
                             div(
                                 class = "ms-tooltip",
                                 style = "display: inline-block; vertical-align: middle",
                                 title = paste(
                                     paste("Data below is sourced from the NC Building Footprint",
                                           'and the NC Addresses Datasets. If data is missing (a NA is present),',
                                           'this indicates data is missing in one of these datasets',
                                           'for the building of interest')
                                 ),
                                 enc2native("\U2753")
                             ),
                             class = "widget-title text-center"),
                         tabPanel('Building Information',
                             div(
                                 style = 'background-color: #222d32; border: #222d32; padding: 10px',
                                 id = 'building_info',
                                 class = 'well',
                                 tableOutput('BUILDING_INFO')
                             )
                         )
                ),
            )
        ),
        # Display map and generate inventory tab
        dashboardBody(
            # width = "60%",
            # width = 8,
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
)

#### App UI ####
# ui <- fluidPage(
#     title = 'EPA Lead Rule Viewer',
#     # Landing Page
#     # Header
#     headerPanel(
#         fluidRow(
#             column(2, div("Lead Rule Viewer")
#             ),
#             column(width = 2, img(src='internet-of-water-coalition-logo_header.png', align = "left")
#             ),
#             column(width = 8,
#                 div(
#                     h1(paste('This was developed by the Internet of Water in partnership',
#                     'with the North Carolina Rural Water Association. User can use this app',
#                     ' to select a water utility and see what building were constructed',
#                     ' before a slected year.'), style = "font-size:20px;"))
#                 )
#         ),
#     ),
# 
#     # Side tab to select water utility and highlight year and display structure
#     # information
#     sidebarLayout(
#         # Site selection
#         sidebarPanel(
#             # Select water utility boundary
#             div("Utility Selection", class = "widget-title text-center", align = 'left'),
#             selectizeInput(inputId = "BOUNDARY",
#                            label = NULL, selected = default_boundary,
#                            choices = boundary_names, multiple = FALSE
#             ),
#             # Year to highlight data
#             div("Highlight Year", class = "widget-title text-center"),
#             numericInput(inputId = "HIGHLIGHT", label = NULL,
#                          value = default_highlight_year, min = 1700,
#                          max = current_year
#             ),
#             div('Justice 40 Boundaries (Set Occupency)',
#                 div(
#                     class = "ms-tooltip",
#                     style = "display: inline-block; vertical-align: middle",
#                     title = paste(
#                         "Justice 40 show areas that are disadvantaged",
#                         "for diffrent reasons See link at the bottom",
#                         "of the app for more information. Slide this bar",
#                         "to make the colors more or less transparent."
#                     ),
#                     enc2native("\U2753")
#                 ),
#                 class = 'widget-title text-center'),
#             sliderInput(inputId = 'JUSTICE_OCUP',
#                          label = NULL, value = 0,
#                          min = 0, max = 1),
#             # Table with building info
#             div("Building Information (click on map marker)",
#                 div(
#                     class = "ms-tooltip",
#                     style = "display: inline-block; vertical-align: middle",
#                     title = paste(
#                         paste("Data below is sourced from the NC Building Footprint",
#                               'and the NC Addresses Datasets. If data is missing (a NA is present),',
#                               'this indicates data is missing in one of these datasets',
#                               'for the building of interest')
#                     ),
#                     enc2native("\U2753")
#                 ),
#                 class = "widget-title text-center"),
#             tabPanel(
#                 'Building Information',
#                 div(
#                     id = 'building_info',
#                     class = 'well',
#                     tableOutput('BUILDING_INFO')
#                 )
#             )
#         ),
#         # Display map and generate inventory tab
#         mainPanel(
#             tabPanel(
#                 "Map",
#                 div(
#                     id = "mapcontainer",
#                     class = "well",
#                     leafletOutput("MAP", height = 400),
#                 ),
#                 textOutput('percetage_over'),
#                 downloadButton('download',"Generate Report for Water Utility"),
#                 div(
#                     class = 'text-well',
#                     h1('Source Data:', style = "font-size:20px;"),
#                     h1(HTML('<a href="https://www.hydroshare.org/resource/c9d8a6a6d87d4a39a4f05af8ef7675ad/">Utility Boundaries</a> (Updated Dec 21, 2022) <br/>'),
#                        HTML('<a href="https://www.nconemap.gov/datasets/2d07f32b93184a758be29dc1f41344bd_0/explore">NC Building Footprints</a> (Updated Jan 5, 2021) <br/>'),
#                        HTML('<a href="https://www.nconemap.gov/pages/addresses">NC Addresses</a> (Updated: Mar 2, 2023) <br/>'),
#                        HTML('<a href="https://www.arcgis.com/home/item.html?id=ee9ddbc95520442482cd511f9170663a">Justice 40</a> (Updated: Dec 15, 2022) <br/>'),
#                        style = "font-size:12px;")
#                     # h1(HTML('Source data for each layer can be found here: <br/> Utility boundaries: https://www.hydroshare.org/resource/c9d8a6a6d87d4a39a4f05af8ef7675ad/ <br/> '), style = "font-size:12px;"),
#                 )
#             ),
#         )
#     ),
#     # Styling
#     # tags$head(tags$style(HTML('.row {background-color: #abb9ca;}',
#     #                           '.well {background-color: #002855; border-color: #002855}',
#     #                           '.text-well {background-color: #abb9ca;}')))
# )

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
        
        print(wd_boundary_name)
        
        wd_boundary <- read_sf(paste0('data/wd_boundaries/', wd_boundary_name, '.geojson'))
    })
    
    # Save last selection (for when user type in a name)
    last_boundary <- reactiveValues(prev_boundary = NULL)
    
    # Append new value to previous values when input$bins changes 
    observeEvent(input$BOUNDARY, {
        last_boundary$prev_boundary <- c(last_boundary$prev_boundary, input$BOUNDARY)
    })
    
    # Building centroids 
    boundary_buildings_react <- reactive({
        # Inputs 
        # boundary_id <- 'NC0106422'
        boundary_id <- input$BOUNDARY
        highlight_year <- highlight_year()
        
        if(boundary_id == ''){
            boundary_id <- last_boundary$prev_boundary[length(last_boundary$prev_boundary)-1]
            print(last_boundary$prev_boundary)
            print(length(last_boundary$prev_boundary)-1)
        }
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
                leafgl::addGlPolygons(
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
                addLegend(title = paste0('Justice 40', '\n', 'Factors Exceeded'),
                          colors = c("#F3F8FD", "#D7E6F4", "#B9D5EA", "#8BBFDD", "#5AA2CF", "#3080BD", "#125FA7", "#083D7E"),
                          labels = c(1:8)) %>%
                setView(lng = lng, lat = lat,
                        zoom = 13)
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
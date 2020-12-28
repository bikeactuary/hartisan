#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages <- c("shiny", "DT", "dplyr", "magrittr", "stringi", "stringr", "sf",
              "leaflet", "tibble", "tidyverse", "shinydashboard", "dashboardthemes")
sapply(packages, require, character.only = TRUE)

sfPermits <- readRDS("permit_data.RDS") %>%
    map(~mutate(., popup = paste0("<b>Permit Number:</b> ", permit_number, "<br>",
                                  "<b>Work Class:</b> ", work_class, "<br>",
                                  "<b>Permit Type:</b> ", permit_type, "<br>",
                                  "<b>Division:</b> ", division, "<br>",
                                  "<b>Address:</b> ", address, "<br>",
                                  "<b>Value:</b> ", scales::dollar(value), "<br>",
                                  "<b>Apply Date:</b> ", as.Date(apply_date), "<br>",
                                  "<b>Status:</b> ", status, "<br>",
                                  "<b>Issue Date:</b> ", as.Date(issue_date), "<br>",
                                  "<b>Description:</b> ", work_description )) ) %>%
    map(~mutate(., 
                permit_type_grp = case_when(permit_type %in% c("Certificate of Approval", "Building (Residential)",
                                                               "Building (Commercial)", "Demolition",
                                                               "Building (Educational)") ~ "Building/Demo",
                                            TRUE ~ "Systems/Other"),
                apply_date = as.Date(apply_date),
                issue_date = as.Date(issue_date),
                expire_date = substr(expire_date, 1, 10) %>% na_if("") %>% as.Date(),
                # value = scales::dollar(value),
                applicant = case_when(is.na(global_entity_name) | stri_length(trimws(global_entity_name)) == 0 ~ paste0(last_name, ", ", first_name) %>% str_to_title(),
                                      is.na(last_name) | stri_length(trimws(last_name)) == 0 ~ global_entity_name,
                                      TRUE ~ paste0(paste0(last_name, ", ", first_name), "; ", global_entity_name)) ))

## define ui
ui <- dashboardPage(
    dashboardHeader(title = "haRtisan"#,
                    # tags$li(class = "dropdown",
                    #         tags$a(href="http://myprofile.munichre.com/Person.aspx?accountname=AMERICA%5Cn1207158",
                    #                tags$img(height = "19px", src="logo.png")
                    #         )
                    # )
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Building Permits", tabName = "a", icon = icon("hammer")),
            menuItem("Citizen Complaints (HPD)", tabName = "b", icon = icon("balance-scale-right"))
            # menuItem("Large Losses", tabName = "large_losses", icon = icon("bomb"))
        )
    ),
    dashboardBody(
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        
        tabItems(
            tabItem(tabName = "a",
                    fluidRow(
                        column(width = 6, leaflet::leafletOutput("map", height = 600)),
                        column(width = 3, 
                               selectizeInput("hood", "Neighborhood(s):", choices = unique(sfPermits[[1]]$name), selected = "Asylum Hill"),
                               dateRangeInput("daterng", "Application Date Range:",
                                              start = "2019-01-01",
                                              end   = "2020-12-31"),
                               shinyWidgets::numericRangeInput("valuerng", "Job Value ($):", value = c(0, max(sfPermits[[1]]$value, na.rm = T) ) ),
                               checkboxGroupInput("class", "Work Class:", 
                                                  choices = sfPermits[[1]]$work_class %>% unique,
                                                  selected = sfPermits[[1]]$work_class %>% unique)
                        ),
                        column(width = 3,
                               checkboxGroupInput("ptype", "Permit Type:", 
                                                  choices = sfPermits[[1]]$permit_type %>% unique,
                                                  selected = sfPermits[[1]]$permit_type %>% unique),
                               checkboxGroupInput("status", "Permit Status:",
                                                  choices = sfPermits[[1]]$status %>% unique,
                                                  selected  = sfPermits[[1]]$status %>% unique)
                        )
                    ),
                    
                    br(),
                    
                    DT::dataTableOutput("permits_mapped")
                    # fluidRow(
                    #     column(width=8,
                    #            box(width = 12,
                    #                title = h3("Rating Output"),
                    #                status = "success",
                    #                htmlOutput("rating"),
                    #                br(),
                    #                column(4, actionBttn("report",
                    #                                     label = "Generate Report (PDF)",
                    #                                     size = "sm",
                    #                                     color = "primary",
                    #                                     style = "jelly") ),
                    #                br()
                    #            ),
                    #            
                    #            box(width = 12,
                    #                title = h3("MVP Submission Section"),
                    #                status = "warning",
                    #                actionBttn("butCalc", label = "Calculate", style = "pill", color = "royal", size = "sm"),
                    #                downloadBttn("download_tasks", label = "Download", style = "pill", color = "success", size = "sm", no_outline = FALSE),
                    #                br(),
                    #                br(),
                    #                div(id='hot_wordWrap', rHandsontableOutput("hot"))
                    #            )
                    #     ),
                    #     box(width = 3,
                    #         status = "warning",
                    #         title = h3("Project Inputs"),
                    #         textInput("cust_name",
                    #                   "Customer Name (Specified Agreement Holders)",
                    #                   value = ""),
                    #         
                    #         textAreaInput("proj_descr",
                    #                       label = "Project Description",
                    #                       value = "",
                    #                       height = 72,
                    #                       resize = "vertical"),
                    #         
                    #         numericInput("contract_val",
                    #                      "Total Warranted Contract Value",
                    #                      value = 1e5,
                    #                      min = 0,
                    #                      max = 1e7),
                    #         sliderInput("mvp_pct",
                    #                     "% Scored from MVP Submission",
                    #                     value = 100,
                    #                     min = 0,
                    #                     max = 100,
                    #                     step = 1,
                    #                     post = "%"),
                    #         htmlOutput("proj_pct"),
                    #         br(),
                    #         sliderInput("coins_pct",
                    #                     "Coinsurance Provision (%)",
                    #                     value = 10,
                    #                     min = 0,
                    #                     max = 50,
                    #                     step = 5,
                    #                     post = "%"),
                    #         
                    #         sliderInput("adj_pct",
                    #                     "UW Risk Modifier (%)",
                    #                     value = 0,
                    #                     min = -50,
                    #                     max = 100,
                    #                     step = 1,
                    #                     post = "%"),
                    #         htmlOutput("mod_alert"),
                    #         br(),
                    #         numericInput("commis_pct",
                    #                      "HSB Assoc. Commission",
                    #                      value = .10,
                    #                      min = 0,
                    #                      max = .20,
                    #                      step = .01),
                    #         
                    #         numericInput("freq",
                    #                      "Assumed Base Claim Frequency",
                    #                      value = .02,
                    #                      min = .01,
                    #                      max = 1,
                    #                      step = .01),
                    #         
                    #         numericInput("lcm",
                    #                      "Estimated Loss Cost Multiplier",
                    #                      value = 3.0,
                    #                      min = 2.0,
                    #                      max = 5.0)
                    #     )
                    # )
            ),
            
            tabItem(tabName = "b"
                    ## other stuff
            ),
            
            tabItem(tabName = "z"
                    ## other stuff
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    permits_mapped <- reactive({
        sfPermits[[1]] %>%
            filter(name == "Asylum Hill") %>%
            filter(apply_date >= input$daterng[1] & apply_date <= input$daterng[2]) %>%
            filter(value >= input$valuerng[1] & value <= input$valuerng[2]) %>%
            filter(permit_type %in% input$ptype) %>%
            filter(status %in% input$status) %>%
            filter(work_class %in% input$class) %>%
            select(-c(geom, town, last_name, first_name, global_entity_name, expire_date, name)) %>%
            relocate(address, .after = permit_number) %>%
            relocate(unit_or_suite, .after = address) %>%
            relocate(permit_type, .after = permit_number) %>%
            relocate(applicant, .after = permit_type) 
        
    })
    
    output$permits_mapped = DT::renderDataTable({
        permits_mapped() %>%
            as_tibble() %>%
            select(-c(popup, geometry)) %>%
            DT::datatable(rownames = FALSE)
    })
    
    pal <- colorFactor(
        palette = 'Dark2',
        domain = sfPermits[[1]]$permit_type_grp
    )
    
    center <- reactive({
        print(paste0("Map Center is: ", input$map_center))
        if(is.null(input$map_center)) {
            return(c(-72.682147, 41.770995))
        } else {
            return(input$map_center)
        }
        
    })
    
    zoom <- reactive({
        print(paste0("Map Zoom is: ", input$map_zoom))
        if(is.null(input$map_zoom)){
            return(13)
        } else {
            return(input$map_zoom)
        }
    })
    
    
    output$map <- leaflet::renderLeaflet({
        cwi::hartford_sf %>%
            filter(town == "Hartford") %>%
            leaflet() %>%
            addTiles() %>%
            addPolygons(color = "#156ced", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, #fillOpacity = 0.5,
                        # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                        popup = ~name,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)) %>%
            addLegend("bottomright", pal = pal, values = permits_mapped()$permit_type_grp, title = "Permit Type Grouping")
    })
    
    observe({
        
        leafletProxy("map") %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            #setView(lng = 179.462, lat =  -20.64275, zoom = 3) %>%
            setView(lng = isolate(center()[1]),
                    lat = isolate(center()[2]),
                    zoom = isolate(zoom())) %>%
            addCircleMarkers(data = permits_mapped(),
                             # radius = 6,
                             popup = ~popup,
                             clusterOptions = markerClusterOptions(),
                             color = ~pal(permit_type_grp) )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

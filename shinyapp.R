library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyWidgets)


st <- read.csv2("https://data.wien.gv.at/csv/wienerlinien-ogd-steige.csv", stringsAsFactors = FALSE)
lin <- read.csv2("https://data.wien.gv.at/csv/wienerlinien-ogd-linien.csv", stringsAsFactors = FALSE)
hs <- read.csv2("https://data.wien.gv.at/csv/wienerlinien-ogd-haltestellen.csv", stringsAsFactors = FALSE)
hs$WGS84_LON <- as.numeric(hs$WGS84_LON)
hs$WGS84_LAT <- as.numeric(hs$WGS84_LAT)

df <- st %>%
  left_join(hs, c("FK_HALTESTELLEN_ID" = "HALTESTELLEN_ID")) %>%
  left_join(lin, c("FK_LINIEN_ID" = "LINIEN_ID")) %>%
  filter(VERKEHRSMITTEL == "ptMetro") %>%
  filter(RICHTUNG == "H") %>%
  mutate(color = case_when(BEZEICHNUNG == "U1" ~ "red",
                           BEZEICHNUNG == "U2" ~ "violet",
                           BEZEICHNUNG == "U3" ~ "orange",
                           BEZEICHNUNG == "U4" ~ "green",
                           BEZEICHNUNG == "U6" ~ "brown")) %>%
  group_by(NAME) %>%
  mutate(station_label = paste0(NAME, " (", toString(BEZEICHNUNG), ")")) %>%
  arrange(REIHENFOLGE.x)


shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Vienna Metro Map"),
    dashboardSidebar(sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", 
               icon = icon("dashboard")),
      menuItem("Documentation", tabName = "documentation",
               icon = icon("book"), 
               badgeLabel = "new", badgeColor = "green")
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                h2("Dashboard"),
      box(leafletOutput("map"),
          width = 12),
      box(prettyCheckboxGroup(inputId = "lines",
                              label = "Metro Lines",
                              choices = sort(unique(df$BEZEICHNUNG)),
                              # inline = TRUE,
                              #selected = unique(df$BEZEICHNUNG),
                              animation = "pulse",
                              outline = FALSE,
                              inline = TRUE,
                              shape = "curve",
                              status = "info"),
          width = 12),
      valueBoxOutput("lineBox",
                     width = 6),
      valueBoxOutput("stationBox",
                     width = 6)
    ),
    tabItem(tabName = "documentation",
              h2("Documentation"),
            box("Just select one or more metro lines so they will be displayed on the map.",
                width = 8),
            box("The application also calculates the number of lines that you have selected and the number of stations on those lines.",
                width = 8),
            box("Stations with more than one line are, of course, counted only once.",
                width = 8)
            )
    )
    )
    ),
  
      
    
  
  server = function(input, output) {
    
    output$map <- renderLeaflet({
          leaflet(data = df) %>%
            addTiles() %>%
            addCircleMarkers(~WGS84_LON, 
                             ~WGS84_LAT, 
                             radius = 8, 
                             color = "grey",
                             fill = "transparent",
                             opacity = .8,
                             label = ~station_label,
                             group = ~BEZEICHNUNG)
        })
    
    output$lineBox <- renderValueBox({
      infoBox(
        "", paste0(length(input$lines)), "Metro Lines", icon = icon("train"),
        color = "purple")
    })
    
    output$stationBox <- renderValueBox({
      infoBox(
        "", paste0(df %>% filter(BEZEICHNUNG %in% input$lines) %>% group_by(NAME) %>% summarize() %>%nrow()), "Stations", icon = icon("train"),
        color = "yellow")
    })
    
    
    observe({
      
      lines <- input$lines
      
      filtered_df <- df %>% 
        filter(BEZEICHNUNG %in% lines)
      
      leafletProxy("map") %>% clearShapes() %>% clearMarkers()
      
      lapply(unique(filtered_df$BEZEICHNUNG), 
             function(x) {
               addPolylines(leafletProxy("map"), 
                            ~WGS84_LON,
                            ~WGS84_LAT,
                            color = ~color,
                            group = ~BEZEICHNUNG,
                            data = filtered_df[filtered_df$BEZEICHNUNG == x, ])
             })
      
      lapply(unique(filtered_df$BEZEICHNUNG), 
             function(x) {
               addCircleMarkers(leafletProxy("map"),
                                ~WGS84_LON, 
                                ~WGS84_LAT, 
                                radius = 8, 
                                color = ~color,
                                fill = "transparent",
                                opacity = .8,
                                label = ~station_label,
                                group = ~BEZEICHNUNG,
                                data = filtered_df[filtered_df$BEZEICHNUNG == x, ])
             })
      
      
    })
  },
  options = list(height = 600)
)
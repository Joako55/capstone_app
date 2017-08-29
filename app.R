#################################################Data Manipulation#######################################################
#Load required librarys.
library(purrr)
library(dplyr)
library(leaflet)
library(magrittr)
library(rgdal)
library(readxl)
library(lubridate)
library(readr)
library(shiny)
library(shinydashboard)



#Makes the market icon for the map.
PP <- awesomeIcons(
  icon = 'ion-android-cart',
  iconColor = 'white',
  library = 'ion',
  markerColor = 'red')

#Loads the shp file of the provinces of RM, Chile; Select cl_13comunas_geo.shp from comunas13.
Comunas <- readOGR("comunas13/cl_13comunas_geo.shp",
                   layer = "cl_13comunas_geo", GDAL1_integer64_policy = TRUE)

#Loads the coordenates of the supermarkets, Select Supers.xlsx .
Supers <- read_excel("data/SANTA ISABEL PARIS.xlsx", col_types = c("text","text", "text", "numeric", "numeric")) 
Puntos<-mutate(Supers,Visible = 1)

#Loads the coordenates file from clientes, Select fullcoord.xlsx .
fullcoord <- read_excel("data/fullcoord.xlsx", 
                        sheet = "fullcoord", col_types = c("blank", 
                                                           "numeric", "numeric", "text", 
                                                           "text", "blank", "blank", "numeric", 
                                                           "date", "blank", "blank", "text", 
                                                           "text", "text", "blank", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "blank", "blank", "blank", 
                                                           "blank", "blank", "text", "numeric", 
                                                           "numeric", "text", "text", "text", 
                                                           "blank"))
#fullcoord as a tibble
fullcoord<-tbl_df(fullcoord)

#Create the column day containing the day of the week of the home delivery
fullcoord$day <- weekdays(as.Date(fullcoord$FechaDespacho)) 

#################################################UI Shiny#########################################################
header <- dashboardHeader(
  title = "Distribucion de Pedidos",
  titleWidth = 300
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,box(width = NULL, solidHeader = TRUE,leafletOutput("SFmap", height = 500),collapsible = T)),
    #Tablero Lateral Superior#
    column(width = 3,box(width = NULL, status = "primary",collapsible = T,
                         selectInput(inputId = "Year",
                                     label = "Seleccione el ano",
                                     choices = c(2015,2016),
                                     selected = c(2015,2016)
                         ),
                         selectInput(inputId = "Mes",
                                     label = "Seleccione el Mes",
                                     choices = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                     selected = c(1,2,3,4,5,6,7,8,9,10,11,12)
                         )
    ),
    box(width = NULL ,status = "warning", collapsible = T,
        checkboxGroupInput("Super", "Ver Puntos de Retiro",choices = list("Mostrar Locales" = 1),
                           selected = 1)
    )
    )
  )
  
)


shinyApp(
  ui = dashboardPage( header,
                      dashboardSidebar(disable = TRUE),
                      body,skin = "green")  
  #################################################Shiny Server#########################################################
  
  ,
  server = function(input, output) {
    
    
    # Show only selected years
    
    
    output$SFmap <- renderLeaflet({
      UbicPedidos <- fullcoord
      
      if (length(UbicPedidos) == 0)
        return(NULL)
      
      
      # Show only selected years
      UbicPedidos <- filter(UbicPedidos, ano %in% c(as.numeric(input$Year)) & mes %in% c(as.numeric(input$Mes)))
      Puntos <- filter(Puntos, Visible %in% as.numeric(input$Super))
      
      
      leaflet(fullcoord) %>%
        #basic map
        addTiles() %>%
        setView(-70.6464,-33.4366, zoom = 10) %>% 
        addLegend("bottomright", colors = "#03F", labels = "Comunas") %>%
        
        #Total
        addMarkers(lng= ~(UbicPedidos$Longitude),
                   lat = ~(UbicPedidos$Latitude),
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                   popup = ~(UbicPedidos$Direccion),
                   label = ~(UbicPedidos$Direccion),
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>% 
        #Overlay Groups (multiple at time)
        addAwesomeMarkers(lng=~(Puntos$Longitude), lat=~(Puntos$Latitude) ,icon=PP , popup = ~(Puntos$Direccion)) %>%
        addPolygons(data=Comunas, color = "#3f3f3f", weight = 1, smoothFactor = 0.7,
                    opacity = 1.0, fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "Blue", weight = 2,
                                                        bringToFront = TRUE))
    })
  }
  
)
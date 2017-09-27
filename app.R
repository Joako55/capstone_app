#################################################Data Manipulation#######################################################
#Load required librarys.
library(shiny)
library(shinydashboard)
library(purrr)
library(dplyr)
library(leaflet)
library(magrittr)
library(rgdal)
library(readxl)
library(lubridate)
library(DT)
library(readr)
library(ggplot2)


#Makes the market icon for the map.
PP <- awesomeIcons(
  icon = 'ion-android-cart',
  iconColor = 'white',
  library = 'ion',
  markerColor = 'red')

ST<-awesomeIcons(
  icon = 'ion-star',
  iconColor = 'white',
  library = 'ion',
  markerColor = "green")

#Loads the shp file of the provinces of RM, Chile; Select cl_13comunas_geo.shp from comunas13.
Comunas <- readOGR("comunas13/cl_13comunas_geo.shp",
                   layer = "cl_13comunas_geo", GDAL1_integer64_policy = TRUE)

#Loads the coordenates of the supermarkets, Select Supers.xlsx .
Supers <- read_excel("data/SANTA ISABEL PARIS.xlsx", col_types = c("text","text", "text", "numeric", "numeric")) 
Puntos<-mutate(Supers,Visible = 1)

#Loads the coordenates file from clientes, Select fullcoord.xlsx .
fullcoord <- read_excel("data/fullcoord.xlsx", 
                        sheet = "fullcoord", col_types = c("skip", 
                                                           "numeric", "numeric", "numeric", 
                                                           "text", "skip", "skip", "date", 
                                                           "date", "skip", "skip", "text", 
                                                           "text", "text", "skip", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "skip", "skip", "skip", 
                                                           "skip", "skip", "text", "numeric", 
                                                           "numeric", "text", "text", "text", 
                                                           "skip"))
#fullcoord as a tibble
fullcoord<-tbl_df(fullcoord)

#Create the column day containing the day of the week of the home delivery
fullcoord$day <- weekdays(as.Date(fullcoord$FechaDespacho))
fullcoord$Hcreacion<-format(fullcoord$Hcreacion,"%H:%M:%S")
fullcoord$FechaDespacho<-format(fullcoord$FechaDespacho,"%d-%m-%Y")


a<-fullcoord %>% 
  group_by(ano,mes) %>%
  summarise(demand = n()) %>%
  ungroup() %>% 
  mutate(Id = row_number())


#################################################UI Shiny#########################################################
header <- dashboardHeader(
  title = "Distribucion de Pedidos",
  titleWidth = 300
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Mapa",
        fluidRow(
            column(width = 9,box(width = NULL, solidHeader = TRUE,leafletOutput("SFmap", height = 500),collapsible = T),
                   box(width = NULL, plotOutput("Ventas"))),
            
            #Tablero Lateral Superior#
            
            column(width = 3,box(width = NULL, status = "primary",collapsible = T,
                         sliderInput("Year", "ano:",
                                     min = as.numeric(min(fullcoord$ano, na.rm=TRUE)), max = as.numeric(max(fullcoord$ano, na.rm = TRUE)+1),
                                     value = c(as.numeric(min(fullcoord$ano, na.rm=TRUE)),as.numeric(max(fullcoord$ano, na.rm=TRUE)))),
                         
                         sliderInput("mes", "mes:",
                                     min = 1, max = 12, value = c(1,12)),
                         
                         sliderInput("dia", "dia:",
                                     min = 1, max = 31, value = c(1,31))
                        
                            ),
                   
                          box(width = NULL ,status = "warning", collapsible = T,
                            checkboxGroupInput("Super", "Ver Puntos de Retiro",choices = list("Mostrar Locales" = 1),
                                                selected = 1)
                              ),
                          box(width = NULL ,status = "warning", collapsible = T,actionButton("action", label = "Update"))
                  )
                )
    ),
    
    tabItem(tabName = "Datos",
            fluidRow(
              column(width = 10,
              DT::dataTableOutput("table")
            )
            )
    ),
    
    tabItem(tabName = "Archivos",
            fluidRow(
                     # App title ----
                     titlePanel("Uploading Files"),
                     
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       sidebarPanel(
                         
                         # Input: Select a file ----
                         fileInput("file1", "Choose CSV File",
                                   multiple = TRUE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         
                         # Horizontal line ----
                         tags$hr(),
                         
                         # Input: Checkbox if file has header ----
                         checkboxInput("header", "Header", TRUE),
                         
                         # Input: Select separator ----
                         radioButtons("sep", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t"),
                                      selected = ","),
                         
                         # Input: Select quotes ----
                         radioButtons("quote", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = '"'),
                         
                         # Horizontal line ----
                         tags$hr(),
                         
                         # Input: Select number of rows to display ----
                         radioButtons("disp", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")
                         
                       ),
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         
                         # Output: Data file ----
                         tableOutput("contents")
                         
                       )
                       
                     )
                     
              )
            )
    )
  )           


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",tabName = "Mapa",icon = icon("dashboard")),
    menuItem("Datos",tabName = "Datos",icon = icon("database")),
    menuItem("Archivos",tabName = "Archivos",icon = icon("upload"))
    )
  )

shinyApp(
  ui = dashboardPage( header,
                      sidebar,
                      body,skin = "green")  
  #################################################Shiny Server#########################################################
  
  ,
  server = function(input, output, session) {
    #Load CSV
    output$contents <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
    #Sales Graph
    output$Ventas <- renderPlot({
      plot<-ggplot(a,aes(Id,demand,group = 1)) + geom_line() + labs(title="Pedidos Online",x ="Periodo", y = "Cantidad Pedidos")
      print(plot)
    })
    
    #Data Table
    output$table <- DT::renderDataTable(DT::datatable(fullcoord, options = list(scrollX = TRUE)))
      
      output$SFmap <- renderLeaflet({
        UbicPedidos <- fullcoord
        
        if (length(UbicPedidos) == 0 || (max(fullcoord$ano)+1 == input$Year[1] & max(fullcoord$ano)+1 == input$Year[2]))
          return(NULL)
        
        
        # Show only selected years
        UbicPedidos <- filter(UbicPedidos, ano  >= input$Year[1],ano <= input$Year[2], mes <= input$mes[2], mes >= input$mes[1], dia <= input$dia[2], dia >= input$dia[1])
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
                     popup = paste("Despachado el",UbicPedidos$FechaDespacho,", En Ventana",UbicPedidos$Ventana,", Por un monto de",UbicPedidos$`Venta Neta`,sep = " "),
                     label = ~(UbicPedidos$Direccion),
                     labelOptions = labelOptions(noHide = F,
                                                 direction = 'auto')) %>% 
          #Overlay Groups (multiple at time)
          addAwesomeMarkers(lng=~(Puntos$Longitude), lat=~(Puntos$Latitude) ,icon=PP , popup = ~(Puntos$Direccion)) %>%
          addAwesomeMarkers(lng= -70.64689, lat= -33.45098 ,icon=ST , popup = "Strip Center") %>%
          addPolygons(data=Comunas, color = "#3f3f3f", weight = 1, smoothFactor = 0.7,
                      opacity = 1.0, fillOpacity = 0.5,
                      highlightOptions = highlightOptions(color = "Blue", weight = 2,
                                                          bringToFront = TRUE))
      })
    }
)
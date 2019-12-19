
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(data.table)
library(shinydashboard)
library(gridExtra)
library(writexl)
library(DT)
library(plotly)
library(rhandsontable)
library(openxlsx)
library(parcoords)
library(leaflet)
library(chorddiag)
#library(d3heatmap)
library(visNetwork)
library(leaflet.extras)
library(shinyjqui)
#Dias <- read.delim("data/Dias.txt", header = F)
#Pruebas <- as.character(levels(Prueb[,1]))
#Grup <- read.delim("data/Grupos.txt", header = F)
#Grupos <- as.character(levels(Grup[,1]))

#func <- read.delim("data/funciones.txt", header= F)
#Funciones <- as.character(levels(func[,1]))

dashboardPage(
  dashboardHeader(title = "AVIMEU",
                  titleWidth = 350
  ),
  dashboardSidebar(
    width = 120,
    sidebarMenuOutput("menuAPP")
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data",
              fluidRow(
                box(
                  width = 12,
                  title = "Upload file", solidHeader = TRUE, collapsible = TRUE,
                  radioButtons("tipoDatos", "Seleccione el tipo de datos:",
                               c("Explicitos" = "Explicitos",
                                 "Implicitos" = "Implicitos"
                                 )),
                  fileInput("file1", "",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv", "text/tab-separated-values",
                                       ".csv",
                                       ".xls",
                                       ".xlsx")), 
                  uiOutput("varIDTRACK")
                ),
                
                #box(
        
                 # title = "Datos", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  DT::dataTableOutput('Table1')
                #)
              ) 
              
      ),
      
      tabItem(tabName = "trans",
              fluidRow(
                column(width = 4,
                  box(width = NULL,
                    title = "Variables Selección",status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("selVariables")
                  )),
                column(width = 8, 
                       box(width = NULL,
                         title = "Selección de filtros", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                         uiOutput("varFiltro"),
                         DT::dataTableOutput("tabReumNum"),
                         uiOutput("selFiltros"),
                         actionButton("add", "Guardar Filtro"),
                         actionButton("filtrar", "Aplicar Filtros", icon("fa-skull")),
                         actionButton("rmv", "Quitar Filtros", icon("refresh")),
                         textOutput('textStatus'), 
                         tags$head(tags$style("#textStatus{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"))
                       ), uiOutput("boxFiltros")
                       
                )
              ), 
      ),
      
      tabItem(tabName = "MulAn",
              jqui_draggable(fluidRow(
                jqui_resizable( box(width = NULL,
                    title = "Trajectory Map",status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("selVarTraj"),
                    leafletOutput("mapTraj")
                    )
              ))),
              jqui_draggable(fluidRow(
                jqui_resizable(box(width = NULL,
                    title = "Gráfico Radar",status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("selCatRadar"), uiOutput("sliderRadar"),
                    plotlyOutput("plotRadar")
                )
              ))),
              jqui_draggable(fluidRow(
                box(width = NULL,
                    title = "Valores por categoría",status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fixedRow(
                      column(3, plotlyOutput("Plot3", height = "600px")),
                      column(9, uiOutput("catFecha"), uiOutput("catNum"), #, uiOutput("catBoxPlotRad"),
                             plotlyOutput("Plot4", height = "600px")))
                    )
              )),
              jqui_draggable(fluidRow(
                jqui_resizable(box(width = NULL, 
                    title = "Coordenadas Paralelas",status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("selCatParCoor"),
                    parcoordsOutput("plotPcor")
                   ) 
              ))),
      ),
      tabItem(tabName = "afir",
              jqui_draggable(fluidRow(
                jqui_resizable(box(
                width = NULL, 
                title = "Red puntos de interés", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                sliderInput("nCluster", "Numero de Clusters:", min = 2, max = 20, value = 3),
                uiOutput("listaHorarios"), uiOutput("distPOIS"), 
                visNetworkOutput("plot_network"),
              )))), 
              jqui_draggable(fluidRow(
                jqui_resizable(box(
                width = NULL, 
                title = "Trayectorias de los cluster ", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                uiOutput("listaCluster"),
                leafletOutput("mapPOIS")
              ))))
      ),      
      
      tabItem(tabName = "ODAn",
              jqui_draggable(fluidRow(
                jqui_resizable(box(
                width = NULL, 
                title = "Matriz Origen - Destino", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                uiOutput("selCatODMatrix"),
                plotlyOutput("matOD")
               )))), 
              
              jqui_draggable(fluidRow(jqui_resizable(box(
                width = NULL, 
                title = "Diagrama de Cuerdas Origen - Destino", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                chorddiagOutput("plotChord")
              )),options = list(aspectRatio = TRUE))),
              
              fluidRow(box(
                width = NULL, 
                title = "Heatmap Temporal", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                uiOutput("selFechaOD"), uiOutput("selFormatoFechaOD"), 
                leafletOutput("mapOD")#, 
                #d3heatmapOutput("d3HeatMapOD")
              ))
      ), 
      tabItem(tabName = "CoOc",
              fluidRow(box(
                width = NULL, 
                title = "Selección Destino para análisis Concurrencia", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                uiOutput("varCoOc"), 
                fixedRow(
                  column(7, leafletOutput("mapCoOc")),
                  column(5, DT::dataTableOutput("infoMapCoOC"))
                )
              )), 
              fluidRow(box(
                width = NULL, 
                title = "Scatter Polar Time", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                uiOutput("selPolarCoOc"), 
                plotlyOutput("plotPolar")
              ))
      ), 
      tabItem(tabName = "path",
              fluidRow(box(
                width = NULL, 
                title = "Selección Filtros Path", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                fixedRow(
                  column(4, radioButtons("tipoHorario", "Seleccione el tipo de filtro a realizar",
                               c("Dias de la semana" = "daySemana" ,
                                 "Dias del mes" = "dayMes")), 
                         uiOutput("listaHorPath"), uiOutput("distPath"), uiOutput("numPath"), 
                         uiOutput("slidernumPath"), 
                         actionButton("filtrarPath", "Aplicar Filtros", icon("fa-skull"))),
                  column(8, leafletOutput("mapPath1"))
                )
              )), 
              fluidRow(box(
                width = NULL, 
                title = "Comportamiento de trayectoria", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                uiOutput("selPath"), uiOutput("selNumPath2"),
                leafletOutput("mapPath2")
              ))
      )
    )
  )
)


################################################################################
# # Server.R
# # R Versions: R version 3.5.3 
# #
# # Author(s): Areli Moreno
# #
# # TRANSVERSAL 
# # Description: APP for compare set of items for proficiency levels
# #             
# #
# # 
# #
# # File history:
# #   210604: Creation
# #   
# #       
#################################################################################
################################################################################
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(writexl)
library(DT)
library(data.table)
library(plotly)
library(rhandsontable)
library(openxlsx)
library(arsenal)
library(purrr)
library(leaflet) # for  maps with ESRI
library(parsedate)
library(parcoords)
library(crosstalk)
library(fpc)
library(lubridate)
library(d3heatmap)
library(visNetwork)
library(analyzeGPS)
library(geosphere)
library(nlme)
library(RColorBrewer)

source("ODMatriz.R")
source("preparedata.R") # function for detect dates vars and create vars for granularity
source("isOD.R")
source("varslatlon.R")
source("tipedata.R")
source("createtraj.R")
source("Radar.R")
source("plotcor.R")

pathSave <- "optFilterAPP.Rdata"
if (file.exists(pathSave)) {
  file.remove(pathSave)
}
auxSel <- list(placeholder = 'Por favor selecion un opción',
               onInitialize = I('function() { this.setValue(""); }'))

flagFile   <- FALSE # Se prende al leer un archivo
flagFiltro <- FALSE # Se prende al hacer un filtro
vecFecha <- c("Mes (%m)", "Dia (%d)", "Hora (%d %H:%M:%S)")  # Vector para granularizar fechas
vecTiempo <- c("MORNING", "AFTERNOON", "NIGHT")
vecDist  <- c("distHaversine", "distGeo", "distVincentyEllipsoid")

# # Funcion para pasar de explicitos o implicitos
extracFiltro <- function(var){  
  if (var$auxTipo == "varchar") {
    value <- paste(var$slideCat, collapse = " ")
  } 
  if (var$auxTipo == "varcont") {
    value <- paste(c(var$slideNum1, var$slideNum2), collapse = "||")
  }
  else {
    value <- paste(c(var$slideDate1, var$slideDate2), collapse = "||")
  }
  return (data.frame('Variable' = var$varFiltro, 'Tipo' = var$auxTipo, 'Valores Filtro' = value))
}

getLimHorario <- function(vecFecha, horarioSel) {
  indVector <- rep(FALSE, length(vecFecha))
  if ("NIGHT" %in% horarioSel) {
      indVector <- indVector | ((vecFecha >= 18  & vecFecha < 24) | vecFecha < 5)
  }
  if ("AFTERNOON" %in% horarioSel) {
      indVector <- indVector | (vecFecha >= 13  & vecFecha < 19)
  }
  if ("MORNING" %in% horarioSel) {
      indVector <- indVector | (vecFecha >= 5  & vecFecha < 13)
  }
  return (indVector)
}

lenGranFecha <- function(formaSel) {
  ifelse(formaSel == "Mes (%m)", 12, ifelse(formaSel == "Dia (%d)", 7, 24))
}

granularizaFecha <- function(vecDate, formaSel) {
    if (formaSel == "Mes (%m)") return(lubridate::month(vecDate))
    if (formaSel == "Dia (%d)") return(lubridate::day(vecDate))
    if (formaSel == "Hora (%d %H:%M:%S)") return(lubridate::hour(vecDate))
}

odExplicito <- function(dFAux, idLugar, idTrack, fechaOrdenar, latLongVars, varNums){
  #idLugar = "Ubicacion"
  #idTrack = "NEW_ID_TRACK"
  #fechaOrdenar = "seg_fecha"
  #latLongVars = c("seg_latitu", "seg_longit")
  
  # # Agregacion por idTrack / idLugar
  dFAux <- dFAux[!is.na(dFAux[[idLugar]]), ]
  dFAux[[fechaOrdenar]] = parsedate::parse_date(dFAux[[fechaOrdenar]])
  suppressWarnings(dFAux[, c("APP_MAX_FECHA_AVIMENU", "APP_MIN_FECHA_AVIMENU") := .(max(get(fechaOrdenar)), min(get(fechaOrdenar))), 
                         by = c(idLugar, idTrack)])
  varAgrega <- dFAux[, lapply(.SD, mean), by = c(idLugar, idTrack),  .SDcols = varNums]
  varTomar <- c(idLugar, idTrack, "APP_MAX_FECHA_AVIMENU", "APP_MIN_FECHA_AVIMENU", fechaOrdenar, latLongVars)
  dFAux <- dFAux[APP_MIN_FECHA_AVIMENU == dFAux[[fechaOrdenar]], varTomar, with = FALSE]
  
  ## Encontrando origenes y destinos
  dFAux <- merge(dFAux, varAgrega, by = c(idLugar, idTrack))
  dFAux <- dFAux[order(get(idTrack), get(fechaOrdenar)), ]
  dFAux[, nCons := 1:.N, by = idTrack]
  dfDesti <- dFAux[, shift(.SD, -1), by = c(idTrack)]
  dfDesti[, nCons := 1:.N, by = idTrack]
  
  # # validando y retornando datos
  names(dfDesti) <- c(paste0(idTrack, "_Destino"), paste0(names(dFAux)[-2], "_Destino"), 'nCons')
  names(dFAux) <- paste0(names(dFAux), "_Origen")
  dfFinal <- merge(dFAux, dfDesti, by.x = c(paste0(idTrack,"_Origen"), "nCons_Origen"), by.y = c(paste0(idTrack,"_Destino"), "nCons"))
  dfFinal <- dfFinal[!is.na(Ubicacion_Destino), ]
  setnames(dfFinal, paste0(idTrack,"_Origen"), "NEW_ID_TRACK")
  #save(dfFinal, file = "dfFinal.Rdata")
  return(dfFinal)
}

ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, distVincentyEllipsoid(c(g1$lat, g1$lon), c(g2$lat, g2$lon))))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}

################################################################################
# # Execute 
################################################################################

server <- function(input, output, session) {

  getEstado <- function(revisaFiltro){
    if (!flagFile) warning("O_O Se debe definir un archivo para procesar\n")
    if (!flagFiltro & revisaFiltro) warning("O_O Se debe hacer un filtro a las variables\n")
  }

  # Creacion de reactive values para diferentes funciones
  baseFin    <- reactiveValues(df_data = NULL, df_clus = NULL)
  baseNet    <- reactiveValues(df_link = NULL, df_nodes = NULL, df_map = NULL, df_lnodes=NULL)
  basePath   <- reactiveValues(df_map = NULL, df_map2 = NULL, listIDs = NULL)
  baseFinOD  <- reactiveValues(df_data = NULL, df_data_agree = NULL)
  menu_vals  <- reactiveValues(menu_list = NULL)
  data_of_click_OD <- reactiveValues(clickedMarker=NULL)
  baseFiltros <- reactiveValues(df_filtros = NULL)

  ## Modificación al Menu
  options(shiny.maxRequestSize=1000*1024^10)
  varToFilter <- reactive({
    c(input$varsNum, input$varsCat, input$vatsFec)
  })

  observeEvent(input$varIDTRACK, {
    req(input$varIDTRACK)
    baseFin$df_data[, NEW_ID_TRACK := .GRP , by = eval(input$varIDTRACK)]
  })

  observeEvent(input$tipoDatos, {
    req(input$tipoDatos, input$tipoDatos !="NA")
    if (input$tipoDatos == "Implicitos" & flagFiltro & length(menu_vals$menu_list) == 4) {
      menu_vals$menu_list[[5]] <- menuItem("OD Analisis", tabName = "ODAn", icon = icon("area-chart"))
      baseOD$df_data = baseFin$df_data
    }
    condUn = input$tipoDatos == "Explicitos" & length(input$varOD) != 0
    if (input$tipoDatos == "Explicitos" & length(menu_vals$menu_list) == 5 & !condUn) {
      menu_vals$menu_list[[5]] <- NULL
    }
  })

  observeEvent(input$varOD, {
    req(input$varOD, input$varOD!="NA")
    condUn = input$tipoDatos == "Explicitos" & length(input$varOD) != 0
    if (condUn & flagFiltro & length(menu_vals$menu_list) == 4 ) {
      req(input$vatsFec, input$varLat,  input$varLon)
      menu_vals$menu_list[[5]] <- menuItem("OD Analisis", tabName = "ODAn", icon = icon("area-chart"))
      baseFinOD$df_data <- odExplicito(baseFin$df_data, input$varOD, "NEW_ID_TRACK", input$vatsFec, c(input$varLat,  input$varLon), tipevars$varcont)
    }

    condUn2 = input$tipoDatos == "Explicitos" & length(input$varOD) == 0
    if (condUn2 & length(menu_vals$menu_list) == 5){
      menu_vals$menu_list[[5]] <- NULL
    }
  })

  output$menuAPP <- renderMenu({   
    sidebarMenu(id = "menuAPP", .list = c(list(
        menuItem(HTML("View <br/> DAta"), tabName = "data", icon = icon("table")),
        menuItem(HTML("Transform <br/> Data"), tabName = "trans", icon = icon("table"))),
        menu_vals$menu_list
    ))
  })

  output$varIDTRACK <- renderUI({
    getEstado(revisaFiltro = FALSE)
    varFilter <- names(baseFin$df_data)
    output <- tagList()
    output[[1]] <- selectizeInput("varIDTRACK", "Selección de ID_TRACK", 
                                  multiple = T, choices = varFilter, options = auxSel)
    
    # # Cambiando de acuerdo a la seleccion
    #selNomDes <- setdiff(varFilter, c(input$varLatLogOri, input$varLatLogDest, input$varNomOri))
    #selNomOri <- setdiff(varFilter, c(input$varLatLogOri, input$varLatLogDest, input$varNomDest))
    #selLTLOOri <- setdiff(varFilter,c(input$varLatLogDest, input$varNomDest, input$varNomOri))
    #selLTLODes <- setdiff(varFilter, c(input$varLatLogOri, input$varNomDest, input$varNomOri))

    if (input$tipoDatos == "Explicitos") {
      output[[2]] <- selectizeInput("varOD", "Selección variables Origen-Destino", multiple = T, 
                                 choices = varFilter, options = auxSel)
    } else {
      output[[3]] <- fluidRow(column(width = 4, selectizeInput("varLatLogOri", "Latitud /Longitud/Fecha (Órigen)", choices = varFilter, multiple = T, options = list(maxItems = 3)),
                              selectizeInput("varNomOri", "Nombre Origen", multiple = F, choices = varFilter, options = auxSel)), 
                              column(width = 4, selectizeInput("varLatLogDest", "Latitud /Longitud/Fecha (Destino)", choices = varFilter, multiple = T, options = list(maxItems = 3)),
                              selectizeInput("varNomDest","Nombre Destino",  multiple = F, choices = varFilter, options = auxSel)), 
                              column(width = 2, textOutput("msjImpli"))
                              )
    }
    return(output)
  })
  
  output$msjImpli <- renderText({
    mensaje <- ""
    if (length(input$varLatLogOri) < 3){
      mensaje <- paste0(mensaje, "..... O_O Se debe definir Longitud, Latitud y Fecha de Origen \n")  
    }
    if (length(input$varLatLogDest) < 3){
      mensaje <- paste0(mensaje, "..... O_O Se debe definir Longitud, Latitud y Fecha de Destino \n")
    }
    if (input$varNomOri == ""){
      mensaje <- paste0(mensaje, "..... O_O Se debe definir nombre de Origen              \n")
    }
    if (input$varNomDest == ""){
      mensaje <- paste0(mensaje, "..... O_O Se debe definir nombre de Destino             \n")
    }
    return(mensaje)
  })

 
  output$varFiltro <- renderUI({
    varFilter = varToFilter()
    selectInput("varFilInt", "Selección de valores", multiple = F, 
                choices = varFilter, selected = varFilter[1])
  })
  
  varSeleccionada <- reactive({
    getEstado(revisaFiltro = TRUE)
    req(input$varFilInt)
    auxVar  <- input$varFilInt
    vecTipo <- map_lgl(tipevars, ~  auxVar %in% .x)
    auxTipo <- names(vecTipo[vecTipo])
    if (identical(auxTipo, "varchar")) {
      datSum <- baseFin$df_data[, .N, by = auxVar]
    } else {
      datSum <- rSumm(baseFin$df_data, yVars = auxVar, nRound = 2, flagDate = auxTipo == "varsdate")  
    }
    list(varFiltro = input$varFilInt, auxTipo = auxTipo, datSum = datSum)
  })

  # Coupled event 1
  output$catNum <- renderUI({
    selectInput("varsNumBox", "Variables Númericas", multiple = F, choices = input$varsNumRadar, 
                selected = input$varsNumRadar[1])
  })
  
  #output$catBoxPlotRad <- renderUI({
  #  optionsAux = setdiff(input$varsCat, input$varsCatRadar)
  #  selectInput("varsCatBox", "Variables CategÃ³ricas", multiple = F, choices = optionsAux, 
  #              selected = optionsAux[1])
  #})
  
  output$Plot3 <- renderPlotly({
    getEstado(revisaFiltro = TRUE)
    auxVar <- input$varsCatRadar
    # Assign to parent frame
    plot.summ <<- baseFin$df_data[, .N, by = auxVar]
    names(plot.summ) <<- c("Class", "Count")
    
    # Plot
    plot_ly(plot.summ, x = ~Class, y = ~Count, type = "bar", source = "select", 
            color = ~Class, showlegend = F) %>%
      layout(title = "No. de observaciones <br> en la selección",
             yaxis = list(domain = c(0, 0.9)))
  })  


  output$catFecha <- renderUI({
    selectInput("repreFecha", "Seleccione el formato para esta fecha", 
                multiple = F, choices = vecFecha, selected = vecFecha[1])
  })
  
  output$Plot4 <- renderPlotly({
    getEstado(revisaFiltro = TRUE)
    req(input$vatsFec, input$varsCatRadar, input$varsNumBox)
    # Get subset based on selection
    event.data <- event_data("plotly_click", source = "select")
    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)

    # graph
    tab <- baseFin$df_data[baseFin$df_data[[input$varsCatRadar]] == event.data[['x']], ]
    tab[[input$vatsFec[1]]] <- granularizaFecha(tab[[input$vatsFec[1]]], input$repreFecha)
    tab <- tab[, lapply(.SD, mean, na.rm = TRUE), by = eval(input$vatsFec[1]), 
                .SDcols = eval(input$varsNumBox)]
    tab <- tab[order(get(input$vatsFec[1])),]
    
    
    plot_ly(tab, x = ~get(input$vatsFec[1]), y = ~get(input$varsNumBox), #color = ~get(input$varsCatBox),  
            type = "scatter", showlegend = T, mode = 'lines') %>%
    layout(title = sprintf("Graficando variable (%s - %s)", input$varsNumBox, event.data[['x']]))
  })

  output$selCatParCoor <- renderUI({
    output = tagList()
    output[[1]] = selectizeInput("varsNumPC", "Variables Númericas", multiple = T, choices = input$varsNum, selected = input$varsNum[1:4])
    output[[2]] = selectInput("varsCatPC", "Variables Categóricas", multiple = F, choices = input$varsCat, selected = input$varsCat[1])
    output
  })
  
  parCorSum <- reactive({
    req(input$varsNumPC, input$varsCatPC)
    baseFin$df_data[, lapply(.SD, mean, na.rm = TRUE), by = eval(input$varsCatPC), 
                    .SDcols = eval(input$varsNumPC)]
  })
    
  output$plotPcor <- renderParcoords({
    plotcor(parCorSum(), input$varsCatPC) 
    plotcoor
  })

  output$selFiltros <- renderUI({
    auxVar <- varSeleccionada()
    output = tagList()
    if (auxVar$auxTipo == "varsdate") {
      output[[1]] = div(h3("Filtro variable Fecha-", auxVar$varFiltro), 
                        dateRangeInput("slideDate", 
                                   label = h3(HTML("<i class='glyphicon glyphicon-calendar'></i> Rango de Fecha")), 
                                   min = auxVar$datSum[, Minimo], max = auxVar$datSum[, Maximo],
                                   start = auxVar$datSum[, Minimo], end = auxVar$datSum[, Maximo]))
    }
    if (auxVar$auxTipo == "varcont") {
      output[[1]] = div(h3("Filtro variable Númerica", auxVar$varFiltro), 
          sliderInput("slideNum", "Seleccione el Rango", auxVar$datSum[, Minimo], auxVar$datSum[, Maximo], 
                      value = c(auxVar$datSum[, Per25], auxVar$datSum[, Per75])))
    }
    
    if (auxVar$auxTipo == "varchar") {
      output[[1]] = div(h3("Resumen de variable Categórica", auxVar$varFiltro),
          selectInput("slideCat", "Seleccione categóricas", multiple = T, choices = auxVar$datSum[[auxVar$varFiltro]], 
                      selected = auxVar$datSum[[auxVar$varFiltro]]))
    }
    output
  })
  
  output$selCatODMatrix <- renderUI({
    output = tagList()
    namesAux = names(baseFinOD$df_data)
    output[[1]] = selectizeInput("varODMAT_cat", "Variables Origen-Destino", multiple = T, choices = namesAux, selected = c(), options = list(maxItems = 2))
    output[[2]] = selectInput("varODMAT", "Variables Númericas", multiple = F, choices = c('Conteo', namesAux), selected = 'Conteo')
    output
  })
  
 
  baseOD <- reactive({
    req(input$varODMAT, input$varODMAT_cat, length(input$varODMAT_cat) == 2)
    if (input$varODMAT == "Conteo") {
      Base1 <- baseFinOD$df_data[, .(summaryVar = .N), by = eval(input$varODMAT_cat)]
    } else {
      Base1 <- baseFinOD$df_data[, .(summaryVar = mean(.SD)), by = eval(input$varODMAT_cat), .SDcols = eval(input$varODMAT)]
    }
    Base1 <- Base1[rowSums(is.na(Base1)) == 0, ]
    save(Base1, file = "Base1.Rdata")
    return(Base1)
  })
  
  output$matOD <- renderPlotly({
    req(input$varODMAT, input$varODMAT_cat, length(input$varODMAT_cat) == 2)
    Base1 <- baseOD()
    ODMatrix(Base1, input$varODMAT_cat, 'summaryVar')
  })
  
  output$plotChord <- renderChorddiag({
    req(input$varODMAT, input$varODMAT_cat, length(input$varODMAT_cat) == 2)
    df_chord <- baseOD()
    nivAll <- unique(c(as.character(df_chord[[1]]),  as.character(df_chord[[2]])))

    # # Tabla incial Grafico 
    names(df_chord) <- c("X", "Y", "VALOR")
    df_chord <- dcast(df_chord, X ~ Y, value.var = "VALOR", fill = 0)
    df = as.matrix(df_chord[, -1])
    row.names(df) = df_chord[[1]]
    
    # # Completando matrix
    faltCol <- setdiff(nivAll, dimnames(df)[[2]])
    faltFil <- setdiff(nivAll, dimnames(df)[[1]])
    if (length(faltCol) > 0) df <- cbind(df, sapply(faltCol, function(x) rep(0, nrow(df))))
    if (length(faltFil) > 0) df <- rbind(df, t(sapply(faltFil, function(x) rep(0, ncol(df)))))
    chorddiag(df, showTicks = F, groupnameFontsize = 10, groupnamePadding = 6, margin = 10)
  })
  
  baseMapOD <- reactive({   
    if (input$tipoDatos == "Implicitos"){
      req(input$varLatLogOri, input$varLatLogDest)
      selOrigen <- select(baseFinOD$df_data, c("NEW_ID_TRACK", input$varLatLogOri))
      setnames(selOrigen,  input$varLatLogOri, c("lat", "long", "Date"))
      selDestino <- select(baseFinOD$df_data, c("NEW_ID_TRACK", input$varLatLogDest)) 
      setnames(selDestino,  input$varLatLogDest, c("lat", "long", "Date"))
    } else {
      req(input$varOD, input$vatsFec, input$varLat, input$varLon)
      selDestino <- paste0(c(input$varLat,  input$varLon, input$vatsFec), "_Destino")
      selOrigen  <- paste0(c(input$varLat,  input$varLon, input$vatsFec), "_Origen")
      selOrigen  <- select(baseFinOD$df_data, c("NEW_ID_TRACK", selOrigen))  
      selDestino <- select(baseFinOD$df_data, c("NEW_ID_TRACK", selDestino))       
      
      # # Ajustando Nombres
      names(selOrigen) <- gsub("_Origen", "", names(selOrigen))
      names(selDestino) <- gsub("_Destino", "", names(selDestino))
      setnames(selOrigen,  c(eval(input$varLat),  eval(input$varLon), input$vatsFec), c("lat", "long", "Date"))
      setnames(selDestino, c(eval(input$varLat),  eval(input$varLon), input$vatsFec), c("lat", "long", "Date"))
    }
    bind_rows(list(selOrigen, selDestino))
  })

  output$selFechaOD <- renderUI({
    selectizeInput("fechaOD", "Seleccione una variable de Fecha", 
                   multiple = F, choices = names(baseFinOD$df_data), options = auxSel)
  })

  output$selFormatoFechaOD <- renderUI({
    selectizeInput("repreFechaOD", "Seleccione el formato para agregados", 
                   multiple = F, choices = vecFecha, options = auxSel)
  })


  observeEvent(c(input$repreFechaOD, input$fechaOD), {
    req(input$varODMAT, input$varODMAT_cat, input$repreFechaOD, input$fechaOD)
    baseFinOD$df_data[['APP_varFecOD']] <- granularizaFecha(baseFinOD$df_data[[input$fechaOD]], input$repreFechaOD)
    if (input$varODMAT == "Conteo") {
      baseFinOD$df_data_agree <- baseFinOD$df_data[, .(summaryVar = .N), 
                                                by = c(eval(input$varODMAT_cat), 'APP_varFecOD'), 
                                                 .SDcols = eval(input$varODMAT)]
    } else {
      baseFinOD$df_data_agree <- baseFinOD$df_data[, .(summaryVar = mean(.SD)), 
                                                by = c(eval(input$varODMAT_cat), 'APP_varFecOD'), 
                                                 .SDcols = eval(input$varODMAT)]      
    }
    baseFinOD$df_data_agree[['Origen_destino']] <- paste(baseFinOD$df_data_agree[[input$varODMAT_cat[1]]],
                                                         baseFinOD$df_data_agree[[input$varODMAT_cat[2]]], sep=" to ")

  })

  output$mapOD <- renderLeaflet({
    req(input$varODMAT, input$varODMAT_cat, input$repreFechaOD, input$fechaOD)
    # build data with 2 places
    leaflet(baseMapOD()) %>% addTiles(options = providerTileOptions(noWrap = TRUE)) %>% 
    addCircleMarkers(~long, ~lat, layerId=~NEW_ID_TRACK, popup = ~as.character(NEW_ID_TRACK), radius = 0.02)
  })

  observeEvent(input$mapOD_marker_click, {
    req(input$repreFechaOD, input$fechaOD)
    indOrigen <- baseFinOD$df_data[['NEW_ID_TRACK']] == input$mapOD_marker_click$id
    indOrigen <- unique(baseFinOD$df_data[indOrigen][[input$varODMAT_cat[1]]])
    indOrigen <- baseFinOD$df_data_agree[[input$varODMAT_cat[1]]] == indOrigen
    Baseheatf <- dcast(baseFinOD$df_data_agree[indOrigen,], Origen_destino ~ APP_varFecOD, 
                       value.var =  "summaryVar")
    data_of_click_OD$clickedMarker <- Baseheatf[-1]
    rownames(data_of_click_OD$clickedMarker) <- Baseheatf$Origen_destino
  })

  
  
    
   #output$d3HeatMapOD <- renderD3heatmap({
  #   req(input$repreFechaOD, input$fechaOD)
  #   if (is.null(data_of_click_OD$clickedMarker)) return(NULL)
  #   d3heatmap(data_of_click_OD$clickedMarker, colors = input$palette, dendrogram =  "none")
  # })

  output$selCatRadar <- renderUI({
    output = tagList()
    output[[1]] = selectizeInput("varsNumRadar", "Variables Númericas", multiple = T, choices = input$varsNum, options = c(auxSel, maxItems = 4))
    output[[2]] = selectizeInput("varsCatRadar", "Variables Categóricas", multiple = F, choices = input$varsCat, options = auxSel)
    output
  })
  
  output$sliderRadar <- renderUI({
    req(input$varsNumRadar, input$varsCatRadar)
    rangeRadar = radarSum()
    rangeRadar = suppressWarnings(summary(melt(rangeRadar, id.vars=input$varsCatRadar, 
                              measure.vars = input$varsNumRadar)$value))
    val = c(rangeRadar['1st Qu.'], rangeRadar['3rd Qu.'])
    sliderInput("rangoRadar", "Seleccione el Rango", 
                min = rangeRadar['Min.'], max = rangeRadar['Max.'],
                value = val)
    
  })

  
  
  radarSum <- reactive({
    getEstado(revisaFiltro = TRUE)
    sumRdar = baseFin$df_data[, lapply(.SD, mean, na.rm = TRUE), 
               by = eval(input$varsCatRadar), .SDcols = eval(input$varsNumRadar)]
    return(sumRdar)
    
  })
  
  output$plotRadar <- renderPlotly({
    req(input$varsNumRadar, input$varsCatRadar)
    radar(radarSum(), input$rangoRadar)
    radarplot
  })
  
  output$selVarTraj <- renderUI({
    selectizeInput("varNumTraj", "Variables Númericas para trajectoria", multiple = F, choices = input$varsNum, options = auxSel)
  })
    
  output$mapTraj <- renderLeaflet({
    req(input$tipoDatos, input$varNumTraj)
    if (input$tipoDatos == "Explicitos"){
      createtraj(baseFin$df_data, tipevars$varlat, tipevars$varlon, input$varNumTraj)
    } else {
      indError <- (length(input$varLatLogOri) != 3) | (length(input$varLatLogDest) != 3) | (input$varNomOri == "") | (input$varNomDest == "")
      if(indError) stop("...Debe revisar la seleccion inicial en la pestaña (View)") 
      Names_point(baseFin$df_data[1:100, ], input$varLatLogOri[1], input$varLatLogOri[2], input$varLatLogDest[1], input$varLatLogDest[2], input$varNomOri, input$varNomDest)
      createtrajOD(baseFin$df_data[1:100, ], input$varLatLogOri[1], input$varLatLogOri[2], input$varLatLogDest[1], input$varLatLogDest[2], input$varNomOri, input$varNomDest, 2)
    }
    traj
  })
  
  output$boxFiltros <- renderUI({
    if (is.null(baseFiltros$df_filtros)){
      auxFiltro <- div() 
    } else {
      auxFiltro <- box(with = 8, title = "Filtros seleccionados", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                       dataTableOutput('TableFiltros'))
    }
    return(auxFiltro)
  })
  
  tabFiltr <- reactive({
    req(!is.null(baseFiltros$df_filtros))
    return(map_df(baseFiltros$df_filtros, ~extracFiltro(.x)))
  })
  
  output$TableFiltros <- DT::renderDataTable({
    DT::datatable(tabFiltr(), rownames = FALSE,
                  options = list(autoWidth = TRUE,
                                 initComplete = JS( "function(settings, json) {",
                                                    "$(this.api().table().header()).css({
                                                    'background-color': '#bdd7ee', 'color': '#000'});","}"),
                                 searching = FALSE))
  })
 
  output$tabReumNum <- DT::renderDataTable({
    auxVar <- varSeleccionada()
    DT::datatable(auxVar$datSum, rownames = FALSE,
                  options = list(autoWidth = TRUE,
                                 initComplete = JS( "function(settings, json) {",
                                                    "$(this.api().table().header()).css({
                                                    'background-color': '#bdd7ee', 'color': '#000'});","}"),
                                 searching = FALSE)
    )
  })
  
  output$selVariables <- renderUI({
    getEstado(revisaFiltro = TRUE)
    output = tagList()
    if ("tipevars" %in% ls(.GlobalEnv) | !is.null(input$tipoDatos)){
      output[[1]] = selectInput("varsNum", "Variables Númericas", multiple = T, choices = tipevars$varcont, selected = tipevars$varcont)
      output[[2]] = selectInput("varsCat", "Variables Categóricas", multiple = T, choices = tipevars$varchar, selected = tipevars$varchar)
      output[[3]] = selectInput("vatsFec", "Variable Fecha", multiple = F, choices =  tipevars$varsdate, selected = tipevars$varsdate[1])
      output[[4]] = selectInput("varLat", "Latitud", multiple = F, choices = names(baseFin$df_data), selected = tipevars$varlat)
      output[[5]] = selectInput("varLon", "Longitud", multiple = F, choices = names(baseFin$df_data), selected = tipevars$varlon)
    } else {
      output[[1]] = renderPrint({"Falta seleccionar el archivo"})
    }
    output
  })

  # # Pestana Path
  observeEvent(c(input$varLat, input$varLon, input$selDPATH), {
    req(input$varLat, input$varLon, input$selDPATH, input$varIDTRACK)
    # # Agregar distancia y velocidad
    Dat <- copy(baseFin$df_data)
    colsPOIS <- c(input$varLat, input$varLon)
    exprDist <- parse(text = paste(input$selDPATH, "(Dat[, colsPOIS, with = FALSE],", 
                      " Dat[, colsPOIS, with = FALSE][, lapply(.SD, lag)])", sep=""))
    Dat[["Distance"]] <- eval(exprDist)

    # # replace in initial points by group
    Dat[,  id := 1:.N, by=NEW_ID_TRACK]
    Dat[id == 1, Distance := 0]

    # # calculate speed
    speed <- speedGPS(Dat[[input$vatsFec]], Dat$Distance[-1])
    speed <- c(speed, 0)
    Dat$Vel <- speed    
    baseFin$df_data <- Dat
  })

  observeEvent(c(input$tipoHorario), {
    req(input$vatsFec, !is.null(baseFin$df_data))
    baseFin$df_data$week_day <- wday(baseFin$df_data[[input$vatsFec]])#1: sunday an so on
    baseFin$df_data$month_day <- mday(baseFin$df_data[[input$vatsFec]])#1: first of month an so on
  })

  output$listaHorPath <- renderUI({
    req(input$tipoHorario, input$vatsFec)
    output <- tagList()
    output[[1]] <- selectizeInput("selHorarioPath", "Seleccione el horario para el filtro", 
                                  multiple = T, choices = vecTiempo, options = auxSel)
    if (input$tipoHorario == "daySemana") {
      vecOptDias <- unique(baseFin$df_data$week_day)
    } else {
      vecOptDias <- unique(baseFin$df_data$month_day)
    }
    output[[2]] <- selectizeInput("selDiaPath", "Seleccione el dia para el filtro", 
                                  multiple = T, choices = vecOptDias, options = auxSel)
    return(output)
  })

  output$distPath <- renderUI({
    selectInput("selDPATH", "Seleccione la distancia deseada del paquete geosphere", 
                multiple = F, choices = vecDist, selected = vecDist[3])
  })

  output$numPath <- renderUI({
    req(input$varsNum)
    selectizeInput("varsNumPath", "Filtro Variables Númericas", multiple = T, 
                   choices = c("Vel", "Distance", input$varsNum), options = c(auxSel, maxItems = 3))
  })

  output$selPath <- renderUI({
    req(!is.null(basePath$listIDs))
    selectizeInput("idSelPath", "Seleccione los ID_TRACK para filtrar", multiple = T, 
                   choices = basePath$listIDs, selected = basePath$listIDs[1:2])#options = auxSel)
  })

  output$selNumPath2 <- renderUI({
    req(input$varsNumPath)
    selectizeInput("varsNumPath2", "Selección de Variables a Analizar", multiple = F, 
                   choices = input$varsNumPath, options = auxSel)
  })

  output$slidernumPath <- renderUI({
    req(input$varsNumPath)
    output <- tagList()
    for(vv in 1:length(input$varsNumPath)){
      auxCol <- input$varsNumPath[vv]
      limSlide <- list(min = min(baseFin$df_data[[auxCol]], na.rm = T), 
                       max = max(baseFin$df_data[[auxCol]]), na.rm = T)
      output[[vv]] <- sliderInput(paste0("slideNumPath_", vv), 
                                  paste0("Seleccione el Rango (", auxCol, ")"), limSlide$min, limSlide$max, 
                                  value = c(limSlide$min, limSlide$max))
    }
    return(output)
  })

  observeEvent(input$filtrarPath, {
    req(input$varLat, input$varLon, input$varIDTRACK, input$selDPATH, 
        input$varsNumPath, input$selHorarioPath, input$selDiaPath)

    # # Asignando percentiles
    Dat <- baseFin$df_data
    Dat[["AuxFiltroHora"]] = getLimHorario(hour(Dat[[input$vatsFec]]), input$selHorarioPath)
    Dat <- data.table(filter(Dat, AuxFiltroHora == TRUE))

    # # Agrupacion con otras variables 
    IDTRIPS_stat <- Dat[, c('Dist' = sum(Distance, na.rm = TRUE), lapply(.SD, mean, na.rm = TRUE)), 
                        by = 'NEW_ID_TRACK', .SDcols = c("Vel", eval(input$varsNumRadar))]

    # # Filtros de las variables numericas
    for(vv in 1:length(input$varsNumPath)){
      auxCol <- input$varsNumPath[vv]
      vecFiltros <- eval(parse(text = paste0("input$slideNumPath_", vv)))
      indSel <- IDTRIPS_stat[[auxCol]] >= vecFiltros[1] & IDTRIPS_stat[[auxCol]] <= vecFiltros[2]
      IDTRIPS_stat <- IDTRIPS_stat[indSel]
    }

    # # Filtro de las otras variables / Guardando bases para filtrar
    basePath$df_map <- Dat[NEW_ID_TRACK %in% unique(IDTRIPS_stat$NEW_ID_TRACK)]
    if (input$tipoHorario == "daySemana") {
      basePath$df_map <- basePath$df_map[week_day %in% input$selDiaPath]
    } else {
      basePath$df_map <- basePath$df_map[month_day %in% input$selDiaPath]
    }
    basePath$listIDs = unique(basePath$df_map$NEW_ID_TRACK)
  })

  output$mapPath1 <- renderLeaflet({
    req(!is.null(basePath$df_map))
    Traj     <- unique(basePath$df_map$NEW_ID_TRACK)
    num_traj <- length(unique(basePath$df_map$NEW_ID_TRACK))
    
    
    my_palette <-  brewer.pal(num_traj, "Paired")
    pal <- colorFactor(my_palette, domain = unique(basePath$df_map$NEW_ID_TRACK))
  
    m <- leaflet(df) %>% addTiles()%>% addFullscreenControl(pseudoFullscreen = TRUE)
    for (i in unique(basePath$df_map$NEW_ID_TRACK)) {
      m <- m %>% 
        addPolylines(data = basePath$df_map[basePath$df_map$NEW_ID_TRACK == i, ], 
                     lng = ~get(input$varLon), lat = ~get(input$varLat), color= ~pal(NEW_ID_TRACK),
                     label=  ~(NEW_ID_TRACK))
      }
    m <-m %>% addLegend(pal = pal, values = Traj)
    m
  })

  observeEvent(c(input$varsNumPath2, input$idSelPath), {
    req(!is.null(basePath$df_map), input$varsNumPath2,  input$idSelPath)
    basePath$df_map2 <- basePath$df_map[NEW_ID_TRACK %in% input$idSelPath, ]
    quintile <- quantile(basePath$df_map2[[input$varsNumPath2]], probs = seq(0, 1, .2))
    basePath$df_map2$Category <- cut(basePath$df_map2[[input$varsNumPath2]], quintile, include.lowest = TRUE)
  })

  output$mapPath2 <- renderLeaflet({
    req(input$varsNumPath2, input$idSelPath)
    dom <- unique(basePath$df_map2$Category)
    my_palette <- brewer.pal(5, "Oranges")
    pal <- colorFactor(my_palette, domain = unique(basePath$df_map2$Category))
    m <- leaflet(df) %>% addTiles() %>% addFullscreenControl(pseudoFullscreen = TRUE)
    m <- m %>% addCircleMarkers(data = basePath$df_map2, 
                 lng = ~get(input$varLon), lat = ~get(input$varLat),  
                 color= ~pal(Category), weight = 1, fillOpacity = 1,
                 label = ~IDTRIP) %>%
    addLegend(pal = pal, values =dom)
    m
  })

  # # Pestana POIS
  output$distPOIS <- renderUI({
    selectInput("selDPOIS", "Seleccione la distancia deseada del paquete geosphere", 
                multiple = T, choices = vecDist, selected = vecDist[3])
  })

  output$listaHorarios <- renderUI({
    selectizeInput("selHorario", "Seleccione el horario para el filtro", 
                   multiple = T, choices = vecTiempo, options = auxSel)
  })

  output$listaCluster <- renderUI({
    req(input$nCluster)
    selectizeInput("selCluster", "Seleccione el número de cluster para filtrar", 
                   multiple = T, choices = 1:input$nCluster, options = auxSel)

  })


  output$mapPOIS <- renderLeaflet({
    req(input$selCluster)
    DatC <- filter(baseNet$df_map, Cluster == as.character(input$selCluster))
    leaflet(DatC) %>% addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl(pseudoFullscreen = TRUE)%>%
    addCircleMarkers(~get(input$varLon), ~get(input$varLat), 
                     radius = 0.02, color=unique(DatC$color))
  })

  observeEvent(c(input$selHorario, input$nCluster, input$selDPOIS), {
    req(input$selHorario, input$vatsFec, input$varLat, input$varLon, 
        input$nCluster, input$varIDTRACK, input$selDPOIS)

    # base para calculos
    Dat <- select(baseFin$df_data, c("NEW_ID_TRACK", input$vatsFec, input$varLat, input$varLon))
    colsPOIS <- c(input$varLat, input$varLon)
    exprDist <- parse(text = paste(input$selDPOIS, "(Dat[, colsPOIS, with = FALSE],", 
                      " Dat[, colsPOIS, with = FALSE][, lapply(.SD, lag)])", sep=""))
    Dat[["Distance"]] <- eval(exprDist)

    # # replace in initial points by group
    Dat <- data.table(Dat)
    Dat[,id := 1:.N, by=NEW_ID_TRACK]
    Dat[id == 1, Distance := 0]

    # # calculate speed
    speed <- speedGPS(Dat[[input$vatsFec]], Dat$Distance[-1])
    speed <- c(speed,0)
    Dat$Speed <- speed
    
    # # identificar lugares de baja velocidad, distancia entre esas regiones de baja velocidad para
    # pintar el grafo. Primero se divide en clusters, los que quiera ver el usuario
    set.seed(input$nCluster)
    clusters <- kmeans(Dat[,3:4], input$nCluster) # 20 clusters ( spacial solamente)
    
    # Save the cluster number in the dataset 
    Dat[,"Cluster"] <- as.factor(clusters$cluster)
    MCluster <- data.table(clusters$centers) # calculando distancia de cluster a cluste

    # # Asignando percentiles
    save(Dat, file = "DatAux.Rdata")
    Dat[["AuxFiltroHora"]] = getLimHorario(hour(Dat[[input$vatsFec]]), input$selHorario)
    Dat <- filter(Dat, AuxFiltroHora == TRUE)
    Clustesres <- Dat %>% group_by(`Cluster`) %>%
                   summarize(Vel  = mean(Speed , na.rm = TRUE),
                             trips  = length(unique(NEW_ID_TRACK)))
    quintile <-  quantile(Clustesres$Vel, probs = seq(0, 1, .2))
    Clustesres$group = cut(Clustesres$Vel,quintile, include.lowest = TRUE)

    # Assing the color to cut,
    Clustesres <-Clustesres %>%
    mutate(
      color = case_when(
        Clustesres$Vel >= quintile[[1]]  & Clustesres$Vel <= quintile[[2]] ~ "blue",
        Clustesres$Vel > quintile[[2]]  & Clustesres$Vel <= quintile[[3]] ~ "orange",
        Clustesres$Vel > quintile[[3]]  & Clustesres$Vel <= quintile[[4]] ~ "chocolate",
        Clustesres$Vel > quintile[[4]]  & Clustesres$Vel <= quintile[[5]] ~ "plum3",
        TRUE ~ "green"
      )
    )

    # # insumos para graficar Red
    Clustesres <- data.frame(data.table(Clustesres)[order(group)])
    MCluster[,"name"] <- rownames(MCluster)
    names(MCluster) <- c("lat", "lon", "name")

    # # insumos para graficar el mapa
    baseNet$df_map <- left_join(Dat, Clustesres, by="Cluster")
    
    # para que funcione la red se debe solo tener los mismos en ambos dataframes
    MCluster <- filter(MCluster, MCluster$name %in% Clustesres$Cluster)
    # # base con nodos Cluster
    distance.mat.m <- GeoDistanceInMetresMatrix(MCluster)
    distance.mat.m[upper.tri(distance.mat.m, diag=T)] <- 0 # quitar por encima de la diagonal
    distance.mat.m <- as.data.frame(as.table(distance.mat.m))
    
    # # grafo
    baseNet$df_link <- distance.mat.m
    names(baseNet$df_link) <- c("FromCluster", "ToCluster", "Distancia")
    baseNet$df_link <- filter(baseNet$df_link,Distancia!=0)
    
    # for js required 0 indexing
    baseNet$df_link <- data.frame(from = as.numeric(baseNet$df_link$FromCluster), 
                                  to = as.numeric(baseNet$df_link$ToCluster), 
                                  length=   baseNet$df_link$Distancia/1000,
                                  width= 0.1)
    
    baseNet$df_nodes <- data.frame(id = Clustesres$Cluster, label = paste("Cluster", Clustesres$Cluster),
                                   group = Clustesres$group,color=Clustesres$color,  value = Clustesres$trips,
                                   title = paste0("<p>", "Cluster:" ,Clustesres$Cluster, "<br>" ,"Trips:",Clustesres$trips, "</p>"), 
                                   stringsAsFactors = FALSE)
    baseNet$lnodes <- data.frame(label = unique(Clustesres$group),
                         shape = c( "square"), color = unique(Clustesres$color),
                         title ="Grupos_Velocidad")
    
    data.frame(Clustesres)
  })


  output$plot_network <- renderVisNetwork({
    req(input$selHorario, input$vatsFec,input$nCluster, input$varLat, input$varLon)
    visNetwork(baseNet$df_nodes, baseNet$df_link, height = "500px", width = "100%") %>% 
    visOptions(selectedBy = "group", 
               highlightNearest = TRUE, 
               nodesIdSelection = TRUE) %>%visLayout(randomSeed = 123) %>%
      visLegend( addNodes =baseNet$lnodes, useGroups = FALSE) 

    #my_color <- 'd3.scaleOrdinal() .domain(x) .range(["blue", "red", "yellow", "purple", "green"])'
    # forceNetwork(Links = baseNet$df_link, Nodes = baseNet$df_nodes,
    #              Source = "FromCluster", # ID of source node 
    #              Target = "ToCluster", # ID of target node
    #              Value = "Distancia", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
    #              NodeID = "Cluster", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
    #              Nodesize = "Vel",  # value from the node list (data frame) that contains value we want to use for a node size
    #              Group = "group",  # value from the node list (data frame) that contains value we want to use for node color
    #              height = 500, # Size of the plot (vertical)
    #              width = 1000,  # Size of the plot (horizontal)
    #              fontSize = 20, # Font size
    #              linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
    #              opacity = 0.85, # opacity
    #              zoom = FALSE, # ability to zoom when click on the node
    #              opacityNoHover = 0.1,
    #              legend=T,
    #              bounded = F,
    #              charge = -500)
    #              #colourScale =my_color)
  })


  # # Reaccion de los botones
  observeEvent(input$rmv, {
    if (file.exists(pathSave)) file.remove(pathSave)
    output$textStatus <- renderText({"O_O... Se eliminaron todos los filtros"})
    rdataFile = gsub("(.+)\\.(csv|txt|xlsx|xls)", 
                     paste0("\\1_", input$tipoDatos, ".Rdata"), input$file1$name)
    if (file.exists(rdataFile)) {
      load(rdataFile)
    } else {
      stop("No ha definido archivo de entrada")
    }
    assign("tipevars", tipevars, envir = .GlobalEnv)
    baseFin$df_data = datAux
    menu_vals$menu_list = NULL
    baseFiltros$df_filtros <- NULL
  })
  
  observeEvent(input$file1, { #Base <- reactive({
    rdataFile = gsub("(.+)\\.(csv|txt|xlsx|xls)", 
                     paste0("\\1_", input$tipoDatos, ".Rdata"), input$file1$name)
    if (file.exists(rdataFile)) {
      print("Estamos Cargando")
      load(rdataFile)
      baseFin$df_data = datAux
    } else {
      indCsv = grepl(".csv|txt",input$file1$datapath)
      indXLS = grepl(".xls|xlsx",input$file1$datapath)
      if (indCsv) {
        baseFin$df_data <- fread(input$file1$datapath) #encoding="UTF-8")
      } else {
        if(indXLS){
          baseFin$df_data <- read.xlsx(input$file1$datapath, header = TRUE,sheetIndex = 1,
                    stringsAsFactors = FALSE)
          #BaseF <- preparedata(Base)
        }
      }
      tipedata(baseFin$df_data)
      # # trabajo con variables tipo fecha
      for (col in tipevars$varsdate) {
       baseFin$df_data[[col]] <- parsedate::parse_date(baseFin$df_data[[col]])
      }
      datAux = baseFin$df_data
      save(datAux, tipevars,  file = rdataFile)
      
    }
    flagFile <<- TRUE
    assign("tipevars", tipevars, envir = .GlobalEnv)
  })

  observeEvent(input$filtrar, {
    # # Hacer nuevas pestañas
      menu_vals$menu_list <- list( menuItem(HTML("Multivariate <br/> Analisis"), tabName = "MulAn", icon = icon("area-chart")),
        menuItem("POIS", tabName = "afir", icon = icon("area-chart")),
        menuItem("CO-Ocurrence", tabName = "CoOc", icon = icon("area-chart")),
        menuItem("Path", tabName = "path", icon = icon("area-chart"))
      ) 

    # # Actualizando filtros
    if (file.exists(pathSave)){ 
      load(pathSave) # se carga filterVars
      for (var in filterVars) {
        if (var$auxTipo == "varchar") {
          baseFin$df_data <- baseFin$df_data[baseFin$df_data[[var$varFiltro]] %in% var$slideCat]
        } 
        if (var$auxTipo == "varcont") {
          indFiltro <- (baseFin$df_data[[var$varFiltro]] >= var$slideNum1)
          indFiltro <- indFiltro & (baseFin$df_data[[var$varFiltro]] <= var$slideNum2)
          baseFin$df_data <- baseFin$df_data[indFiltro]
        }
        if (var$auxTipo == "varsdate"){
          auxLI <- as.POSIXct(as.Date(var$slideDate1, origin = "1970-01-01"))
          auxLS <- as.POSIXct(as.Date(var$slideDate2, origin = "1970-01-01"))
          baseFin$df_data <- baseFin$df_data[(baseFin$df_data[[var$varFiltro]] >= auxLI) & (baseFin$df_data[[var$varFiltro]] <= auxLS)]
        }
      }
    }
    flagFiltro <<- TRUE
  }) 

  observeEvent(input$add, {
    auxVar <- varSeleccionada()
    lsAuxVar <- c(auxVar, slideDate = input$slideDate, slideNum = input$slideNum, 
                slideCat = list(input$slideCat))#, repreFecha = input$repreFecha)
    filterVars <- list()
    if (file.exists(pathSave))  load(pathSave)
    filterVars[[auxVar[["varFiltro"]]]] <- lsAuxVar
    save(filterVars, file = pathSave)
    baseFiltros$df_filtros <- filterVars
    output$textStatus <- renderText({paste0("..... Se guardo filtro para la variable (", auxVar$varFiltro,")")})
  })

  # # Coocurrencias
  output$varCoOc <- renderUI({
    output <- tagList()
    output[[1]] <- selectizeInput("varCoOc_Ori", "Variables Númericas Origen", multiple = T, choices = names(baseFinOD$df_data), options = auxSel)
    output[[2]] <- selectizeInput("varCoOc_Des", "Variables Númericas Destino", multiple = T, choices = names(baseFinOD$df_data), options = auxSel)
    return(output)
  })

  colLatLon <- reactive({
    if (input$tipoDatos == "Implicitos"){
      return(list(pOri = input$varLatLogOri, pDest = input$varLatLogDest))
    } else {
      selDestino <- paste0(c(input$varLat,  input$varLon), "_Destino")
      selOrigen  <- paste0(c(input$varLat,  input$varLon), "_Origen")
      return(list(pOri = selOrigen, pDest = selDestino))
    }
  })

  baseCoOc <- reactive({   
    if (input$tipoDatos == "Implicitos"){
      req(input$varLatLogOri, input$varLatLogDest, input$varCoOc_Ori, input$varCoOc_Des)
      indError <- (length(input$varLatLogOri) != 3) | (length(input$varLatLogDest) != 3) | (input$varNomOri == "") | (input$varNomDest == "")
      if(indError) stop("...Debe revisar la seleccion inicial en la pestana (View)") 
      selOrigen <- select(baseFinOD$df_data, c("NEW_ID_TRACK", input$varLatLogOri, input$varCoOc_Ori))
      setnames(selOrigen,  input$varLatLogOri, c("lat", "long", "Date"))
      selDestino <- select(baseFinOD$df_data, c("NEW_ID_TRACK", input$varLatLogDest, input$varCoOc_Des)) 
      setnames(selDestino,  input$varLatLogDest, c("lat", "long", "Date"))
    } else {
      req(input$varOD, input$vatsFec, input$varLat, input$varLon, input$varCoOc_Ori, input$varCoOc_Des)
      selDestino <- paste0(c(input$varLat,  input$varLon, input$vatsFec), "_Destino")
      selOrigen  <- paste0(c(input$varLat,  input$varLon, input$vatsFec), "_Origen")
      selOrigen  <- select(baseFinOD$df_data, c("NEW_ID_TRACK", selOrigen, input$varCoOc_Ori))  
      selDestino <- select(baseFinOD$df_data, c("NEW_ID_TRACK", selDestino, input$varCoOc_Des))       
      
      # # Ajustando Nombres
      names(selOrigen) <- gsub("_Origen", "", names(selOrigen))
      names(selDestino) <- gsub("_Destino", "", names(selDestino))
      setnames(selOrigen,  c(eval(input$varLat),  eval(input$varLon), input$vatsFec), c("lat", "long", "Date"))
      setnames(selDestino, c(eval(input$varLat),  eval(input$varLon), input$vatsFec), c("lat", "long", "Date"))
    }
    bind_rows(list(selOrigen, selDestino))
  })

  observeEvent(input$menuAPP, {
    if(input$menuAPP == "CoOc"){
      shared_CoOc <<- SharedData$new(baseCoOc)  
    }
  })

  output$mapCoOc <- renderLeaflet({
    leaflet(shared_CoOc) %>% addTiles() %>% addCircleMarkers(radius = 0.02)
  })

  output$selPolarCoOc <- renderUI({
    output <- tagList()
    dfMap  <- shared_CoOc$data(withSelection = TRUE) %>%
              filter(selected_ | is.na(selected_)) %>%
              mutate(selected_ = NULL)
    output[[1]] <- selectInput("repreFechaCoOc", "Seleccione el formato para esta fecha", 
                               multiple = F, choices = vecFecha, selected = vecFecha)
    nMaxAux <- length(unique(dfMap[["NEW_ID_TRACK"]]))
    output[[2]] <- numericInput("nGrupos", "Número de grupos:", 3, min = 1, max = nMaxAux - 1)
    output
  })

  output$infoMapCoOC <- DT::renderDataTable({
    DT::datatable(shared_CoOc, extensions="Scroller", style="bootstrap", class="compact", width="100%",
                  options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
  }, server=FALSE)

  output$plotPolar <- renderPlotly({
    colLatLon <- colLatLon()
    dfMap <- shared_CoOc$data(withSelection = TRUE) %>%
      filter(selected_ | is.na(selected_)) %>%
      mutate(selected_ = NULL)
    

    # # Calculos de puntos de origen
    idTrack <- unique(dfMap[["NEW_ID_TRACK"]])
    Basep <- baseFinOD$df_data[baseFinOD$df_data$NEW_ID_TRACK %in% idTrack, ][, .SD[1], by = "NEW_ID_TRACK"]

    # # Grupos por punto de inicio
    clusters <- kmeans(Basep[, .SD, .SDcols = colLatLon$pOri[1:2]], input$nGrupos)
    Basep[["Cluster"]] <- as.factor(clusters$cluster)

    # Cambios en fecha
    #if (input$repreFechaCoOc == "Año (%Y)") dfMap[[input$vatsFec]] <- year(dfMap[[input$vatsFec]]); auxLen = 24
    dfMap[["Date"]] <- granularizaFecha(dfMap[["Date"]], input$repreFechaCoOc)
    auxLen <- lenGranFecha(input$repreFechaCoOc)
    # # Agregacion de base inicial
    IDTT <- dfMap[!duplicated(dfMap$NEW_ID_TRACK, dfMap[["Date"]]),]
    IDTT <- left_join(IDTT, select(Basep, "NEW_ID_TRACK", "Cluster"), by="NEW_ID_TRACK")
    dataResum <- data.table(IDTT)[, .(Countrip = .N), by = c("Cluster", "Date")] 

    # # Grafico polar 
    tickvals0 = seq(0, 345, ceiling(345 / auxLen))
    ticktext = seq(1:auxLen)
    horas <- cbind(tickvals0, Date = ticktext)

    dataResum <- merge(dataResum, horas, by = "Date")  
    pal <- c("red", "blue", "green")
    p <- plot_ly(data=dataResum,
      type = 'scatterpolar',
      r = ~Countrip,
      theta = ~tickvals0,
      size = ~Countrip,
      sizes = c(100, 300),
      mode = 'markers',
      color=~dataResum$Cluster,
      opacity = 1
    ) %>%
      layout(
        showlegend = FALSE,
        polar = list(
          angularaxis = list(
            showticklabels = TRUE,
            # remove grid lines and ticks
            showgrid = TRUE,
            ticks = '',
            # if you want the axis to go the other way around
            direction = 'clockwise',
            tickmode="array",
            tickvals = tickvals0,
            ticktext = ticktext
          ),
          radialaxis = list(
            tickvals = tickvals0,
            ticktext = ticktext
            #ticktext = c('', "One", "Two", "Three", "Four", "Five", "Six", "Seven")
          )
        )
      )
    ggplotly(p)


  })
  
  ##############################################################################
  #             Graphs and tables for selection of items                       #
  ##############################################################################
  
  
  # Table with filter items ---------------------------------------
  
  output$Table1 <- DT::renderDataTable({
   getEstado(revisaFiltro = FALSE)
   Datos1 <- baseFin$df_data[1:100,]
      },options = list(scrolly = TRUE))

  tipo <- reactive({
    tip <- if_else(isOD(BaseFilt())==1,"This data is implicit","This data is explicit")
    #tip <-"This data is implicit"
  })
  
  output$caption <- renderText({
    tipo()
  })
  
  # render leaflet
  
  output$map <- renderLeaflet({
    
    #Leer datos------------------------------------------------------------------------
    
    
    dataResum <- read.delim("C:/Users/RIA/Documents/Tesis2/Aplicativo/AplicativoPres/AplicativoPres/data/dataResum.txt", header=T, sep="\t", dec=".")
    dataResumClus <- read.delim("C:/Users/RIA/Documents/Tesis2/Aplicativo/AplicativoPres/AplicativoPres/data/dataResumClus.txt", header=T, sep="\t", dec=".")
    
    #dataResum <-read.delim("C:/Users/LENOVO/Documents/ANDES/Tesis1/Aplicativo/Preprocessing/output/aggregate_uber_trip_data.txt", sep="\t", dec=".")
    #dataResum["Month"] <- as.character(dataResum[,"Month"])
    #dataResum["Day"] <- as.character(dataResum[,"Day"])
    
    mesIN <- input$Mes
    Ruta <-input$Ruta
    DiaIn <-input$dia

    #mesIN <- 5
    #Ruta <-"MP03R4"
    #DiaIn <-"9"
    
    
    
    #Agregar  datos------------------------------------------------------------------------
    
    dataFilter <- filter(dataResum,   Day_seg_fecha== DiaIn & Month_seg_fecha==mesIN & Codigo.Ruta==Ruta)
    dataTodo <- dataResumClus
    
    
    #Filtra datos------------------------------------------------------------------------
    
    if(mesIN =="TODOS" & Ruta == "TODAS" & DiaIn=="TODOS"){
      dataB <- dataTodo
     }else {
      datab <- dataFilter
    }
    
    
    #Asigna Colores-----------------------------------------   
  
    colorB="blue"
    #Genera grafico------------------------------------------------------------------------
    
      maxi<- max(dataB$meanVel)
      leaflet(data = dataB) %>% addTiles() %>%
        addFullscreenControl(pseudoFullscreen = TRUE)%>%
        addCircleMarkers(~ seg_longit, ~seg_latitu, popup = ~as.character(meanVel),
                         label = ~as.character(meanVel),
                         weight = 1,
                         radius = (dataB$meanVel/maxi) * 20,
                         color = colorB,
                         stroke = TRUE, fillOpacity = 0.6)%>% setView(-77.2811100, 1.2136100, zoom = 18)
   
  })
  
  
  
  #Times series and correlation
  
  
  # Plot time series chart 
  output$timeseries <- renderPlotly({
    
    Variables1 <- fread("data/Variables1.txt")
    Variables2 <- fread("data/Variables2.txt")
    
    
    Variables2$seg_veloci <- round(Variables2$seg_veloci,1)
    Variables1$seg_veloci <- round(Variables1$seg_veloci,1)
    
    # Create dates
    Variables1$Fecha.x <- parsedate::parse_date(Variables1$Fecha.x)
    Variables2$Fecha.x <- parsedate::parse_date(Variables2$Fecha.x)
    
    
    #filter Ruta
    Variables2 <- filter(Variables2,`Codigo Ruta`==input$Ruta )
    
   
    # continuous vars
    
    varsc <-c("Fecha.x","seg_veloci", "Galones",  "Toneladas_Recogidas", 
              "Gal_Hora",  "Ton_Km",  "Nro_Compactaciones",
              "Km_Recoleccion", "Km_Viaje_vacio", "Km_Regreso",
              "Km_Descargue")
    Variables2 <- subset(Variables2, select=varsc)
    
    varp <- input$Variable
    
    
    # Reshape
    ds <- reshape2::melt(Variables2, id = "Fecha.x")
    ds <- filter(ds, (variable %in% varp))
    
    # Set some colors
    plotcolor <- "#F5F1DA"
    papercolor <- "#E3DFC8"
    
    
    
    
    p <- plot_ly(source = "source") %>% 
      add_lines(data = ds, x = ~Fecha.x, y = ~value, color = ~variable, mode = "lines", line = list(width = 3))
    
    # Add SP500
    p <- p %>%
      layout(title = "Time series of Datos",
             xaxis = list(title = "Fecha", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
             yaxis = list(title = input$Variable, gridcolor = "#bfbfbf"), 
             plot_bgcolor = plotcolor,
             paper_bgcolor = papercolor, 
             yaxis2 = list(title = "SP500", side = "right", overlaying = "y"))
    p
  })
  
  
  output$correlation <- renderPlotly({
    
    # Read in hover data
   eventdata <- event_data("plotly_hover", source = "source")
   validate(need(!is.null(eventdata), "Hover over the time series chart to populate this heatmap"))
    #
    ## Get point number
    datapoint <- as.numeric(eventdata$pointNumber)[1]
    
    # Get window length
    window <- as.numeric(input$window)
    
    # Show correlation heatmap
    rng <- (datapoint - window):(datapoint + window)
    cormat <- round(cor(Variables2[rng, 2:11]),2)
    
    p <-plot_ly(x = rownames(cormat), y = colnames(cormat), z = cormat, type = "heatmap", 
            colors = colorRamp(c('#e3dfc8', '#808c6c')))%>% 
      layout(title = "Correlation heatmap",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    p
    
  })
  
  
 # output$distPlot <- renderChorddiag({
  #  dat <- input$matriz
   # df <- read.delim(dat$datapath, header=T, sep="\t", dec=".")
    #df = as.matrix(df)
    #row.names(df) = c(colnames(df))
    #df
    
    #chorddiag(df, showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
    
  #}
  #)
  
  
  
  
  
  # Table with counts form hist ---------------------------------------
  
}



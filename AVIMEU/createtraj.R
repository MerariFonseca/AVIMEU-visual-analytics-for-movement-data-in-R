library(data.table)
library(leaflet)
library(dplyr)
library(parsedate)
library(rgdal)
#BasePrueba <-fread("BasePrueba.txt")
#BasePrueba <- filter(BasePrueba, seg_latitu != 0)
#Base1 <- filter(BasePrueba,Fecha.x == "2019-05-04" & veh_codigo.y=="SVR 145")
#write.table(BasePrueba, "BasePrueba.txt", sep="\t", quote=F, row.names = F, dec=".")

createtraj <- function(Base,lat, lon, colNum){
  require(data.table)
  Base[, by = eval(colNum)]
  trak <- select(Base, c(lat,lon, colNum))
  names(trak) <- c("lat","lon", colNum)
  lats <-mean(trak$lat, na.rm = TRUE)
  lons <-mean(trak$lon, na.rm = TRUE)
  
  #qpal <- colorQuantile("Blues", Base[[colNum]], n = 6)
  map <- leaflet() %>% addTiles()%>%addFullscreenControl(pseudoFullscreen = TRUE)%>%
    setView(lat = lats , lng = lons, zoom=13)

    map <- addPolylines(map, lng=~lon,lat=~lat,data=trak, #color =~qpal(Base[[colNum]]), 
                      weight = 3)
  assign("traj", map, environment<-.GlobalEnv)
}


Names_point <- function(Base,latstart, lonstart, latend, lonend, ori_name,destin_name)
{
  trak <- select(Base, c(latstart,lonstart,latend,lonend,  ori_name,destin_name))
  print(str(trak))
  names(trak) <- c("latst","lonst","laten","lonen","oriname", "destname")
  
  Namespuntosor <- trak %>%
    group_by(oriname) %>%
    summarize(lat = mean(latst, na.rm = TRUE),
              lon = mean(lonst, na.rm = TRUE))
  names(Namespuntosor) <- c("Name", "lat", "lon")
  
  
  Namespuntosdes <- trak %>%
    group_by(destname) %>%
    summarize(lat = mean(laten, na.rm = TRUE),
              lon = mean(lonen, na.rm = TRUE))
  names(Namespuntosdes) <- c("Name", "lat", "lon")
  
  Namespoint <-unique(rbind(Namespuntosor,Namespuntosdes))
  assign("namespoint", Namespoint, environment<-.GlobalEnv)
  
}

my_icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
)

# # function for create traj od
createtrajOD <- function(Base,latstart, lonstart, latend, lonend, ori_name, destin_name, colNum){
  require(data.table)
  #Base[, by = eval(colNum)]
  trak <- select(Base, c(latstart,lonstart,latend,lonend,  ori_name,destin_name, colNum))
  names(trak) <- c("latst","lonst","laten","lonen","oriname", "destname", colNum)
  
  # # generate labels for ori dest lines
  line_labels<- sprintf(
    "<strong>Origin:</strong> %s<br/> <strong>Destination:</strong> %s<br /> <strong>Distance:</strong> %g miles</sup>",
    trak$oriname, trak$destname, round(trak[[colNum]],2)
  )%>% lapply(htmltools::HTML)
  
  #qpal <- colorQuantile("Blues", Base[[colNum]], n = 6)
  map<-leaflet(data=trak)%>%addTiles%>%addFullscreenControl(pseudoFullscreen = TRUE)%>%
    addMarkers(data = namespoint, label =~Name, 
               icon = my_icons,
               labelOptions = labelOptions(noHide = F, direction = 'auto'),
               options = markerOptions(riseOnHover = TRUE)) 
  for (i in 1:nrow(trak)) 
    map<-map%>%addPolylines(lat=c(trak[i,]$latst,trak[i,]$laten),lng=c(trak[i,]$lonst,trak[i,]$lonen),
                            label = line_labels, weight = 2)
  
  assign("traj", map, environment<-.GlobalEnv)
}




  
#createtraj(Base1, "seg_latitu","seg_longit", "seg_fecha")

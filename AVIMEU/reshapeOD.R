#####
# rehape  data base  origen-destino for graph 


#BaseFil <- react
library(dplyr)
library(data.table)



reshapeOD <- function(Base,startlat,startlon,starttime,endlat,endlon,endtime){


baseStart <- Base

baseStart[,"lat"] <-  baseStart[,startlat]
baseStart[,"lon"] <-  baseStart[,startlon]
baseStart[,"time"] <-  baseStart[,starttime]

baseend <- Base
baseend[,"lat"] <-  baseend[,endlat]
baseend[,"lon"] <-  baseend[,endlon]
baseend[,"time"] <-  baseend[endtime]

baseF= rbind(baseStart,baseend)
return(baseF)

}



#cosa <- cosa %>%filter(Bike.ID== "4727") 
#cosa <- preparedata(cosa)
#  cosa <- cosa %>%filter(time_year== 2017) & time_month == 3 & time_Day ==2 ) 


#library(leaflet)
#library(magrittr)

#leaflet()%>%
#  addTiles() %>%
#  addPolylines(data = datos, lng = ~lon, lat = ~lat, group = ~Trip.ID)


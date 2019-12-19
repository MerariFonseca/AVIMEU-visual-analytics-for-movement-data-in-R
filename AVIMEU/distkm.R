
library(geosphere)
 # Function for calculate  distance between two points in database



# distancia <- "distGeo"

distkm <- function(distancia,BASE){
  
  vars <- names(BASE)
  varstartlat <-(vars[grep("(?=.*(start|Start|START|INICIO|inicio|Inicio|Inicial|INICIAL|Inicial|inicial|origen|ORIGEN|Origen|Origin|ORIGIN|origin))(?=.*(latit|LATIT|Latit))",vars, perl=T)])
  varstartlon <-(vars[grep("(?=.*(start|Start|START|INICIO|inicio|Inicio|Inicial|INICIAL|Inicial|inicial|origen|ORIGEN|Origen|Origin|ORIGIN|origin))(?=.*(longit|Longit|LOGIT))",vars, perl=T)])
  varsstart <- c(varstartlon,varstartlat)
  
  varendtlat <-(vars[grep("(?=.*(END|end|End|FIN|fin|Fin|Destino|DESTINO|destino|destination|DESTINATION|Destination))(?=.*(latit|LATIT|Latit))",vars, perl=T)])
  varendlon <-(vars[grep("(?=.*(END|end|End|FIN|fin|Fin|Destino|DESTINO|destino|destination|DESTINATION|Destination))(?=.*(longit|Longit|LOGIT))",vars, perl=T)])
  varsend <- c(varendlon,varendtlat)
  
  distakm <- if(distancia=="distGeo"){
    distGeo(BASE[,.SD,.SDcols=varsstart], 
            BASE[,.SD,.SDcols=varsend])/1000
    
    }else{
      if(distancia =="distHaversine"){
        distHaversine(BASE[,.SD,.SDcols=varsstart], 
                      BASE[,.SD,.SDcols=varsend])/1000
    
            }else{
              if(distancia=="distCosine"){
                distCosine(BASE[,.SD,.SDcols=varsstart], 
                           BASE[,.SD,.SDcols=varsend])/1000
                
                }else{
                  if(distancia=="distVincentySphere"){
                    distVincentySphere(BASE[,.SD,.SDcols=varsstart], 
                                       BASE[,.SD,.SDcols=varsend])/1000
                    }
                }
            }
    }
  return(distakm)
  
  }








#ntrip <- filter(DATABASE, DATABASE$`Bike ID`=="5944")

#preparedata(DATABASE)

#head(DATABASE)

#subconj <- filter(DATABASE, DATABASE$`Bike ID`=="5944")

#flow_dests, flowlines, flow, route_network, routes_fast, routes_slow
#data(flow_dests)

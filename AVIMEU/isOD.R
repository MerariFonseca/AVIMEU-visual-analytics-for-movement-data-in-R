

# # #################################
#  Fuction for detect if database is contain origin destination

isOD <- function(DATABASE){
  
  vars <-names(DATABASE)
  varstarttime <-(vars[grep("(?=.*(start|Start|START|INICIO|inicio|Inicio|Inicial|INICIAL|Inicial|inicial|origen|ORIGEN|Origen|Origin|ORIGIN|origin))(?=.*(Time|time|TIME|DATE|date|Date|Fecha|fecha|FECHA|Hora|HORA|hora|Hour|HOUR|hour))",vars, perl=T)])
  varstartlat <-(vars[grep("(?=.*(start|Start|START|INICIO|inicio|Inicio|Inicial|INICIAL|Inicial|inicial|origen|ORIGEN|Origen|Origin|ORIGIN|origin))(?=.*(latit|LATIT|Latit))",vars, perl=T)])
  varstartlon <-(vars[grep("(?=.*(start|Start|START|INICIO|inicio|Inicio|Inicial|INICIAL|Inicial|inicial|origen|ORIGEN|Origen|Origin|ORIGIN|origin))(?=.*(longit|Longit|LOGIT))",vars, perl=T)])
  
  varendttime <-(vars[grep("(?=.*(END|end|End|FIN|fin|Fin|Destino|DESTINO|destino|destination|DESTINATION|Destination))(?=.*(Time|time|TIME|DATE|date|Date|Fecha|fecha|FECHA|Hora|HORA|hora|Hour|HOUR|hour))",vars, perl=T)])
  varendtlat <-(vars[grep("(?=.*(END|end|End|FIN|fin|Fin|Destino|DESTINO|destino|destination|DESTINATION|Destination))(?=.*(latit|LATIT|Latit))",vars, perl=T)])
  varendlon <-(vars[grep("(?=.*(END|end|End|FIN|fin|Fin|Destino|DESTINO|destino|destination|DESTINATION|Destination))(?=.*(longit|Longit|LOGIT))",vars, perl=T)])
  
  
  Ordest <-  if_else(length(c(varstarttime,varstartlat,varstartlon,varendttime,varendtlat,varendlon))==6, 1,2)
  return(Ordest)
}
 
#library(data.table)
# data1 = fread("E:/Documentos/Tesis2/Aplicativo511Veolia/AplicativoPres/BasePrueba.txt")

 # # mapa para pintar puntos de origenes

dat <- fread("C:/Users/usuario/Documents/BasePrueba4.txt")


library(dplyr)
library(leaflet)




library(leaflet)
df1 = data.frame(lat = c(1.276981,1.216106,1.200913), lng =c(-77.26114,-77.27351,-77.27958), cluster=c(3,1,2), peso=c(1,10,3))
df2 = data.frame(color = c("blue", "#218E80","orange"), domain=c(1,2,3))
leaflet(df1) %>% addTiles()%>% addCircleMarkers(color = df2$color,
                                  labelOptions = labelOptions(noHide = F, direction = 'auto'),
                                  options = markerOptions(riseOnHover = TRUE),
                                  label =~cluster, weight = ~peso )


c(1.276981,1.216106,1.200913)
c(-77.26114,-77.27351,-77.27958)
c(1,3,2),


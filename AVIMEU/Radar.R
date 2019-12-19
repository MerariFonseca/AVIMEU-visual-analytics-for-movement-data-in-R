library(plotly)

#Base2 <- select(Base1, c("Codigo Ruta","DIF_HORA_INI","DIF_HORA_FIN",
                         #"DIF_TempoPlan"))




#Base2 <-
# Base2 %>%
#  group_by(`Codigo Ruta`) %>%
#  summarize(DIF_HORA_INI = mean(DIF_HORA_INI, na.rm = TRUE),
#            DIF_HORA_FIN = mean(DIF_HORA_FIN, na.rm = TRUE),
#            DIF_TempoPlan = mean(DIF_TempoPlan, na.rm = TRUE))


radar <- function(Base2, rango){
  p <- plot_ly(
  type = 'scatterpolar',
  mode='lines'
  ) 
  for(i in seq(1:dim(Base2)[1])){
    p= add_trace(p,
    r = unname(unlist(Base2[i,2:dim(Base2)[2]])),
    theta = names(Base2)[2:dim(Base2)[2]],
    name = Base2[i,1],
    fill="toself"
  )%>%
      layout(
        title = "Mean of interest vars",
        polar = list(
          radialaxis = list(
            visible = T,range = rango)
          )
      )
  }
  assign("radarplot", p, environment<-.GlobalEnv)
  
}


#radar(Base2,c(-5,5))


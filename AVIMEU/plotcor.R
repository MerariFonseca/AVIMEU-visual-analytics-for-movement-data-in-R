library(parcoords)
#Base1 <- filter(BasePrueba, veh_codigo.y=="SVR 145")


# informaci?nn a nivel de ruta , dia, vehiculo y conductor
#Base2 <- select(Base1, c("Codigo Ruta","DIF_HORA_INI","DIF_HORA_FIN",
#                         "DIF_TempoPlan","Velocidad Total"))



#Base2 <-
#  Base2 %>%
#  group_by(`Codigo Ruta`) %>%
#  summarize(DIF_HORA_INI = mean(DIF_HORA_INI, na.rm = TRUE),
#            DIF_HORA_FIN = mean(DIF_HORA_FIN, na.rm = TRUE),
#            DIF_TempoPlan = mean(DIF_TempoPlan, na.rm = TRUE),
#            Velocidad_Total = mean(`Velocidad Total`, na.rm = TRUE))


# # function for plot

plotcor <- function(DATABASE,category){
 coord <- parcoords(
  DATABASE
  ,rownames = FALSE
  ,brushMode = "1d-axes"
  ,reorderable = TRUE
  ,queue = TRUE,
   #,color= list(
   # colorBy= category, colorScheme = "schemeCategory10"
   # ),
  withD3 = TRUE
)
 assign("plotcoor", coord, environment<-.GlobalEnv)
}
#plotcor(Base2, "Codigo Ruta")





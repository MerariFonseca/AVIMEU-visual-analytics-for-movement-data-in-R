 # # Function for plot OD Matrix. 
library(corrplot)
library(plotly)
library(data.table)
library(dplyr)

#Base <-fread("C:/Users/jota9/OneDrive/Documentos/2019/Aplicativo511Veolia-20191106T021846Z-001/Aplicativo511Veolia/AplicativoPres/DatosOD/Metro_Bike_Share_Trip_Data.csv", colClasses = "character")
#Base[,"Fecha"] <- substring(Base$`Start Time`,1,10)
#Base           <- subset(Base, Fecha =="08/10/2016")

ODMatrix <- function(Base1, vecXY, VARIABLE){
  
  plot_ly(Base1,
        x = ~get(vecXY[1]),
        y = ~get(vecXY[2]),
        size = ~get(VARIABLE),
        type = 'scatter',
        mode = 'markers',
        marker = list(sizemode = ~get(VARIABLE), symbol="square"),
        text = ~paste('Valor', get(VARIABLE))
        )%>%
  layout(title = "OD Matrix for Trips", xaxis = list(title = vecXY[1]), 
         yaxis = list(vecXY[2]))
}







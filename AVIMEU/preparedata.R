# # #################################
#  Fuction for Prepare data extract and dectec date vars

Sys.setlocale("LC_ALL","English") # for format, changue

preparedata <- function(DATABASE){

library(dplyr)
library(dataPreparation)
library(data.table)
library(parsedate)


### Data for proof
#datapath1 <- "C:/Users/RIA/Documents/Tesis2/Aplicativo/Metro_Bike_Share_Trip_Data.csv"
#datapath2 <- "C:/Users/RIA/Documents/Tesis2/Aplicativo/MultimodalTraffic.txt"


# # detect vars of date
datesf<-identifyDates(DATABASE, cols = "auto", formats = "%m/%d/%Y %I:%M:%S %p", n_test = 30,
                        ambiguities = "IGNORE", verbose = TRUE)#intentar con SOLVE
  
  
varsdate <-names(datesf[grep("%Y",datesf)])
  
  
setDT(DATABASE)[, (varsdate) := lapply(.SD, parse_date), .SDcols = varsdate]
  

# trasnform var detected like date


DATABASE[, (paste0("Year_",varsdate, sep = "")) := lapply(.SD, year), .SDcols = varsdate]
DATABASE[, (paste0("Month_",varsdate, sep = "")) := lapply(.SD, month), .SDcols = varsdate]
DATABASE[, (paste0("Day_",varsdate, sep = "")) := lapply(.SD, day), .SDcols = varsdate]
DATABASE[, (paste0("Hour",varsdate, sep = "")) := lapply(.SD, hour), .SDcols = varsdate]
DATABASE[, (paste0("wday",varsdate, sep = "")) := lapply(.SD, wday), .SDcols = varsdate]



return(DATABASE)

}







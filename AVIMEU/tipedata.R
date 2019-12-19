
# # #################################
#  Fuction for detect lat, long, dates, categorical and continuous vars


library(dplyr)
library(dataPreparation)
library(data.table)
library(parsedate)

rSumm <- function(x, byVars = NULL, yVars, nRound = 0, flagDate = FALSE){
  if (length(yVars) == 0) return(NULL)
  x <- x[, .SD, .SDcols = c(byVars, yVars)]
  x <- melt(x, id.vars = c(byVars), measure.vars = yVars)
  if (flagDate){
    #x[, value := parsedate::parse_date(value)]
    return(x[,.(N=.N,
                Minimo= min(value, na.rm=TRUE),
                Maximo= max(value, na.rm=TRUE)), 
             by = c(byVars, "variable")])
  }
  return(x[,.(N=.N,
              Promedio= round(mean(value, na.rm=TRUE), nRound),
              Desviacion= round(sd(value, na.rm=TRUE), nRound),
              Minimo= round(min(value, na.rm=TRUE), nRound),
              Per25= round(quantile(value, .25, na.rm=TRUE), nRound),
              Per50= round(quantile(value, .50, na.rm=TRUE), nRound),
              Per75= round(quantile(value, .75, na.rm=TRUE), nRound),
              Maximo= round(max(value, na.rm=TRUE), nRound)), 
           by = c(byVars, "variable")])
}

tipedata <- function(DATABASE){
  
  vars <-names(DATABASE)
  # # detect vars lat lon
  varlat <-(vars[grep("(?=.*(latit|LATIT|Latit))",vars, perl=T)])
  varlon <-(vars[grep("(?=.*(longit|Longit|LONGIT))",vars, perl=T)])

  # # detect vars of date
  datesf<-identifyDates(DATABASE, cols = "auto", formats = "%m/%d/%Y %I:%M:%S %p", n_test = 30,
                        ambiguities = "IGNORE", verbose = TRUE)#intentar con SOLVE
  
  
  varsdate <-names(datesf[grep("%Y",datesf)])
  
  # # detect categorical vars ( not in datevar)
  
  
  varchar<- DATABASE[,sapply(DATABASE,class) == 'character']
  varchar<-names(varchar[varchar=="TRUE"])
  varchar <-varchar[!varchar %in% c(varsdate, varlat,varlon)]
  
  #  # numerical rest
 
  varcont <- vars[!vars %in% c(varchar,varsdate,varlat,varlon)]
  
  tipevars <-list(varchar,varsdate,varlat,varlon,varcont)
  names(tipevars) <- c("varchar","varsdate","varlat","varlon","varcont")
  assign("tipevars",tipevars,envir = .GlobalEnv)
}
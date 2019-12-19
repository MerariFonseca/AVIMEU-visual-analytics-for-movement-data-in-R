# # function for select vars except lat and lon

varslatlon <- function(B){
  
  vars <-names(B)
  varlat <-(vars[grep("(?=.*(latit|LATIT|Latit))",vars, perl=T)])
  varlon <-(vars[grep("(?=.*(longit|Longit|LOGIT))",vars, perl=T)])
  
  varsdel <-  c(varlat,varlon)
  return(varsdel)
}
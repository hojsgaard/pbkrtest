readExcel <- function (file, sheet = "Ark1") 
{
  require(RODBC)
  sheet2 <- paste('"',sheet,'$\"',sep='')
  str <- paste("select * from", sheet2)
  channel <- odbcConnectExcel(file)
  tables <- sqlTables(channel)
  d <- sqlQuery(channel, str)
  odbcClose(channel)
  return(d)
}

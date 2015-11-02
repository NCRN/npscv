#' @include filterpaste.R
#' 
#' @title cvparks
#' 
#' @description Returns the name, park code and other data for I&M parks with data available through the NPS common view
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @param fetch  Indicates what you wish to retrive from the common view data service. Options are:
#' \describe{
#' \item{"code"}{The default. Returns the four letter code of the park}
#' \item{"name"}{Returns the full name of the park}
#' \item{"full"}{Returns the Park code, Park name, the network code and the network name.}
#' \item{"all"}{Returns all data from the "Park" table of the common view as well as the network name and code.}
#' }
#' @param network  A network four letter code in quotes. Only data on parks in that network will be returned.
#' @param data Indicates the type of data you are interesed in. Currently has no function as only water monitoring data can be retrieved.
#' @return Eithter a \code{vector} with park codes or names, or a \code{data.frame} with all network data.
#'  
#' @export



cvparks<-function(fetch="code",network=NA, data="water"){  
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Parks?$expand=Network"
  filtnames<-c("Network/NetworkCode")[!is.na(network)]
  filtvalues<-c(network)[!is.na(network)]
  optfilter<-filterpaste(punct="&",names=filtnames,values=filtvalues)
  optfetch<-switch(fetch,
                   code="&$select=ParkCode",
                   name="&$select=ParkName",
                   full="&$select=ParkCode,ParkName,Network/NetworkCode,Network/NetworkName",
                   all=""
  )
  DataOut<-fromJSON(url(paste0(base,optfilter,optfetch)),flatten=TRUE)[[2]]
  names(DataOut)[names(DataOut)=="Network.NetworkCode"]<-"NetworkCode"
  names(DataOut)[names(DataOut)=="Network.NetworkName"]<-"NetworkName"
  return(DataOut)
}

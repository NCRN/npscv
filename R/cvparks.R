#' @title cvparks
#' 
#' @description Returns the name and code of I&M parks with data as avaialable through the NPS common view
#' 
#' @importFrom jsonlite fromJSON
#' 
#'  @param fetch  Indicates what you wish to retrive from the common view data service. Options are:
#'  \describe{
#'  \item{"code"}{The default. Returns the four letter code of the park}
#'  \item{"name"}{Returns the full name of the park}
#'  \item{"all"}{Returns all data from the "parks" table of the common view as well as the nework name and code.}
#'  }
#'  @param network  A network four letter code in quotes. Only data on parks in that network will be returned.
#'  @param data Indicates the type of data you are interesed in. Currently has no function as only water monitoring data can be retrieved.
#'  @return Eithter a \code{vector} with park codes or names, or a \code{data.frame} with all network data.
#'  
#'  @export
#'  



cvparks<-function(fetch="code",network=NA, data="water"){  
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Parks?$expand=Network"
  optnetwork<-ifelse(is.na(network), "", paste0("&$filter=Network/NetworkCode eq '",network,"'"))
  optfetch<-switch(fetch,
                   code="&$select=ParkCode",
                   name="&$select=ParkName",
                   all=""
  )
  
  fromJSON(url(paste0(base,optnetwork,optfetch)))[[2]]
  
}

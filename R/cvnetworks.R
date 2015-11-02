#' @title cvnetworks
#' 
#' @description Returns the name and code of I&M netowrks with data as avaialable through the NPS common view
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @param fetch  Indicates what you wish to retrive from the common view data service. Options are:
#' \describe{
#' \item{"code"}{The default. Returns the four letter code of the I & M network}
#' \item{"name"}{Returns the full name of the I & M network}
#'  \item{"full"}{Returns both the 4 letter codea and the full name of the I&M network}
#'  \item{"all"}{Returns all data from the "networks" table of the common view}
#' }
#' @param data Indicates the type of data you are interesed in. Currently has no function as only water monitoring data can be retrieved.
#' @return Eithter a \code{vector} with network codes or names, or a \code{data.frame} with network data.
#'  
#' @export

cvnetworks<-function(fetch="code", data="water"){
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Networks" 
  optfetch<-switch(fetch,
                   code="?$select=NetworkCode",
                   name="?$select=NetworkName",
                   full="?$select=NetworkCode,NetworkName",
                   all=""
  )
  fromJSON(url(paste0(base,optfetch)),flatten=TRUE)[[2]]
}

#' @include filterpaste.R
#' 
#' @title cvcharacteristics
#' 
#' @description Returns the name, other data for wat4er qruaily charactersitcs that are avaialable through the NPS common view
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @param fetch  Indicates what you wish to retrive from the common view data service. Options are:
#' \describe{
#' \item{"name"}{Returns the name of the characteristic}
#' \item{"full"}{Return the:  Charactersiticn name, substrate, sample fraction, category, display name, units, and details.}
#' \item{"all"}{Returns all data from the "Characteristic" table of the common view.}
#' }
#' @param network  A network four letter code in quotes. Only data on parks in that network will be returned.
#' @param park A park four letter code in qoutes. Only data from that park will be returned.
#' @param data Indicates the type of data you are interesed in. Currently has no function as only water monitoring data can be retrieved.
#'  
#' @return Either a \code{vector} with site codes or names, or a \code{data.frame} with all network data.
#'  
#' @export


cvcharacteristics<-function(fetch="name",network=NA, park=NA, data="water"){  
  
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Characteristics"
  
#   optexpand<-if(all(is.na(c(network,park)))) "" else "?$expand=Data"  # we need this to get network, parks and sites filters
#   
#   
#   filtnames<-c("Site/NetworkCode","Site/ParkCode")[!is.na(c(network,park))]
#   filtvalues<-c(network,park)[!is.na(c(network,park))]
#    
#   optfilter<-filterpaste(punct="&", names=filtnames, values=filtvalues) #if we have a filter we already used "?" in expand
   
  #selectpunct<-if(all(is.na(c(network,park)))) "?" else "&" 
  
  selectpunct<-"?"
  
  optselect<-switch(fetch,
                    name=paste0(selectpunct,"$select=CharacteristicName"),
                    full=paste0(selectpunct,"$select=CharacteristicName,CharacteristicSubstrate,CharacteristicSampleFraction,CharacteristicCategory,CharacteristicNameDisplay,CharacteristicUnits,CharacteristicDetails"),
                    all=""
  )  
  #return(paste0(base,optexpand,optfilter,optselect))
  fromJSON(url(paste0(base,optselect)),flatten=TRUE)[[2]]  
}



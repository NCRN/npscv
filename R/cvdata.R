#' @include filterpaste.R
#' 
#' @title cvdata
#' 
#' @description Returns data from the NPS common view.
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @param fetch  Indicates what you wish to retrive from the common view data service. Options are:
#' \describe{
#' \item{"basic"}{Returns the measurement data from the common view.}
#' \item{"full"}{Return the measurement data as well as metadata on the characteristics and sites.}
#' \item{"all"}{Returns all data from the Datum, Characteristic", Site, Site, Date and Time tables of the common view.}
#' }
#' @param network  A network four letter code in quotes. Only data on parks in that network will be returned.
#' @param park A park four letter code in quotes. Only data from that park will be returned.
#' @param site A site code in quotes. Only data from that site will be returned. 
#' @param characteristic A characterisitc name in quotees. Only data from that characteristic will be returned. 
#' @param data Indicates the type of data you are interesed in. Currently has no function as only water monitoring data can be retrieved.
#'  
#' @return A \code{data.frame} with the selected data. 
#'  
#' @export





cvdata<-function(fetch="all",network=NA, park=NA, site=NA, characteristic=NA, data="water"){  
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Data?$expand=Characteristic,Date,Site,Time"
  filtnames<-c("Site/NetworkCode","Site/ParkCode","Site/SiteCode","Characteristic/CharacteristicName")[!is.na(c(network,park,site,characteristic))]
  filtvalues<-c(network,park,site,characteristic)[!is.na(c(network,park,site,characteristic))]
  optfilter<-filterpaste(punct="&",names=filtnames,values=filtvalues)
  optfetch<-switch(fetch,
                   basic="&$select=CharacteristicValue, CharacteristicLabFlag, CharacteristicOtherFlag, CharacteristicDepth, TaxonomicID, TaxonomicSource, IndividualID, Replicate, Characteristic/CharacteristicName, Characteristic/CharacteristicNameDisplay, Characteristic/CharacteristicUnits, Date/StandardDate, Site/NetworkCode, Site/ParkCode, Site/SiteCode, Site/SiteName, Site/SiteType, Time/StandardTime",
                     
                   full="&$select=CharacteristicValue, CharacteristicLabFlag, CharacteristicOtherFlag, CharacteristicDepth, DataMethod, DetectionLimit,
QuantificationLimit, QuantificationLimitUpper, TaxonomicID, TaxonomicSource, IndividualID, Replicate, Characteristic/CharacteristicName, Characteristic/CharacteristicSubstrate, Characteristic/CharacteristicSampleFraction, Characteristic/CharacteristicCategory, Characteristic/CharacteristicNameDisplay, Characteristic/CharacteristicUnits, Characteristic/DefaultGraphMin, Characteristic/DefaultGraphMax, Characteristic/CharacteristicDetails, Date/StandardDate, Site/NetworkCode, Site/NetworkName, Site/ParkCode,Site/ParkName,Site/SiteCode,Site/SiteName,Site/USGSCodeSite, Site/USGSCodeNearest, Site/SiteType, Site/SiteLat, Site/SiteLon, Site/Description,Time/StandardTime",
                   all=""
  )
  DataOut<-fromJSON(url(paste0(base,optfilter,optfetch)),flatten=TRUE)[[2]]
  
  if(fetch!="all"){
    names(DataOut)[names(DataOut)=="Characteristic.CharacteristicName"]<-"CharacteristicName"
    names(DataOut)[names(DataOut)=="Characteristic.CharacteristicSubstrate"]<-"CharacteristicSubstrate"
    names(DataOut)[names(DataOut)=="Characteristic.CharacteristicSampleFraction"]<-"CharacteristicSampleFraction"
    names(DataOut)[names(DataOut)=="Characteristic.CharacteristicCategory"]<-"CharacteristicCategory"
    names(DataOut)[names(DataOut)=="Characteristic.CharacteristicNameDisplay"]<-"CharacteristicNameDisplay"
    names(DataOut)[names(DataOut)=="Characteristic.CharacteristicUnits"]<-"CharacteristicUnits"
    names(DataOut)[names(DataOut)=="Characteristic.DefaultGraphMin"]<-"DefaultGraphMin"
    names(DataOut)[names(DataOut)=="Characteristic.DefaultGraphMax"]<-"DefaultGraphMax"
    names(DataOut)[names(DataOut)=="Characteristic.CharacteristicDetails"]<-"CharacteristicDetails"
  
    names(DataOut)[names(DataOut)=="Date.StandardDate"]<-"Date"
  
    names(DataOut)[names(DataOut)=="Site.NetworkCode"]<-"NetworkCode"
    names(DataOut)[names(DataOut)=="Site.NetworkName"]<-"NetworkName"
    names(DataOut)[names(DataOut)=="Site.ParkCode"]<-"ParkCode"
    names(DataOut)[names(DataOut)=="Site.ParkName"]<-"ParkName"
    names(DataOut)[names(DataOut)=="Site.SiteCode"]<-"SiteCode"
    names(DataOut)[names(DataOut)=="Site.SiteName"]<-"SiteName"
    names(DataOut)[names(DataOut)=="Site.USGSCodeSite"]<-"USGSCodeSite"
    names(DataOut)[names(DataOut)=="Site.USGSCodeNearest"]<-"USGSCodeNearest"
    names(DataOut)[names(DataOut)=="Site.SiteType"]<-"SiteType"
    names(DataOut)[names(DataOut)=="Site.SiteLat"]<-"SiteLat"
    names(DataOut)[names(DataOut)=="Site.SiteLon"]<-"SiteLon"
    names(DataOut)[names(DataOut)=="Site.Description"]<-"Description"
  
    names(DataOut)[names(DataOut)=="Time.StandardTime"]<-"StandardTime"
  }
  return(DataOut)

}

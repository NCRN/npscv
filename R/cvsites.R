




cvsites<-function(fetch="code",network=NA, park=NA, data="water"){  
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Sites"
  optnetwork<-ifelse(is.na(network), "", paste0("?$filter=NetworkCode eq '",network,"'"))
  optpark<-paste0(
    if(is.na(park)) "" else {if (is.na(netork)) "?" else "&"},   # added  correct punctuation
  ### now that we have correct punctation - do the filter.
    if(is.na(park)) "" else "$filter=ParkCode eq '",park,"'"
  )
              
              
              
              
              fromJSON(url(paste0(base,optnetwork,optpark)))  
}






cvsites<-function(fetch="code",network=NA, park=NA, data="water"){  
  
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Sites"
  
  filts<-c("park","network")[!is.na(c(park,network))]
  
  
  
  
  optnetwork<-ifelse(is.na(network), "", paste0("?$filter=NetworkCode eq '",network,"'"))
  
  optpark<-paste0(
    if(is.na(park)) "" else {if (is.na(network)) "?" else "&"},   # added  correct punctuation
  ### now that we have correct punctation - do the filter.
    if(is.na(park)) "" else paste0("$filter=ParkCode eq '",park,"'")
  )

  optfetch<-switch(fetch,
                   code="$select=ParkCode",
                   name="$select=ParkName",
                   all=""
  )  
              
              return(filts)
             # return(paste0(base,optnetwork,optpark))
             # fromJSON(url(paste0(base,optnetwork,optpark)))  
}



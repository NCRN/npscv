#' @include filterpaste.R
#' 
#' @title cvsites Returns the name, site code and other data for monitoring sites with data avaialable through the NPS common view
#' 
#' @description 
#' 
#' @importFrom jsonlite fromJSON
#' 
#'  @param fetch  Indicates what you wish to retrive from the common view data service. Options are:
#'  \describe{
#'  \item{"code"}{The default. Returns the code for the site
#'  \item{"name"}{Returns the full name of the site}
#'  \item{"all"}{Returns all data from the "parks" table of the common view as well as the nework name and code.}
#'  }
#'  @param network  A network four letter code in quotes. Only data on parks in that network will be returned.
#'  @param park A park four letter code in qoutes. Only data from that park will be returned.
#'  @param data Indicates the type of data you are interesed in. Currently has no function as only water monitoring data can be retrieved.
#'  @return Either a \code{vector} with site codes or names, or a \code{data.frame} with all network data.
#'  
#'  @export
#'  






cvsites<-function(fetch="code",network=NA, park=NA, data="water"){  
  
  base<-"http://irmadevservices.nps.gov/WaterQualityDataServices/OData/Sites"
  
  filtnames<-c("network","park")[!is.na(c(network,park))]
  filtvalues<-c(network,park)[!is.na(c(network,park))]
  
  optfilter<-filterpaste(punct="?", names=filtnames, values=filtvalues)
  
  

  optselect<-paste0((if(all(is.na(c(network,park)))) "?" else "&"), 
                    switch(fetch,
                   code="$select=ParkCode",
                   name="$select=ParkName",
                   all=""
  ))  
             #return(paste0(base,optfilter,optselect))
             fromJSON(url(paste0(base,optfilter)))  
}




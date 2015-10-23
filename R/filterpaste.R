#' @title filterpaste
#' 
#' @description Creates the paste arguement to be passed to the OData service
#' 
#'  @param punct The punctuation that should being the filter arugment. If filter is the first argument it should be "?" otherwise use "&".
#'  @param names  A \code{vector} with the name of the fields you wish to filter on. Names shoudl be in quotes
#'  @param values The values for each field indicated in \code{names}. Must be in the same order as \code{names}. 
#'  @return A text string that can be used for the fiter arguement of an OData call.
#'  
#'  @details This is an internal helper function. It creates the filter options for the OData call. Currently it cannot handle "or" type arugments. That is, if you want data from two parks - Park 1 or Park2 this cannot create the apporiate filter -  you would have to make 2 data calls. The output of this fuction is then pasted ot the propoer url and other arugments to make the comnplete Odata call.
#'  



filterpaste<-function(punct="&",names, values){
  
  if (length(names)==0) return("")
  opts<-paste(names,paste0("'",values,"'"),sep=" eq ", collapse = " and ")
  filtopts<-paste0(punct,"$filter=",opts)
  return(filtopts)

  
  
}
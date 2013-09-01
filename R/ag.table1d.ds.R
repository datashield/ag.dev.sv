#' 
#' @title Creates 1 dimensional contingency tables - potentially disclosive data are suppressed
#' @description The function pbs.table.1d is a server-side subfunction of datashield.aggregate(). 
#' It generates 1-dimensional table for all data sources. Valid (non-disclosive) data are defined as data
#' from studies where no table cells have counts between 1 and 4 (the upper value [4] can in principle be changed but only by
#' rewriting the underlying function - it cannot be changed by a standard DataSHIELD user). If the count in any cell
#' in the table produced by a given data source IS invalid, that cell count is changed to "-1" and the name of the category
#' corresponding to the count is changed to "-9"
#' @param  vect1 A single vector (variable) with a limited series of discrete values (numeric or alphanumeric) - typically a factor
#' @return A list object with one component from each separate study which contains the following items where out.obj is the
#' assigned output list obj:
#' \code{is.table.valid}     For example, out.obj[[2]]$is.table.valid is a logical (Boolean) indicator
#' of whether the data in study 2 is entirely valid (TRUE) for the stated variable or at least one category is invalid (FALSE) 
#' \code{safe.table}         For example, out.obj[[1]]$safe.table contains the safe (non-disclosive) one dimensional table that can be released from
#' study 1 - unsafe data are concealed by converting invalid counts to "-1" and their corresponding catregory identifier to "-9" 
#' @author Burton, P.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("DIS_DIAB")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate the two-dimensional table
#' out.obj <- datashield.aggregate(opals, quote(ag.table1d.ds(D$DIS_DIAB))) 
#'}
#'
ag.table1d.ds <- function(vect1){
 
  # START Script 2
  # get.var.names.script
  
  # Generic block of script to extract names of vectors from arguments to assign or aggregate functions
  # ACTION                                       
  # Assign vector of interest to be script.vect1 #
  # vect1 keep as it is
  # If you need more vectors just add them in here #
  # and duplicate lines below                     #
  var.name.1_os2=deparse(substitute(vect1))
  
  # Returns as many key output object names as specified
  # var.name.1_os2 is name of first variable extracted from input object 
  
  # get.var.names.script
  # END Script 2
  
       
  # START Script 1
  # valid.id.test.script
  
  # Generic block of script to test that a 1d vector that is to be used as a categorical factor
  # has no categories with between 1 and 4 observations
  # ACTION 
  # Assign vector of interest to be factor_is1 #
  factor_is1<-vect1                            
  # Assign its name from get.var.names.script #
  var.name.1_is1<-var.name.1_os2              
  print(var.name.1_os2)
  
  # Reset to K+1 if the non-allowable range is changed to 1 to K
  critical.min<-5 
  factor.valid<-TRUE
  
  table.new<-table(factor_is1)
  for(j in 1:length(table.new))
  {
    if(table.new[j]<critical.min && table.new[j] > 0)
    {
      table.new[j]<-(-1)
      factor.valid<-FALSE
      names(table.new)[j]<-(-9)
    }
  }
  names(dimnames(table.new))<-paste("Categories/Counts for Variable:\n",var.name.1_is1)
  names(factor.valid)<-paste(var.name.1_is1)
  out.list_os1<-list(factor.valid,as.table(table.new))
  names(out.list_os1)<-list("is.factor.valid","safe.table")
  
  # Returns 1 key output object
  # out.list_os1 is primary output: a list containing 
  out.list_os1<-out.list_os1
  
  # valid.1d.test.script
  # END Script 1
  
  out.list_os1
}


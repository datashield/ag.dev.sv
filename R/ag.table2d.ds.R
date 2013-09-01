#'
#' @title Creates 2-dimensional contingency tables - potentially disclosive data are suppressed
#' @description The function pbs.table.2d is a server-side subfunction of datashield.aggregate(). 
#' It generates 2-dimensional tables for all data sources. Valid (non-disclosive) data are defined as data
#' from studies where no table cells have counts between 1 and 4 (the upper value [4] can in principle be changed but only by
#' rewriting the underlying function - it cannot be changed by a standard DataSHIELD user). If the count in any cell
#' in the table produced by a given data source IS invalid, that cell count is changed to "-1" and the name of the two categories
#' that correspond to it are both changed to "-9"
#' @param    obj1   One parameter but it combines two vectors (variables) each with a limited series of discrete
#' values (numeric or alphanumeric) - typically factors. Note they must be specified as a
#' single object with the two vectors linked using cbind().
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
#' myvar <- list("DIS_DIAB","GENDER")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate the two-dimensional table
#' out.obj <- datashield.aggregate(opals, obj1=quote(ag.table2d.ds(cbind(D$DIS_DIAB,D$GENDER))))
#'}
#' 
ag.table2d.ds <- function(obj1){

{ # START Script 2
  # get.var.names.script.2
  # Generic block of script to extract names of vectors from arguments to assign or aggregate functions
  # ACTION
  # Assign vector of interest to be script.vect1 #
  # vect1 - keep as is (otherwise deparse fails) #
  # If you need more vectors just add them in here#
  #and duplicate lines below                     #
  vect1_os2 <- obj1[,1]
  vect2_os2 <- obj1[,2]
  var.name.1_os2 <- "VAR.1"
  var.name.2_os2 <- "VAR.2"
  # get.var.names.script.2
} # END Script 2

{ # START script 1
  # valid.id.test.script.1
  # Generic block of script to test that a 1d vector that is to be used as a categorical factor
  # has no categories with between 1 and 4 observations
  # ACTION   
  # Assign vector of interest to be factor_is1 #
  factor1_is1 <- vect1_os2
  factor2_is1 <- vect2_os2
  #Assign its name from get.var.names.script #
  var.name.1_is1 <- var.name.1_os2
  var.name.2_is1 <- var.name.2_os2
 
  #Reset to K+1 if the non-allowable range is changed to 1 to K
  critical.min <- 5 
  
  table.valid <- TRUE
  table.new <- table(factor1_is1,factor2_is1,dnn=c(var.name.1_is1,var.name.2_is1))
  nnr <- dim(table.new)[1]
  nnc <- dim(table.new)[2]

  for(j in 1:nnr)
  {
    for(k in 1:nnc)
    {
       if(table.new[j,k]<critical.min && table.new[j,k] > 0)
        {
        table.new[j,k] <- (-1)
        table.valid <- FALSE
        dimnames(table.new)[[1]][j] <- (-9)
        dimnames(table.new)[[2]][k] <- (-9)
        }
    }
  }
  out.list_os1 <- list(table.valid,as.table(table.new))
  names(out.list_os1) <- list("is.table.valid","safe.table")
  
  # Returns 1 key output object
  # out.list_os1 is primary output: a list containing 
  out.list_os1 <- out.list_os1
  
  # valid.1d.test.script.1
} # END Script 1 

  out.list_os1
}


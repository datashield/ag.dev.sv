#'
#' @title Checks that variables do exist and do not contains missing data only.
#' @description this functions checks if the variables are present in the dataset assigned 
#' to R from an opal datasource and if the variables are empty (contain NAs only).
#' This function is internal (not available to users) as it is only required by \code{ds.glm}.
#' THIS FUNCTION IS NOT USED ALONE, IT IS CALLED BY THE FUNCTION \code{ds.checkvar} FROM THE PACKAGE \code{dsbaseclient}.
#' @param dataset a \code{dataframe} that contains the data assigned to R
#' @param variable the variables to check
#' @return '0' if the variable did not fail any of the checks and '1' if the variable failed
#' one or more checks.
#' @author Gaye, A.
#' @export
#' @examples {
#' # Please see the examples in the documentation of the function 'ds.glm' from the package 'dsmodellingclient'
#' }
#'
ag.checkvar.ds <- function(dataset, variable){
  
  # record the results of the checks, the recording variable
  # is set to 0 initially (i.e. both tests are negative)
  toremove <- 0
  
  # get names of the variables that have been assigned to R
  var.names <- colnames(dataset)
  
  # variables to hold the results of the checks
  misngvar1 <- c()
  misngvar2 <- c()
  tagtemp1 <- c()
  tagtemp2 <- c() 
  
  # 1) check if each of the variables in 'formula' is present in the list of 
  # assigned variables, if missing record it in the vector 'tagtemp1'
  # 2) if the vector is not missing check that it does not contain only missing 
  # values, if that is the case record the information in the vector 
  
  tagtemp1 <- !(variable %in% var.names)
  if(tagtemp1){ 
      misngvar1 <- append(misngvar1, variable) 
  }else{
    # check how if variable contains only missings, if yes record
    col2check <- which(var.names == variable)
    lmiss <- length(which(is.na(dataset[,col2check])))
    lvar  <- length(dataset[,col2check])  
    if(lmiss > 0){      
      tagtemp2 <- lmiss == lvar
      if(tagtemp2){ misngvar2 <- append(misngvar2, variable) }
    }else{
      tagtemp2 <- 0
    }
  }

  # if any of the variables in the formula is missing set the
  # recording variable to 1
  if(sum(tagtemp1) > 0 ){ 
    toremove <- 1
  }
  
  # if any of the variables in the formula is empty 
  # (i.e. contains missing values) set the the recording
  # variable to 2     
  if(sum(tagtemp2) > 0 ){ 
    toremove <- 2
  } 
  
  return(toremove)
}

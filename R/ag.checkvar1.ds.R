#'
#' @title Checks that variables do exist and do not contains missing data only.
#' @description this functions checks if the variables are present in the dataset assigned 
#' to R from an opal datasource and if the variables are empty (contain NAs only).
#' This function is internal (not available to users) as it is only required by \code{ds.glm}.
#' @param dataset a \code{dataframe} that contains the data assigned to R
#' @param variable the variables to check
#' @return '0' if the variable did not fail any of the checks and '1' if the variable failed
#' one or more checks.
#' @author Gaye, A.
#' @export
#' 
ag.checkvar1.ds <- function(dataset, variable){
  
  # keep track of the datasets/studies that fail any one or more of the checks
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

  # if any of the variables in the formula is missing tell and 
  # removed the study which has missing variables
  if(sum(tagtemp1) > 0 ){ 
    mm <- paste(misngvar1, collapse=",")
    cat("The variable", mm, "is missing from \n")       
    cat("This study will not be included in the analysis\n")
    toremove <- 1
  }
  
  # if any of the variables in the formula is empty 
  # (i.e. contains missing values only removed the study     
  if(sum(tagtemp2) > 0 ){ 
    mm <- paste(misngvar2, collapse=",")
    cat("The variable", mm, "in is empty (NAs only)\n")       
    cat("This study will not be included in the analysis\n")
    toremove <- 1
  } 
  cat("\n")
  
  return(toremove)
}

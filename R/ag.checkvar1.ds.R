#'
#' @title Checks that all studies have the variables included in the glm \code{formula}
#' @description The combined glm fit fails if the outcome variable or one of the covariates
#' is missing from any of the studies. This function is called by the function \code{ds.glm}
#' to ensure that only studies that contains all the required variables are included in
#' the analysis. If a study is excluded an alert is displayed to inform the user.
#' This function is internal (not available to users) as it is only required by \code{ds.glm}.
#' @param data a \code{dataframe} that hold the data assigned from the opal datasource to R
#' @param servername name of the login object the data were assigned to
#' @param formula an object of class \code{formula} which describes the model to be fitted
#' @return an object of class \code{opal}, the input object (if no study has been excluded)
#' or an updated one if some study(ies) has been excluded due to missing variables.
#' @author Gaye, A.
#' 
ag.checkvar1.ds <- function(data, servername, formula){

   # get the names of the variables from the formula
   xx <- all.vars(formula)
   variables <- xx[-1]
   
   # lists to hold the results of the checks for all the datasets/studies
   misngvar1 <- c()
   misngvar2 <- c()
   
   # keep track of the datasets/studies that fail any one or more of the checks
   toremove <- 0
     
   # get names of the variables that have been assigned from this opal
   var.names <- colnames(data)
     
   # declare two vectors which hold the results of the two checks that are conducted
   # tagtemp1 tells if a variable is not present in a datasets/study/server
   # tagtemp2 tells if a variable contains only missing values  
   tagtemp1 <- vector("numeric", (length(variables)))
   tagtemp2 <- vector("numeric", (length(variables)))
     
   # 1) check if each of the variables in 'formula' is present in the list of 
   # assigned variables, if missing record it in the vector 'tagtemp1'
   # 2) if the vector is not missing check that it does not contain only missing 
   # values, if that is the case record the information in the vector 
     
   for(j in 1:length(variables)){
   tagtemp1[j] <- !(variables[j] %in% var.names)
     if(tagtemp1[j]){ 
       misngvar1 <- append(misngvar1, variables[j]) 
     }else{
       # check how if variable contains only missings, if yes record
       col2check <- which(var.names == variables[j])
       lmiss <- length(which(is.na(data[,col2check])))
       lvar  <- length(data[,col2check])  
       if(lmiss > 0){      
         tagtemp2[j] <- lmiss == lvar
         if(tagtemp2[j]){ misngvar2 <- append(misngvar2, variables[j]) }
       }else{
         tagtemp2[j] <- 0
       }
     }
   }
   # if any of the variables in the formula is missing tell and 
   # removed the study which has missing variables
   if(sum(tagtemp1) > 0 ){ 
     mm <- paste(misngvar1, collapse=",")
     if(length(misngvar1) > 1){
       cat("The variables", mm, "are missing from ", servername,"\n")
     }else{
       cat("The variable", mm, "is missing from ", servername,"\n")       
     }
     cat("This study will not be included in the analysis\n")
     toremove <- 1
   }

   # if any of the variables in the formula is empty 
   # (i.e. contains missing values only removed the study     
   if(sum(tagtemp2) > 0 ){ 
     mm <- paste(misngvar2, collapse=",")
     if(length(misngvar2) > 1){
       cat("The variables", mm, "in", servername," are empty (NAs only)\n")
     }else{
       cat("The variable", mm, "in", servername," is empty (NAs only)\n")       
     }
     cat("This study will not be included in the analysis\n")
     toremove <- 1
   } 
   cat("\n")
   
   # set the output to NULL if the study failes any of the tests
   if(toremove) { 
     data <- NULL
   }
        
   return(data)
}


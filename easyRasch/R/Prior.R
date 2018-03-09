#' Runs Prior function
#'
#' 
#'
#' @param theta numeric designating difficulty 
#' @return dnorm mean=0, sd=3 of theta
#' @author Jack Ploshnick
#' @note simply calculates dnorm 
#' @examples
#' 

#' @rdname Prior
#' @export

setGeneric("Prior", #sets generic function in S4
           function(theta="numeric") {
             standardGeneric("Prior")
           } )

setMethod("Prior", "numeric",
 function(theta){ #takes in theta
  height<- dnorm(theta, mean=0, sd=3) #calculates Height
  
  return(height) #returns Height
})

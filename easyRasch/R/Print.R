#' Runs Print function
#'
#' 
#'
#' @param x object of class Rasch
#' @return Print of Name, integral value, integral error
#' @author Jack Ploshnick
#' @note Very simple print function
#' @examples
#' 

#' @rdname Print
#' @export
setMethod("print", "Rasch",
          function(x){
            print(x@name) #prints name
            
            z<- EAP(x,-6,6) #does EAP calculation 
            
            print(z$value) #prints value
            print(z$abs.error) #prints absolute error
            
          })

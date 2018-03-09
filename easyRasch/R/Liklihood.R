#' Runs Liklihood function
#'
#' 
#'
#' @param raschObj a object of class Rasch
#' @param theta a numeric designating theta 
#'
#' @return A numeric that is the product of PQ values 
#' @author Jack Ploshnick
#' @note I am not exactly sure what math I am suppose to do here. This is my best intepretation 
#' @examples
#' 
#' @rdname Liklihood
#' @export

setGeneric("Liklihood", #sets generic function in S4
           function(raschObj="Rasch", theta= "numeric") {
             standardGeneric("Liklihood")
           } )

setMethod("Liklihood",c( "Rasch", "numeric"),
  function(raschObj, theta){
  
  
  P_Q_values = Probability(raschObj,theta)[[2]] #This gets the PQ values from Probability function
  
  
  for(i in 1:length(raschObj@y_j_value)){#this loop calculates PQ values
    
    if(raschObj@y_j_value[i]==1){#if y==1, use P.i.j value
      P_Q_values[i] = P_Q_values[i]^(raschObj@y_j_value[i])
      
    }
    if(raschObj@y_j_value[i]==0){ #if y==0, use 1- p.i.j value
      P_Q_values[i] = P_Q_values[i]^(1-raschObj@y_j_value[i])
      
    }
    
    x<- prod(P_Q_values) # Multiplies them all together 
    
    return(x)
  }
})

#' Runs Probability function
#'
#' 
#'
#' @param raschObj a object of class Rasch
#' @param theta numeric designating difficulty 
#'
#' @return two lists. one of P.i.j. One of PQ
#' @author Jack Ploshnick
#' @note It works with a for loop
#' @examples
#' 

#' @rdname Probability 
#' @export
#' 

setGeneric("Probability", #sets generic function in S4
           function(raschObj="Rasch", theta= "numeric") {
             standardGeneric("Probability")
           } )

setMethod("Probability",c( "Rasch", "numeric"),
          function(raschObj, theta){
  
  PQ= vector(mode="numeric", length= length(test@y_j_value)) #creates blank PQ vector
  P= vector(mode="numeric", length= length(test@y_j_value)) #creates blank P vector
  
  for(i in 1:length(raschObj@y_j_value)){ #this loop calculates the P values
    P.i.j= (exp(theta - raschObj@a_value[i]))/(1+exp(theta - raschObj@a_value[i])) #formula from paper
    
    P[i]= P.i.j}
  
  
  for(i in 1:length(raschObj@y_j_value)){#this loop calculates PQ values
    
    if(raschObj@ y_j_value[i]==1){#if y==1, use P.i.j value
      P.i.j= (exp(theta - raschObj@a_value[i]))/(1+exp(theta - raschObj@a_value[i]))
      
      PQ[i]<- P.i.j # P
    }
    if(raschObj@ y_j_value[i]==0){ #if y==0, use 1- p.i.j value
      P.i.j= (exp(theta - raschObj@a_value[i]))/(1+exp(theta - raschObj@a_value[i]))
      
      PQ[i]<- 1-P.i.j # Q
    }
    
    
  }
  return(list(P, PQ)) #this returns both vectors 
  
})


### creating Rasch class



setClass(Class="Rasch",  #Sets S4 class of Rasch
         representation = representation(
           name = "character", # three slots as specified in problem set
           a_value = "vector", 
           y_j_value = "vector"
         ),
         prototype = prototype(
           name = c(), #default values are empty 
           a_value=  c(),
           y_j_value  = c()
         )
)





setValidity("Rasch", function(object){  
  
  valuesLength= (length(object@a_value)== length(object@y_j_value))#ensures values the same length
  
  CarTest = function(object){
    z<- (object@y_j_value ==1 | object@y_j_value ==0 ) #ensures only values of 0 or 1 possible as y_j value
    return(z)
  }
  
  if(!valuesLength | !all(CarTest(object))){ #returns error if chosen Rasch is broken        
    stop("Rasch not valid")
  }
  
})


setMethod("initialize", "Rasch", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})

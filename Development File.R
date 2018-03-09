#Start time 6:52 PM on 3/3/18

a= c(1,2,3,4,5,4,3,2,1) #difficulty

y= c(1,1,1,0,0,1,1,0,1) #answer. 1 is correct. 0 is incorrect 

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

setValidity("Rasch", function(object){ #do this later 

})


setMethod("initialize", "Rasch", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})

########### Making probability function 

test<- new('Rasch', name= "Steve", a_value= a, y_j_value= y  )


PQ= vector(mode="numeric", length= length(test@y_j_value))
P= PQ= vector(mode="numeric", length= length(test@y_j_value))

work.pls<- function(Rasch, theta){
  for(i in 1:length(Rasch@y_j_value)){
    P.i.j= (exp(theta - Rasch@a_value[i]))/(1+exp(theta - Rasch@a_value[i]))
    
    P[i]= P.i.j}
    
    
   for(i in 1:length(Rasch@y_j_value)){
    
    if(Rasch@ y_j_value[i]==1){
      P.i.j= (exp(theta - Rasch@a_value[i]))/(1+exp(theta - Rasch@a_value[i]))
      
      PQ[i]<- P.i.j
    }
    if(Rasch@ y_j_value[i]==0){
      P.i.j= (exp(theta - Rasch@a_value[i]))/(1+exp(theta - Rasch@a_value[i]))
      
      PQ[i]<- 1-P.i.j
    }
    
    
  }
  return(list(P, PQ))
 
}


work.pls(test, 6)



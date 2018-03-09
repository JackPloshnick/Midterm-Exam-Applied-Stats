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


PQ= vector(mode="numeric", length= length(test@y_j_value)) #creates blank PQ vector
P= vector(mode="numeric", length= length(test@y_j_value)) #creates blank P vector

Probability<- function(raschObj, theta){
  for(i in 1:length(raschObj@y_j_value)){ #this loop calculates the P values
    P.i.j= (exp(theta - raschObj@a_value[i]))/(1+exp(theta - raschObj@a_value[i]))
    
    P[i]= P.i.j}
    
    
   for(i in 1:length(raschObj@y_j_value)){#this loop calculates PQ values
    
    if(raschObj@ y_j_value[i]==1){
      P.i.j= (exp(theta - raschObj@a_value[i]))/(1+exp(theta - raschObj@a_value[i]))
      
      PQ[i]<- P.i.j # P
    }
    if(raschObj@ y_j_value[i]==0){
      P.i.j= (exp(theta - raschObj@a_value[i]))/(1+exp(theta - raschObj@a_value[i]))
      
      PQ[i]<- 1-P.i.j # Q
    }
    
    
  }
  return(list(P, PQ)) #this returns both vectors 
 
}


Probability(test, -6)

############## Liklihood function 

Liklihood<- function(raschObj, theta){
  P_Q_values = Probability(raschObj,theta)[[2]] #This gets the PQ values from Probability function
  x<- prod(P_Q_values) # Multiplies them all together 
  
  return(x)
}

Liklihood(test, 5)

####### Prior

?dnorm


Prior<- function(theta){ #takes in theta
  height<- dnorm(theta, mean=0, sd=3) #calculates Height
  
  return(height) #returns Height
}

Prior(45)

###### EAP


EAP<- function(raschObj, lower, upper){
  
  myFunction <- function(raschObj, theta){# I am not quite sure what the question is asking 
    f= theta* Liklihood(raschObj, theta) *Prior(theta) #I interpret the question to mean, integrate over the function
    ## theta* liklihood(theta)* Prior(theta). This is the function I am trying to integrate. If the problem asked for me to
    ### integrerate over a diferent formula, I can do that, I just don't understand the question. 
    
    return(f)
  }
 
 g_of_theta = integrate(myFunction, raschObj= raschObj, lower= lower, upper= upper)#integrates over function 
  
  return(g_of_theta)
}

z<- EAP(test,-6,6)




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
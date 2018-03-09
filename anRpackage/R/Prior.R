Prior<- function(theta){ #takes in theta
  height<- dnorm(theta, mean=0, sd=3) #calculates Height
  
  return(height) #returns Height
}
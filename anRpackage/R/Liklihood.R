Liklihood<- function(raschObj, theta){
  
  
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
}
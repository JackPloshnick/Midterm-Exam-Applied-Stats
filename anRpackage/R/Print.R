setMethod("print", "Rasch",
          function(x){
            print(x@name) #prints name
            
            z<- EAP(x,-6,6) #does EAP calculation 
            
            print(z$value) #prints value
            print(z$abs.error) #prints absolute error
            
          })
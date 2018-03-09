#Start time 6:52 PM on 3/3/18
#end time 11:33 PM on 3/3/18


########### make the Package

library(devtools)
library(roxygen2)

package.skeleton()


current.code <- as.package("easyRasch")
load_all(current.code)
document(current.code)

?Liklihood







############### function 

a= c(1,2,3,4,5,4,3,2,1) #difficulty

y= c(1,1,0,0,0,0,1,1,1) #answer. 1 is correct. 0 is incorrect 


test<- new('Rasch', name= "Steve", a_value= a, y_j_value= y  )

Probability(test, 4)

Liklihood(test, 4)

Prior(4)


z<- EAP(test,4,4)



print(test) #integral from -6 to 6

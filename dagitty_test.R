library(dagitty)

mydag1<-dagitty( "dag{
                  X -> Y
                  Y <- Z <- X
                  X <- A -> Z
                  
                 }")
coordinates(mydag1) <- list( 
  y = c(A = 0, X = 1, Y = 1, Z = 0),
  x = c(A = 0, X = 0, Y = 2, Z = 1))

plot(mydag1)

adjustmentSets(mydag1, exposure="X", outcome= "Y")


## chapter 10
rm(list = ls())
library(rethinking)


## 1
## maximum entropy
par(mfrow=c(2,1))

p <- 0.7
( A <- c( (1-p)^2 , p*(1-p) , (1-p)*p , p^2 ) )
plot( 1:4 , A , type="b" , main="calculated" )
-sum( A*log(A) )

sim.p <- function(G=1.4) {
    x123 <- runif(3)
    x4 <- ( (G)*sum(x123)-x123[2]-x123[3] )/(2/G)
    z <- sum( c(x123,x4) )
    p <- c( x123 , x4 )/z
    list( H=-sum( p*log(p) ) , p=p )
}

H <- replicate( 1e5 , sim.p(1.4) )
dens( as.numeric(H[1,]) , adj=0.1 )

entropies <- as.numeric(H[1,])
distributions <- H[2,]
max(entropies)

mdist <- distributions[ which.max(entropies) ][[1]]
mdist

plot( 1:4 , mdist , type="b" , main="simulated" )

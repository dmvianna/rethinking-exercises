rm(list = ls())
library(rethinking)

## I'm a real wild chain
y <- c(-1,1)
set.seed(11)
m9.2 <- ulam(
    alist(
        y ~ dnorm( mu , sigma ) ,
        mu <- alpha ,
        alpha ~ dnorm( 1 , 10 ) ,
        sigma ~ dexp( 1 )
    ) , data=list(y=y) , chains=3
)

precis( m9.2 )

pairs( m9.2@stanfit )

traceplot( m9.2 )

trankplot( m9.2 )

## non-identifiable
set.seed(41)
y <- rnorm( 100 , mean=0 , sd=1 )

set.seed(384)
m9.4 <- ulam(
    alist(
        y ~ dnorm( mu , sigma ) ,
        mu <- a1 + a2 ,
        a1 ~ dnorm( 0 , 10 ) ,
        a2 ~ dnorm( 0 , 10 ) ,
        sigma ~ dexp( 1 )
    ) , data=list(y=y) , chains=3
)

precis( m9.4 )

traceplot( m9.4 )

trankplot( m9.4 )

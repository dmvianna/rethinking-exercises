rm(list = ls())
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$A <- standardize( d$MedianAgeMarriage )
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )

m5.1 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bA * A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

m5.2 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bM * M ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

m5.3 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bM*M + bA*A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

set.seed(24071847)
compare( m5.1 , m5.2 , m5.3 , func=PSIS )

set.seed(24071847)
PSIS_m5.3 <- PSIS( m5.3 , pointwise=TRUE )
set.seed(24071847)
WAIC_m5.3 <- WAIC( m5.3 , pointwise=TRUE )
par(mfrow=c(2,3))
plot( PSIS_m5.3$k , WAIC_m5.3$penalty , xlab="PSIS Pareto k" ,
     ylab="WAIC penalty" , col=rangi2 , lwd=2 )

identify( x=PSIS_m5.3$k , y=WAIC_m5.3$penalty , labels=d$Loc )

## use Student t for thick tails
m5.3t <- quap(
    alist(
        D ~ dstudent( 2 , mu , sigma ) ,
        mu <- a + bM*M + bA*A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

## PSIS(m5.3t)
compare( m5.3 , m5.3t , func=PSIS )

precis( m5.3 )
precis( m5.3t )

set.seed(24071847)
PSIS_m5.3t <- PSIS( m5.3t , pointwise=TRUE )
set.seed(24071847)
WAIC_m5.3t <- WAIC( m5.3t , pointwise=TRUE )
## par(mfrow=c(2,3))
plot( PSIS_m5.3t$k , WAIC_m5.3t$penalty , xlab="PSIS Pareto k" ,
     ylab="WAIC penalty" , col=rangi2 , lwd=2 )
identify( x=PSIS_m5.3t$k , y=WAIC_m5.3t$penalty , labels=d$Loc )

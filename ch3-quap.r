## finding the posterior distribution with quap
library(rethinking)
par(mfrow=c(2,3))

data(Howell1)
d <- Howell1
str(d)

d2 <- d[ d$age >= 18 , ]
str(d2)

flist <- alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif( 0 , 50 )
)

m4.1 <- quap( flist , data=d2 )
precis( m4.1 )

## strong prior
m4.2 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu ~ dnorm( 178 , 0.1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2
)
precis( m4.2 )

m4.1.vcov <- vcov( m4.1 )

## these are variances
diag( m4.1.vcov )
## these are correlations
cov2cor( m4.1.vcov )

## extract samples
post <- extract.samples( m4.1 , n=1e4 )
precis( post )

dens( post )

par(mfrow=c(2,3))
plot( post )

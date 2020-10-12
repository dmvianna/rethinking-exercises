## load data and copy
rm(list = ls())
par(mfrow=c(2,3))
library(rethinking)
data(milk)
d <- milk
str(d)

d$K <- standardize( d$kcal.per.g )
d$N <- standardize( d$neocortex.perc )
d$M <- standardize( log(d$mass) )

## data has cases with NA
## m5.5_draft <- quap(
##     alist(
##         K ~ dnorm( mu , sigma ) ,
##         mu <- a + bN*N ,
##         a ~ dnorm( 0 , 1 ) ,
##         bN ~ dnorm( 0 , 1 ) ,
##         sigma ~ dexp( 1 )
##     ) , data = d
## )

dcc <- d[ complete.cases(d$K,d$N,d$M) , ]

## prior is too permissive
m5.5_draft <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bN*N ,
        a ~ dnorm( 0 , 1 ) ,
        bN ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp( 1 )
    ) , data = dcc
)

prior <- extract.prior( m5.5_draft )
xseq <- c(-2,2)
mu <- link( m5.5_draft , post=prior , data=list(N=xseq) )
plot( NULL, xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

## OK prior
m5.5 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bN*N ,
        a ~ dnorm( 0 , 0.2 ) ,
        bN ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = dcc
)

prior <- extract.prior( m5.5 )
xseq <- c(-2,2)
mu <- link( m5.5 , post=prior , data=list(N=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

precis( m5.5 )

## Doesn't look like there would be a strong relationship
xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.5 , data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc ,
     ylab="kcal / g (std)" , xlab="neocortex percent (std)"
     )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

## Try another relationship
m5.6 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bM*M ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = dcc
)

precis( m5.6 )

## weak negative relationship
xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.6 , data=list(M=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ M , data=dcc
   , ylab="kcal / g (std)" , xlab="log female body mass (std)"
     )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

## now we try a multivariate model
m5.7 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bN*N + bM*M ,
        a ~ dnorm( 0 , 0.2 ) ,
        bN ~ dnorm( 0 , 0.5 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = dcc
)

precis( m5.7 )

plot( coeftab( m5.5 , m5.6 , m5.7 ) , pars=c("bM", "bN") )

## M and N are positively correlated
## N is positively correlated with K
## M is negatively correlated with K
## M and N cancel each other's effect on K!
## a multivariate model teases their effects out,
## revealing strong relationships on K for both.
pairs( ~K + M + N , dcc )

## counterfactuals
xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M+0.15 , length.out=30) )
mu <- link( m5.7 , data=data.frame( M=xseq , N=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$M) , ylim=range(dcc$K) ,
     xlab="log female body mass (std)" , ylab="kcal / g (std)"
     )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N+0.15 , length.out=30) )
mu <- link( m5.7 , data=data.frame( N=xseq , M=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$N) , ylim=range(dcc$K) ,
     xlab="log neocortex percent (std)" , ylab="kcal / g (std)"
     )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

## simulate masking
## M -> K <- N
## M -> N
n <- 100
M <- rnorm( n )
N <- rnorm( n , M )
K <- rnorm( n , N - M )
d_sim <- data.frame(K=K,N=N,M=M)

## markov equivalence
library(dagitty)
dag5.7 <- dagitty( "dag{
 M -> K <- N
 M -> N }")

drawdag(dag5.7)
coordinates(dag5.7) <- list( x=c(M=0,K=1,N=2), y=c(M=0.5,K=1,N=0.5) )

MElist <- equivalentDAGs(dag5.7)

drawdag(MElist)

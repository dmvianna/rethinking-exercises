## load data and copy
rm(list = ls())
par(mfrow=c(2,3))
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

## standardise variables
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage )

sd( d$MedianAgeMarriage )

m5.1 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bA * A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    )
    , data = d
)

## simulate Median Age Marriage vs Divorce rate
set.seed(10)
prior <- extract.prior( m5.1 )
mu <- link( m5.1 , post=prior , data=list( A=c(-2,2) ) )
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2)
   , xlab="Median age marriage (std)"
   , ylab="Divorce rate (std)")
for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("black",0.4) )

## compute percentile interval of mean
A_seq <- seq( from=-3 , to=3.2 , length.out=30 )
mu <- link( m5.1 , data=list(A=A_seq) )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

## plot it all
plot( D ~ A , data=d , col=rangi2 )
lines( A_seq , mu.mean , lwd=2 )
shade( mu.PI , A_seq )

## simulate Marriage vs Divorce rate
m5.2 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bM * M ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

## compute percentile interval of mean
M_seq <- seq( from=-3 , to=3.2 , length.out=30 )
mu <- link( m5.2 , data=list(M=M_seq) )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

## plot it all
plot( D ~ M , data=d , col=rangi2 )
lines( M_seq , mu.mean , lwd=2 )
shade( mu.PI , M_seq )

## overthinking draw DAG
library(dagitty)
dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" )
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
drawdag( dag5.1 )

## implications
DMA_dag2 <- dagitty( "dag{ D <- A -> M }")
impliedConditionalIndependencies( DMA_dag2 )

DMA_dag1 <- dagitty( "dag{ D <- A -> M -> D }")
impliedConditionalIndependencies( DMA_dag1 )

## multiple regression
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
precis( m5.3 )

## approximating the posterior
plot( coeftab(m5.1,m5.2,m5.3), par=c("bA","bM"))
## Once we know median age at marriage (A) for a state,
## there is little or no additional predictive power in
## also knowling the rate of marriage in that state.

## simulate
N <- 50 # number of simulated states
age <- rnorm( N ) # sim A
mar <- rnorm( N , -age ) # sim A -> M
div <- rnorm( N , age ) # sim A -> D

## predictor residual plot

m5.4 <- quap(
    alist(
        M ~ dnorm( mu , sigma ) ,
        mu <- a + bAM * A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bAM ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

mu <- link( m5.4 )
mu_mean <- apply( mu , 2 , mean )
mu_resid <- d$M - mu_mean

## posterior predictive plot
## call link without specifying new data
## so it uses original data
mu <- link( m5.3 )
## summarise samples across cases
mu_mean <- apply( mu , 2 , mean )
mu_PI <- apply( mu , 2 , PI )
## simmulate observations
## again no new data, so uses original data
D_sim <- sim( m5.3 , n=1e4 )
D_PI <- apply( D_sim , 2 , PI )
plot( mu_mean ~ d$D , col=rangi2 , ylim=range(mu_PI) ,
     xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) ) lines( rep(d$D[i],2) , mu_PI[,i] , col=rangi2 )

identify( x=d$D , y=mu_mean , labels=d$Loc )

## simulating a spurious predictor
N <- 100 # number of cases
x_real <- rnorm( N ) # x_real as Gaussian with mean 0 and sd 1
x_spur <- rnorm( N , x_real ) # x_spur as Gaussian with mean=x_real
y <- rnorm( N , x_real ) # y as Gaussian with mean=x_real
d <- data.frame(y, x_real, x_spur) # bind all together in a data frame
pairs(d)


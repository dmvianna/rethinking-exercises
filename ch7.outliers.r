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

## 7E1
## 1. The measure of uncertainty should increase linearly as a function of the probability of an event
## 2. The measure of uncertainty should increase as the number of potential events increases
## 3. The measure of uncertainty should be additive. That is, it should be possible to add its component measures of uncertainty.

## 7E2
p <- c( 0.3 , 0.7 )
-sum( p * log(p) )
## 0.61

## 7E3
p <- c( 0.2 , 0.25 , 0.25 , 0.3 )
-sum( p * log(p) )
## 1.38

## 7E4
p <- c( 1/3 , 1/3 , 1/3 )
-sum( p * log(p) )
# 1.10

## 7M2
## Model Selection: Chooses the model that's best at predicting
## Model Comparison: Doesn't discard models, so we can study how variables influence prediction. This can be valuable information for causal inference.

## 7M3
## Individual observations might be highly influential. Given that a single extreme observation might is unlikely to be reproduced in all future samples, these are indicative of how likely the model is to overfit.

## 7M5
## Informative priors reduce the model's sensitivity to data. It assigns less likelihood to extreme values.

## 7M6
## Overly informative priors reduce the model's sensitivity to data to an extent akin to ignoring relevant information contained therein.


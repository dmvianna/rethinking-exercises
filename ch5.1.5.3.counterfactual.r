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

m5.3_A <- quap(
    alist(

        ## A -> D <- M
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bM*M + bA*A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 ) ,

        ## A -> M
        M ~ dnorm( mu_M , sigma_M ) ,
        mu_M <- aM + bAM*A ,
        aM ~ dnorm( 0 , 0.2 ) ,
        bAM ~ dnorm( 0 , 0.5 ) ,
        sigma_M ~ dexp( 1 )
    ) , data = d
)

precis( m5.3_A )

## prepare interventions 2 sd above and 2 sd below the mean
A_seq <- seq( from=-2 , to=2 , length.out=30 )
## prep data
sim_dat <- data.frame( A=A_seq )
## simulate M and then D, using A_seq
s <- sim( m5.3_A , data=sim_dat , vars=c("M","D") )

## plot it
plot( sim_dat$A , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
     xlab="manipulated A" , ylab="counterfactual D" )
shade( apply( s$D , 2 , PI ) , sim_dat$A )
mtext( "Total counterfactual effect of A on D" )

## plot it
plot( sim_dat$A , colMeans(s$M) , ylim=c(-2,2) , type="l" ,
     xlab="manipulated A" , ylab="counterfactual M" )
shade( apply( s$M , 2 , PI ) , sim_dat$A )
mtext( "Counterfactual effect A -> M" )

## what if median age marriage moved from 20 to 30 years old?
## new data frame, standardised to mean 26.1 and sd 1.24
sim2_dat <- data.frame( A=(c(20,30)-26.1)/1.24)
s2 <- sim( m5.3_A , data=sim2_dat , vars=c("M","D") )
mean( s2$D[, 2] - s2$D[, 1] )

## break A -> M, simulate D after manipulating M
sim_dat <- data.frame( M=seq(from=-2,to=2,length.out=30) , A=0 )
s <- sim( m5.3_A , data=sim_dat , vars="D" )

plot( sim_dat$M , colMeans(s) , ylim=c(-2,2) , type="l" ,
     xlab="manipulated M" , ylab="counterfactual D" )
shade( apply(s,2,PI) , sim_dat$M )
mtext( "Total counterfactual effect of M on D" )


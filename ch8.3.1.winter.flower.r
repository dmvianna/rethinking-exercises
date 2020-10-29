rm(list = ls())
library(rethinking)
data(tulips)
d <- tulips
str(d)

d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

m8.4 <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ) ,
        mu <- a + bw*water_cent + bs*shade_cent ,
        a ~ dnorm( 0.5 , 0.25 ) ,
        bw ~ dnorm( 0 , 0.25 ) ,
        bs ~ dnorm( 0 , 0.25 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

m8.5 <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ) ,
        mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent ,
        a ~ dnorm( 0.5 , 0.25 ) ,
        bw ~ dnorm( 0 , 0.25 ) ,
        bs ~ dnorm( 0 , 0.25 ) ,
        bws ~ dnorm( 0 , 0.25 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

## plot posterior predictions
## par(mfrow=c(1,3)) # 3 plots in 1 row
par(mfrow=c(2,3))
for ( model in c(m8.4, m8.5) ) {
    for ( s in -1:1 ) {
        idx <- which( d$shade_cent==s )
        plot( d$water_cent[idx] , d$blooms_std[idx] , xlim=c(-1,1) , ylim=c(0,1) ,
             xlab="water" , ylab="blooms" , pch=16 , col=rangi2 )
        mtext(paste0( "post: shade:" , s))
        mu <- link( model , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
        for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
    }
}

## plot prior predictions
set.seed(7)
for ( model in c(m8.4, m8.5) ) {
    prior <- extract.prior(model)
    for ( s in -1:1 ) {
        idx <- which( d$shade_cent==s )
        plot( NULL , xlim=c(-1.01,1.01) , ylim=c(-0.5,1.5) ,
             xlab="water" , ylab="blooms" , pch=16 , col=rangi2 )
        abline( h=1 , lty=2 )
        abline( h=0 , lty=2 )
        mtext(paste0( "prior: shade:" , s))
        mu <- link( model, post=prior , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
        lines( -1:1 , mu[1, ] , col=col.alpha("black",1) )
        for ( i in 2:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
    }
}

## 8E1
## 1. heat
## 2. field of expertise
## 3. Oxygen

## 8E2
## 1 2 4

## 8E3
## 1
## caramel_onions ~ dnorm( mu , sigma )
## mu <- a + bw*water + bh*heat + bwh*water*heat
## a ~ dnorm
## bw ~ dnorm
## bh ~ dnorm
## bwh ~ dnorm
## sigma ~ dexp(1)

## 2
## speed ~ dnorm( mu , sigma )
## mu <- a + bc*ncyl + bi*inj + bci*ncyl*inj
## bc ~ dnorm
## bi ~ dnorm
## bci ~ dnorm
## sigma ~ dexp(1)

## 3
## pol ~ dnorm( mu , sigma )
## mu <- a[bid] + b[bid]*belief
## a[bid] ~ dnorm
## b[bid] ~ dnorm
## sigma ~ dexp(1)

## 4
## intel ~ dnorm( mu , sigma )
## mu <- a + bs*soc + b[mid]*man + bsm*soc*man
## bs ~ dnorm
## b[mid] ~ dnorm
## bsm ~ dnorm
## sigma ~ dexp(1)

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
        mu <- link( m8.5 , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
        for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
    }
}

## polynomial regression and b-splines
library(rethinking)

data(Howell1); d <- Howell1

par(mfrow=c(2,3))
plot( height ~ weight , d )

d$weight_s <- ( d$weight - mean(d$weight) ) / sd(d$weight)
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3
m4.5 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b1 * weight_s + b2 * weight_s2 + b3 * weight_s3,
        a ~ dnorm( 178 , 20 ) ,
        b1 ~ dlnorm( 0 , 1 ) ,
        b2 ~ dnorm( 0 , 1 ) ,
        b3 ~ dnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d
)

precis( m4.5 )

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2 , weight_s3=weight.seq^3 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) , xaxt="n" )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )
at <- c(-2,-1,0,1,2)
labels <- at * sd(d$weight) + mean(d$weight)
axis( side=1 , at=at , labels=round(labels,1) )

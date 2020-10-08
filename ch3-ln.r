## finding the posterior distribution with quap
library(rethinking)

data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]

par(mfrow=c(2,3))
plot(d$height ~ d$weight)

## simulate heights using priors

set.seed(2971)
N <- 100 # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )
## plot
plot( NULL , xlim=range(d2$weight) , ylim=c(-100, 400) ,
     xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i] * (x - xbar) ,
                       from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                                col=col.alpha("black",0.2) )

## log-normal
b <- rlnorm( 1e4 , 0 , 1 )
dens( b , xlim=c(0,5) , adj=0.1 )

## simulate log-normal heights using priors
set.seed(2971)
N <- 100 # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rlnorm( N , 0 , 1 )
## plot
plot( NULL , xlim=range(d2$weight) , ylim=c(-100, 400) ,
     xlab="weight" , ylab="height" )
abline( h=272 , lty=1 , lwd=0.5 )
abline( h=0 , lty=2 )
mtext( "b ~ dlnorm(0,1)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i] * (x - xbar) ,
                       from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                                col=col.alpha("black",0.2) )

## quap

## load data
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
## defaine average weight
xbar <- mean(d2$weight)
##fit model
m4.3 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b * ( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2
)
precis( m4.3 )

round( vcov( m4.3 ) , 3)

## plot model
par(mfrow=c(2,3))

plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean( post$a )
b_map <- mean( post$b )
curve( a_map + b_map * ( x - xbar ) , add=TRUE )

## plot many lines, by increasing the number of cases we're learning from
learn_slowly <- function(N) {
    dN <- d2[ 1:N , ]
    mN <- quap(
        alist(
            height ~ dnorm( mu , sigma ) ,
            mu <- a + b * ( weight - mean( weight )) ,
            a ~ dnorm( 178 , 20 ) ,
            b ~ dlnorm( 0 , 1 ) ,
            sigma ~ dunif( 0 , 50 )
        ) , data=dN
    )
    ## extract 20 samples from the posterior
    post <- extract.samples( mN , n=20 )
    ## display raw data and sample size
    plot( dN$weight , dN$height ,
         xlim=range( d2$weight ) , ylim=range( d2$height ) ,
         col=rangi2 , xlab="weight" , ylab="height" )
    mtext( concat("N = ", N) )
    ## plot the lines, with transparency
    for ( i in 1:20 )
        curve( post$a[i] + post$b[i] * ( x - mean(dN$weight) ) ,
              col=col.alpha("black", 0.3) , add=TRUE )
}

for (n in c(3,10,50,150,352)) learn_slowly(n)

## mu at 50
mu_at_50 <- post$a + post$b * ( 50 - xbar )
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )

PI( mu_at_50 , prob=0.89 )

mu <- link( m4.3 )
str(mu)

## define sequence of weights to compute predictions for
## these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )

## use link to compute mu
## for each sample from posterior
## and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

## use type="n" to hide raw data
plot( height ~ weight , d2 , type="n" )
## loop over samples and plot each mu value
for ( i in 1:1000 )
    points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

## summarise the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

## plot raw data
## fading out points to make line and interval more visible
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
## plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
## plot a shaded region for 89% PI
shade( mu.PI , weight.seq )

## how link works
post <- extract.samples( m4.3 )
mu.link <- function(weight) post$a + post$b * ( weight - xbar )
weight.seq <- seq( from=25 , to=70 , by=1 )
mu <- sapply( weight.seq , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.CI <- apply( mu , 2 , PI , prob=0.89 )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89)

## prediction intervals
sim.height <- sim( m4.3 , data=list(weight=weight.seq) )
str(sim.height)

height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

## plot everything
## plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
## draw MAP on line
lines( weight.seq , mu.mean )
## draw HPDI region for line
shade( mu.HPDI , weight.seq )
## draw PI region for simulated heights
shade( height.PI , weight.seq )

## how sim works
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply(
    weight.seq , function(weight)
        rnorm(
            n=nrow(post) ,
            mean=post$a + post$b * ( weight - xbar ) ,
            sd=post$sigma
        )
)
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

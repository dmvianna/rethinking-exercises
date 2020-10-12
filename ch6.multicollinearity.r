## load data and copy
rm(list = ls())
par(mfrow=c(2,3))
library(rethinking)

## grant selection on trustworthiness and newsworthiness
## set.seed(1914)
N <- 200 # num grant proposals
p <- 0.1 # proportion to select
## uncorrelated newsworthiness and trustworthiness
nw <- rnorm(N)
tw <- rnorm(N)
## select top 10% of combined scores
s <- nw + tw # total score
q <- quantile( s , 1-p ) # top 10 threshold
selected <- ifelse( s >= q , TRUE , FALSE )
cor( tw[selected] , nw[selected] )

plot( tw ~ nw , ylab="trustworthiness" , xlab="newsworthiness" )
points(nw[selected], tw[selected], col = "blue" , pch=19 )
model <- lm( tw[selected] ~ nw[selected] )
abline( model , col = "blue" )


## multicollinear legs
N <- 100 # number of individuals
set.seed(909) 
height <- rnorm(N,10,2) # sim total height of each
leg_prop <- runif(N,0.4,0.5) # leg as proportion of height
leg_left <- leg_prop*height + # sim left as proportion + error
    rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + #sim right as proportion + error
    rnorm( N , 0 , 0.02 )
d <- data.frame(height, leg_left, leg_right) # combine into data.frame

m6.1 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + bl*leg_left + br*leg_right ,
        a ~ dnorm( 10 , 100 ) ,
        bl ~ dnorm( 2 , 10 ) ,
        br ~ dnorm( 2 , 10 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

precis(m6.1)
plot(precis(m6.1))

post <- extract.samples(m6.1)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

sum_blbr <- post$bl + post$br
dens( sum_blbr , col=rangi2 , lwd=2 , xlab="sum of bl and br" )

m6.2 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + bl*leg_left ,
        a ~ dnorm( 10 , 100 ) ,
        bl ~ dnorm( 2 , 10 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

precis(m6.2)

## multicollinear milk
data(milk)
d <- milk

d$K <- standardize( d$kcal.per.g )
d$F <- standardize( d$perc.fat )
d$L <- standardize( d$perc.lactose )

## kcal.per.g regressed on perc.fat
m6.3 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bF*F ,
        a ~ dnorm( 0 , 0.2 ) ,
        bF ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

## kcal.per.g regressed on perc.lactose
m6.4 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bL*L ,
        a ~ dnorm( 0 , 0.2 ) ,
        bL ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

## both strong correlations
precis( m6.3 )
precis( m6.4 )

## now add both to the model
m6.5 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bL*L + bF*F ,
        a ~ dnorm( 0 , 0.2 ) ,
        bF ~ dnorm( 0 , 0.5 ) ,
        bL ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

# much much weaker correlations
precis( m6.5 )

## a-ha
pairs( ~ kcal.per.g + perc.fat + perc.lactose , data=d , col=rangi2 )

## simulate collinearity

sim.coll <- function( r=0.9 ) {
    d$x <- rnorm( nrow(d) , mean=r*d$perc.fat ,
                 sd=sqrt( (1-r^2)*var(d$perc.fat) ) )
    m <- lm( kcal.per.g ~ perc.fat + x , data=d )
    sqrt( diag( vcov(m) ) )[2] # stdev of parameter
}

rep.sim.coll <- function( r=0.9 , n=100 ) {
    stddev <- replicate( n , sim.coll(r) )
    mean(stddev)
}

r.seq <- seq( from=0 , to=0.99 , by=0.01 )
stddev <- sapply( r.seq , function(z) rep.sim.coll(r=z,n=100) )
plot( stddev ~ r.seq , type="l" , col=rangi2 , lwd=2 , xlab="correlation" )


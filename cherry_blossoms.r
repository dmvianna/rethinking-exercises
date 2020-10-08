## polynomial regression and b-splines
library(rethinking)
library(splines)

par(mfrow=c(2,3))

data(cherry_blossoms)
d <- cherry_blossoms
precis(d)

## simple plot
plot( y=d$doy , x=d$year )

## splines
d2 <- d[ complete.cases(d$doy) , ] # complete cases on doy
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq( 0 , 1, length.out=num_knots ) )

## cubic basis functions
B <- bs(d2$year ,
        knots=knot_list[ -c( 1 , num_knots ) ] ,
        degree=3 ,
        intercept=TRUE
        )

## visualise cubic basis functions
plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
for ( i in 1:ncol(B) ) lines( d2$year , B[,i] )

## the model
m4.7 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + B %*% w ,
        a ~ dnorm(100,10) ,
        w ~ dnorm(0,10),
        sigma ~ dexp(1)
    ) ,
    data=list( D=d2$doy , B=B ) ,
    start=list( w=rep( 0 , ncol(B) ) )
)

## plot basis * weight
post <- extract.samples( m4.7 )
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
     xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i] * B[,i] )

## 97% posterior interval for mu, at each year
mu <- link( m4.7 )
mu_PI <- apply( mu , 2, PI , 0.97 )
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )


## 4M1

## model
set.seed(0)
N <- 1000
mu <- rnorm( N, 0, 10 )
sigma <- rexp( N, 1 )
y <- rnorm( N, mu , sigma )
dens(y)

## 4M2
m4.m2 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu ~ dnorm( 0 , 10 ) ,
        sigma ~ dexp( 1 )
    ) ,
    data=list(D=c(0,1,2))
)

## 4M3
## Yi ~ Normal( mu , sigma )
## mu = alpha + beta * Xi
## alpha ~ Normal( 0 , 10 )
## beta ~ Uniform( 0 , 1 )
## sigma ~ Exponential( 1 )

## 4M4
## height ~ Normal( mu , sigma )  # distribution of heights is normal
## mu = alpha + beta * ( year - year_xbar )
## distribution of means is normal, centered in xbar, 20 cm sd
## alpha ~ Normal( xbar , 20 )
## slope is between 0 (no relationship) and 1 (extreme positive relationship)
## beta ~ Log-Normal( 0 , 1 )
## sigma ~ Uniform( 0 , 20 )  # 20 cm sd too

## 4M5
## No revision. My beta already takes care of that.

## 4M6
## This suggests that my sigma sd might be too wide. I should shorten it to 8 cm.

## 4M7
## bigger alpha covariance when xbar is ommitted; minimal change in posterior?!

## load data
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
## defaine average weight
xbar <- mean(d2$weight)

##fit model
set.seed(0)
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

set.seed(0)
m4.3.xbarless <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b * ( weight ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2
)

## precis( m4.3 )

round( vcov( m4.3 ) , 3 )
round( vcov( m4.3.xbarless ) , 3 )

## plot model
par(mfrow=c(1,2))

plot.model <- function( model , title ) {
    ## define sequence of weights to compute predictions for
    ## these values will be on the horizontal axis
    weight.seq <- seq( from=25 , to=70 , by=1 )

    ## use link to compute mu
    ## for each sample from posterior
    ## and for each weight in weight.seq
    mu <- link( model , data=data.frame(weight=weight.seq) )
    ## str(mu)

    ## summarise the distribution of mu
    mu.mean <- apply( mu , 2 , mean )
    mu.PI <- apply( mu , 2 , PI , prob=0.89 )

    ## plot raw data
    ## fading out points to make line and interval more visible
    plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) , main=title)
    ## plot the MAP line, aka the mean mu for each weight
    lines( weight.seq , mu.mean )
    ## plot a shaded region for 89% PI
    shade( mu.PI , weight.seq )
}

plot.model( m4.3 , "with xbar" )
plot.model( m4.3.xbarless , "without xbar" )

## 4M8

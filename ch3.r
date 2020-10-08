library(rethinking)
par(mfrow=c(2,3))

growth <- replicate( 1e4 , prod( 1 + runif( 12 , 0 , 0.1 )))

dens( growth )

big <- replicate( 1e4 , prod( 1 + runif( 12 , 0 , 0.5)))
small <- replicate( 1e4 , prod( 1 + runif( 12 , 0 , 0.1 )))
dens(big, main="big")
dens(small, main="small")
log.big <- replicate( 1e4 , log(prod( 1 + runif( 12, 0, 0.5 ))))
dens(log.big, main="log.big")

## !Kung San

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 ,]

dens(d2$height)

curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

## prior predictive simulation
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

## grid approximation of the posterior
mu.list <- seq( from=150 , to=160 , length.out=100 )
sigma.list <- seq( from=7, to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) ,
                  function(i) sum(
                                  dnorm(
                                      d2$height ,
                                      post$mu[i] ,
                                      post$sigma[i] ,
                                      log=TRUE
                                  )
                              )
                  )
post$prob <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
    dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prob - max(post$prob) )

contour_xyz( post$mu , post$sigma , post$prob )

image_xyz( post$mu , post$sigma, post$prob)

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE , prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot( sample.mu , sample.sigma , cex=2 , pch=16 , col=col.alpha( rangi2 , 0.05 ))

dens( sample.mu , main="sample.mu")
dens( sample.sigma , main="sample.sigma" )

PI( sample.mu )
PI( sample.sigma )

## overthinking

d3 <- sample( d2$height , size=20 )
mu.list <- seq( from=150 , to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
    sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
               log=TRUE )))
post2$prod <- post2$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
    dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                       prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=1 ,
     col=col.alpha( rangi2 , 0.1 ) , xlab="mu" , ylab="sigma", pch=16 )
dens( sample2.sigma , norm.comp=TRUE)


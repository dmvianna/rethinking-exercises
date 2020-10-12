## load data and copy
rm(list = ls())
par(mfrow=c(2,3))
library(rethinking)
data(Howell1)
d <- Howell1

## male | female
d$sex <- ifelse( d$male==1 , 2 , 1 )
str( d$sex )

m5.8 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a[sex] ,
        a[sex] ~ dnorm( 178 , 20 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data = d
)

precis( m5.8 , depth=2 )

post <- extract.samples(m5.8)
post$diff_fm <- post$a[,1] - post$a[,2]
precis( post , depth=2 )

## clades
data(milk)
d <- milk
levels(d$clade)

d$clade_id <- as.integer( d$clade )
str(d$clade)

d$K <- standardize( d$kcal.per.g )
m5.9 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a[clade_id] ,
        a[clade_id] ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
labels <- paste0( "a[" , 1:4 , "]:" , levels(d$clade) )
plot( precis( m5.9 , depth=2 , pars="a" ) , labels=labels ,
     xlab="expected kcal (std)" )

set.seed(63)
d$house <- sample( rep(1:4, each=8) , size=nrow(d) )

m5.10 <- quap(
    alist(
        K ~ dnorm( mu , sigma ) ,
        mu <- a[clade_id] + h[house] ,
        a[clade_id] ~ dnorm( 0 , 0.5 ) ,
        h[house] ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)

houses <- c("Gryffindor","Hufflepuff","Ravenclaw","Slytherin")
labels <- paste0( "d[" , 1:4 , "]:" , houses )
plot( precis(m5.10, depth=2 , pars="h" ) , labels=labels ,
     xlab="expected kcal (std)" )


## Exercises

## 5E1
## 2 (alpha = 0) and 4

## 5E2
## Animal diversity A
## latitude L
## plant diversity P

## A ~ Normal( mu , sigma )
## mu <- alpha + bL*L + bP*P
## alpha ~ Normal( 0 , 0.2 )
## bL ~ Normal( 0 , 0.5 )
## bP ~ Normal( 0 , 0.5 )
## sigma ~ dexp( 1 )

## 5E3
## Amount of Funding F
## Size of Laboratory L
## Time to PhD T

## T ~ Normal( mu , sigma )
## mu <- alpha + bL*L + bF*F
## alpha ~ Normal( 0 , 0.2 )
## bL ~ Normal( 0 , 0.5 )  # should be positive -- bigger lab shorter PhD
## bF ~ Normal( 0 , 0.5 )  # should be negative -- more funding longer PhD
## sigma ~ dexp( 1 )

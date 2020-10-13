## load data and copy
rm(list = ls())
par(mfrow=c(2,3))
library(rethinking)

set.seed(71)
## number of plants
N <- 100
## simulate initial heights
h0 <- rnorm(N,10,2)

## assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm( N, 5 - 3*fungus )
## compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )
precis(d)

sim_p <- rlnorm( 1e4 , 0 , 0.25 )
precis( data.frame(sim_p))

m6.6 <- quap(
    alist(
        h1 ~ dnorm( mu , sigma ) ,
        mu <- h0*p ,
        p ~ dlnorm( 0 , 0.25 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
precis(m6.6)

m6.7 <- quap(
    alist(
        h1 ~ dnorm( mu , sigma ) ,
        mu <- h0 * p ,
        p <- a + bT*treatment + bF*fungus ,
        a ~ dlnorm( 0 , 0.2 ) ,
        bT ~ dnorm( 0 , 0.5 ) ,
        bF ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
precis(m6.7)

m6.8 <- quap(
    alist(
        h1 ~ dnorm( mu , sigma ) ,
        mu <- h0 * p ,
        p <- a + bT*treatment ,
        a ~ dlnorm( 0 , 0.2 ) ,
        bT ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
precis(m6.8)

library(dagitty)
plant_dag <- dagitty("dag {
H_0 -> H_1
F -> H_1
T -> F
}")
coordinates( plant_dag ) <- list(
    x=c(H_0=0,T=2,F=1.5,H_1=1) ,
    y=c(H_0=0,T=0,F=0,H_1=0)
)
drawdag( plant_dag )

impliedConditionalIndependencies(plant_dag)

## hidden variable influencing post-treatment variable
set.seed(71)
N <- 1000
h0 <- rnorm(N,10,2)
treatment <- rep( 0:1 , each=N/2 )
M <- rbern(N)
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 + 0.4*M )
h1 <- h0 + rnorm( N , 5 + 3*M )
d2 <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )


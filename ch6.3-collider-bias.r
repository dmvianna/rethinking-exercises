## load data and copy
rm(list = ls())
par(mfrow=c(2,3))
library(rethinking)

## innate happiness; happy people marry earlier
## as time goes by, both married and unmarried groups
## get sadder, as one gets sadder people in, and the
## other supplies the former with the happiest people
## it has.

## happiness is set at birth and doesn't change,
## but when we condition on marriage, it seems to
## influence happiness, because both age and
## happiness influence it.
d <-sim_happiness( seed=1977 , N_years=1000 )
precis(d)

d2 <- d[ d$age > 17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

d2$mid <- d2$married + 1
m6.9 <- quap(
    alist(
        happiness ~ dnorm( mu , sigma ) ,
        mu <- a[mid] + bA*A ,
        a[mid] ~ dnorm( 0 , 1 ) ,
        bA ~ dnorm( 0 , 2 ) ,
        sigma ~ dexp( 1 )
    ) , data = d2
)
precis( m6.9 , depth=2 )

m6.10 <- quap(
    alist(
        happiness ~ dnorm( mu , sigma ) ,
        mu <- a + bA*A ,
        a ~ dnorm( 0 , 1 ) ,
        bA ~ dnorm( 0 , 2 ) ,
        sigma ~ dexp( 1 )
    ) , data = d2
)
precis( m6.10 )


## the haunted DAG

## set slope effects
N <- 200 # number of grandparent-parent-child triads
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect of G on C
b_PC <- 1 # direct effect of P on C
b_U  <- 2 # direct effect of U on P and C

## now draw random observations
set.seed(1)
U <- 2*rbern( N , 0.5 ) - 1
G <- rnorm( N )
P <- rnorm( N , b_GP*G + b_U*U )
C <- rnorm( N , b_PC*P + b_GC*G + b_U*U )
d <- data.frame( C=C , P=P , G=G , U=U )

m6.11 <- quap(
    alist(
        C ~ dnorm( mu , sigma ) ,
        mu <- a + b_PC*P + b_GC*G ,
        a ~ dnorm( 0 , 1 ) ,
        c( b_PC , b_GC ) ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
precis( m6.11 )

## now add the confounder explicitly
m6.12 <- quap(
    alist(
        C ~ dnorm( mu , sigma ) ,
        mu <- a + b_PC*P + b_GC*G + b_U*U ,
        a ~ dnorm( 0 , 1 ) ,
        c( b_PC , b_GC , b_U ) ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
precis( m6.12 )

## two roads
library(dagitty)
dag_6.1 <- dagitty("dag{
U [unobserved]
X -> Y
X <- U <- A -> C -> Y
U -> B <- C
}")
adjustmentSets( dag_6.1 , exposure="X" , outcome="Y" )

## waffle states
dag_6.2 <- dagitty("dag{
A -> D
A -> M -> D
A <- S -> M
S -> W -> D
}")
adjustmentSets( dag_6.2 , exposure="W" , outcome="D" )

impliedConditionalIndependencies( dag_6.2 )

## Practice

## 6E1
## Multicollinearity - very strong association between two or more predictor variables.
## Post-treatments bias - using a variable that depends on the treatment as a predictor.
## Collider bias - if B -> A <- C, if we condition on B, this will induce a statistical
## association between B and C.

## 6E3
## Fork - X <- Z -> Y - X _||_ Y | Z
## Pipe - X -> Z -> Y - conditioning on Z blocks the path between X and Y
## Collider - X -> Z <- Y - conditioning on Z creates spurious association between X and Y
## Descendant - X -> Z <- Y ; Z -> D - conditioning on D partly conditions on Z

## 6M1
## 4 paths
dag_6.M1 <- dagitty("{
U [unobserved]
V [unobserved]
X -> Y
X <- U <- A -> C -> Y
U -> B <- C
C <- V -> Y
}")
adjustmentSets( dag_6.1 , exposure="X" , outcome="Y" )

## Could close either C or A
## Closing A we could condition on C

## 6M2
## X -> Z -> Y

N <- 200 # number of cases
bX <- 0.9 # direct effect of X on Z
bZ <- 0.9 # direct effect of Z on Y
set.seed(1)
X <- rnorm( N )
Z <- rnorm( N , bX*X )
Y <- rnorm( N , bZ*Z )
d <- data.frame( X=X , Z=Z , Y=Y )

m6.m2 <- quap(
    alist(
        Y ~ dnorm( mu , sigma ) ,
        mu <- alpha + bX*X + bZ*Z ,
        alpha ~ dnorm( 0 , 1 ) ,
        c(bX,bZ) ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
precis( m6.m2 )

m6.m2b <- quap(
    alist(
        Y ~ dnorm( mu , sigma ) ,
        mu <- alpha + bZ*Z ,
        alpha ~ dnorm( 0 , 1 ) ,
        bZ ~ dnorm( 0 , 1 ) ,
        sigma ~ dexp( 1 )
    ) , data = d
)
precis( m6.m2b )

## weak collinearity, because the effect of X is expressed through Z. There is no other causal path.

## 6M3
dag_6.m1 <- dagitty("dag{
X <- Z <- A -> Y
X -> Y
}")
adjustmentSets( dag_6.m1 , exposure="X" , outcome="Y" )

dag_6.m2 <- dagitty("dag{
X -> Z -> A -> Y
Z -> Y
X -> Y
}")
adjustmentSets( dag_6.m2 , exposure="X" , outcome="Y" )

dag_6.m3 <- dagitty("dag{
X <- A -> Z <- Y
X -> Z
X -> Y
}")
adjustmentSets( dag_6.m3 , exposure="X" , outcome="Y" )

dag_6.m4 <- dagitty("dag{
X <- A -> Z -> Y
X -> Z
X -> Y
}")
adjustmentSets( dag_6.m4 , exposure="X" , outcome="Y" )

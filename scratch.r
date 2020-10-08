varying_points <- function(W, size, length.out=100) {

                                        # define grid
    p_grid <- seq( from=0 , to=1 , length.out=length.out )
                                        # define prior
    ## uniform prior
    ## prior <- rep( 1 , length.out )
    ## other priors
    prior <- ifelse( p_grid < 0.5 , 0 , 1)
    ## yet another
    ## prior <- exp( -5 * abs( p_grid - 0.5 ) )

                                        # compute likelihood at each value in grid
    likelihood <- dbinom( W , size=size , prob=p_grid )

                                        # compute product of likelihood and prior
    unstd.posterior <- likelihood * prior

                                        # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior / sum(unstd.posterior)

                                        # plot
    plot( p_grid , posterior , type="b" ,
     xlab="probability of water" , ylab="posterior probability" )
    mtext( paste0( W, " of ", size, " water" ) )
}

par(mfrow=c(2,3))
varying_points(3, 3)
varying_points(3, 4)
varying_points(5, 7)

## for (i in c(2^(2:7))) {
##     varying_points(i)
## }

library(rethinking)
globe.qa <- quap(
    alist(
        W ~ dbinom( W+L ,p) , # binomial likelihood
        p ~ dunif(0,1)        # uniform prior
    ) ,
    data=list(W=6,L=3)
)

## display summary of quadratic approximation
precis( globe.qa )

#' analytical calculation
quadratic_evaluation <- function(N) {
    W <- N - N / 3
    L <- N - (N / 3) * 2
    print(paste0("W: ", W))
    print(paste0("L: ", L))
    curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 )
    #' quadratic approximation
    curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )
}

for (n in c(9, 18, 36, 64)) {
    quadratic_evaluation(n)
}

## MCMC

n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
    p_new <- rnorm( 1, p[i-1] , 0.1 )
    if ( p_new < 0 ) p_new <- abs( p_new )
    if ( p_new > 1 ) p_new <- 2 - p_new
    q0 <- dbinom( W , W+L , p[i-1] )
    q1 <- dbinom( W , W+L , p_new )
    p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
}

hist( p )
dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE)

par(mfrow=c(2,3))

## vampires
pr_positive_vampire <- 0.95
pr_positive_mortal <- 0.01
pr_vampire <- 0.001
pr_positive <- pr_positive_vampire * pr_vampire +
    pr_positive_mortal * (1 - pr_vampire)
pr_vampire_positive <- pr_positive_vampire * pr_vampire / pr_positive
print(pr_vampire_positive)

## posterior of globe tossing
globe <- function( definition=10^3, sample_size=1e4 ) {
    p_grid <- seq( from=0 , to=1 , length.out=definition )
    prob_prior <- rep( 1, definition)
    prob_data <- dbinom( 6 , size=9 , prob=p_grid )
    posterior <- prob_data * prob_prior
    posterior <- posterior / sum(posterior)
    samples <- sample( p_grid , prob=posterior , size=sample_size , replace=TRUE)
    return( samples )
    ## return( sum( samples > 0.5 & samples < 0.75 ) / sample_size )
}

samples <- globe(10^3)
plot(samples)

library(rethinking)
dens( samples )


## add up posterior probability where p < 0.5
intervals <- function(definition=10^3) {
    p_grid <- seq( from=0 , to=1 , length.out=definition )
    prob_prior <- rep( 1, definition)
    prob_data <- dbinom( 6 , size=9 , prob=p_grid )
    posterior <- prob_data * prob_prior
    posterior <- posterior / sum(posterior)
    return( sum( posterior[ p_grid < 0.5 ] ))
}

quantile( samples , 0.8 )

quantile( samples , c( 0.1 , 0.9 ))

## skewed posteriors

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum( posterior )
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
plot( samples )
plot( posterior )
dens( samples )

PI( samples , prob=0.5 )
HPDI( samples , prob=0.5 )


p_grid[ which.max(posterior) ]

chainmode( samples , adj=0.01 )

mean( samples )
median( samples )

sum( posterior * abs( 0.5 - p_grid ))

loss <- sapply( p_grid , function(d) sum( posterior * abs( d - p_grid )))

plot(loss)

p_grid[ which.min(loss) ]

median(samples)

dbinom( 0:2 , size=2 , prob=0.7 )

dummy_w <- rbinom( 1e2 , size=9 , prob=0.7 )
simplehist( dummy_w , xlab="dummy water count")

w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist( w )

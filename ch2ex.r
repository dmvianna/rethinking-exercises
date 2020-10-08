library(rethinking)
#par(mfrow=c(2,3))


## samples
length.out <- 1e3
length.sample <- 1e4
p_grid <- seq( from=0 , to=1 , length.out=length.out )
prior <- rep( 1, length.out )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

set.seed(100)
samples <- sample( p_grid , prob=posterior , size=length.sample , replace=TRUE)

## 3E1
p_under = 0.2
sum( posterior[ p_grid  < p_under ] )
sum( samples < p_under ) / length.sample

## 3E2
p_over = 0.8
sum( posterior[ p_grid  > p_over ] )
sum( samples > p_over ) / length.sample

## 3E3
sum( posterior[ p_grid > p_under & p_grid < p_over ] )
sum( samples > p_under & samples < p_over ) / length.sample

## 3E4
quantile( samples, 0.2 )
## both
PI( samples, 0.6 )
## 3E5
quantile( samples, 0.8 )

## 3E6
HPDI( samples, 0.66 )

## 3M1
size <- 10e8
data <- 7 * size / 10
length.out <- 1e3
length.sample <- 1e4
p_grid <- seq( from=0 , to=1 , length.out=length.out )
## prior <- ifelse( p_grid > 0.5, 1, 0 )
prior <- rep( 1 , length.out )
likelihood <- dbinom( data , size=size , prob=p_grid )
## likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum( posterior )
samples <- sample( p_grid , prob=posterior , size=length.sample , replace=TRUE)

## 3M2
HPDI( samples, 0.9 )

## 3M3
w <- rbinom( length.sample , size=15 , prob=samples )
simplehist(w)
p <- sum(w[w == 8]) / sum(w)
p

## 3M4
## we need to transform the desired fraction to base 15,
## which is the number of tosses we originally used
## desired probability = 6 / 9 == 2 / 3
## 15 / 3 = 5
## 2 * 5 = 10
## final probability = 10 / 15

p_2 = sum(w[ w == 10 ]) / sum(w)
p_2

## 3M5
chainmode( samples , adj=0.01 )

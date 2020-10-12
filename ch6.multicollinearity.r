## load data and copy
rm(list = ls())
par(mfrow=c(2,3))
library(rethinking)

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

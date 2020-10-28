## average brain volumes and body masses
sppnames <- c( "afarensis","africanus","habilis","boisei",
              "rudolfensis","ergaster","sapiens")
brainvolcc <- c(438,452,612,521,752,871,1350)
masskg <- c(37.0,35.5,34.5,41.5,55.5,61.0,53.5)
d <- data.frame(species=sppnames,brain=brainvolcc,mass=masskg)
str(d)

d$mass_std <- (d$mass - mean(d$mass)) / sd(d$mass)
d$brain_std <- d$brain / max(d$brain)

m7.1 <- quap(
    alist(
        brain_std ~ dnorm( mu , exp(log_sigma) ) ,
        mu <- a + b*mass_std ,
        a ~ dnorm( 0.5 , 1 ) ,
        b ~ dnorm( 0 , 10 ) ,
        log_sigma ~ dnorm( 0 , 1 )
    ) , data = d
)

set.seed(12)
s <- sim( m7.1 )
r <- apply(s,2,mean) - d$brain_std
resid_var <- var2(r)
outcome_var <- var2( d$brain_std )
1 - resid_var/outcome_var

R2_is_bad <- function( quap_fit ) {
    s <- sim( quap_fit , refresh=0 )
    r <- apply(s,2,mean) - d$brain_std
    1 - var2(r)/var2(d$brain_std)
}

m7.2 <- quap(
    alist(
        brain_std ~ dnorm( mu , exp(log_sigma) ) ,
        mu <- a + b[1]*mass_std + b[2]*mass_std^2 ,
        a ~ dnorm( 0.5 , 1 ) ,
        b ~ dnorm( 0 , 10 ) ,
        log_sigma ~ dnorm( 0 , 1 )
    ) , data=d , start=list(b=rep(0,2))
)

m7.3 <- quap(
    alist(
        brain_std ~ dnorm( mu , exp(log_sigma) ) ,
        mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 ,
        a ~ dnorm( 0.5 , 1 ) ,
        b ~ dnorm( 0 , 10 ) ,
        log_sigma ~ dnorm( 0 , 1 )
    ) , data=d , start=list(b=rep(0,3))
)

m7.4 <- quap(
    alist(
        brain_std ~ dnorm( mu , exp(log_sigma) ) ,
        mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 ,
        a ~ dnorm( 0.5 , 1 ) ,
        b ~ dnorm( 0 , 10 ) ,
        log_sigma ~ dnorm( 0 , 1 )
    ) , data=d , start=list(b=rep(0,4))
)

m7.5 <- quap(
    alist(
        brain_std <- dnorm( mu , exp(log_sigma) ) ,
        mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 +
            b[5]*mass_std^5 ,
        a ~ dnorm( 0.5 , 1 ) ,
        b ~ dnorm( 0 , 10 ) ,
        log_sigma ~ dnorm( 0 , 1 )
    ) , data=d , start=list(b=rep(0,5))
)

m7.6 <- quap(
    alist(
        brain_std <- dnorm( mu , 0.001 ) ,
        mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 +
            b[4]*mass_std^4 + b[5]*mass_std^5 + b[6]*mass_std^6 ,
        a ~ dnorm( 0.5 , 1 ) ,
        b ~ dnorm( 0 , 10 )
    ) , data=d , start=list(b=rep(0,6))
)

plot_this <- function( model ) {
    post <- extract.samples( model )
    mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
    l <- link( model , data=list(mass_std=mass_seq) )
    mu <- apply( l , 2 , mean )
    ci <- apply( l , 2 , PI )
    plot( brain_std ~ mass_std , data=d )
    lines( mass_seq , mu )
    shade( ci , mass_seq )
}

par(mfrow=c(3,2))
for ( m in c(m7.2,m7.3,m7.4,m7.5,m7.6) ) plot_this(m)

## earth vs mars
## we want to know if the tossed globe is earth

## planet prior likelihood
planets <- data.frame(
    planet=c( "earth" , "mars" ),
    prior=c( 0.3 , 1 ),
    evidence=c( 1 , 1 )
)

planets["unnormalised.posterior"] = planets["prior"] * planets["evidence"]
planets["posterior"] = planets["unnormalised.posterior"] / sum(planets["unnormalised.posterior"])
print(planets)

## cards

cards <- data.frame(
    card=c("bb","ww","bw"),
    prior=c(1,0,0.5),
    evidence=c(1,0,1)
    )
cards['unnormalised'] <- cards["prior"] * cards["evidence"]
cards["posterior"] <- cards["unnormalised"] / sum(cards["unnormalised"])
print(cards)

## more cards
cards <- data.frame(
    card=c("bb","ww","bw"),
    prior=c(4,0,1),
    evidence=c(1,0,1)
    )
cards['unnormalised'] <- cards["prior"] * cards["evidence"]
cards["posterior"] <- cards["unnormalised"] / sum(cards["unnormalised"])
print(cards)

## heavy cards

cards <- data.frame(
    card=c("bb","ww","bw"),
    prior=c(1,0,0.5),
    weight=c(1,3,2),
    evidence=c(1,0,1)
    )
cards['unnormalised'] <- cards["prior"] * cards["evidence"] * cards["weight"]
cards["posterior"] <- cards["unnormalised"] / sum(cards["unnormalised"])
print(cards)


## two cards (still don't get it)

# 1. resolve the original problem assigning equal priors to the cards
cards <- data.frame(
    card=c("bb","ww","bw"),
    prior=c(1,1,1),
    evidence=c(2,0,1),
    )
cards['unnormalised'] <- cards["prior"] * cards["evidence"]
cards["posterior"] <- cards["unnormalised"] / sum(cards["unnormalised"])
print(cards)


# 2. add the new card
cards <- data.frame(
    card=c("bb","ww","bw"),
    prior=c(1,1,1),
    ## compatible options with fist card bb:
    ## Bb Ww Bw
    ## Bb wW Bw
    ## Bb wB Ww
    ## Bb wB wW
    ## bB Ww Bw
    ## bB Ww wB
    ## compatible options with the first card Bw:
    ## Bw Ww Bb
    ## Bw wW Bb
    ## Bw Ww bB
    ## Bw wW bB
    evidence=c(3*2,0,1*2)
    )
cards['unnormalised'] <- cards["prior"] * cards["evidence"]
cards["posterior"] <- cards["unnormalised"] / sum(cards["unnormalised"])
print(cards)


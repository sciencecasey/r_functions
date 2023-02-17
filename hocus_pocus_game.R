
vals= c(2:10, 'J', 'K', 'Q', 'A')
suit = c(rep('heart', 13), rep('spade', 13), rep('clover', 13), rep('diamond', 13))
data = rep(vals, 4)
data = cbind(data, suit)
dealt = sample(1:52, 21)
showcase = data[dealt,]

# indx = round(runif(1, 1, 21))
indx = 4
card = showcase[indx,]
card

hocus_pocus_round = function(indx, showcase){
    if(indx>14){
        in_pile= 3
    }else if(indx>7){
        in_pile = 2
    }else{
        in_pile = 1
    }
    in_pile*1:7
    not_piles = c(1,2,3)
    not_piles = not_piles[not_piles != in_pile]

    # make a new deck to put into piles
    dealing = showcase
    dealing[8:14,] = showcase[(in_pile*7):((in_pile*7)-6),]
    dealing[1:7, ]  = showcase[(not_piles[1]*7):((not_piles[1]*7)-6),]
    dealing[15:21, ]  = showcase[(not_piles[2]*7):((not_piles[2]*7)-6),]

    # function to deal into piles in order
    pile = function(x){
        seq(x, 21, 3)
    }

    # put back into "3" piles of 7
    showcase[1:7,] = dealing[pile(1),]
    showcase[8:14,] = dealing[pile(2),]
    showcase[14:21,] = dealing[pile(3),]
    return(showcase)

}

showcase = hocus_pocus_round(indx, showcase = showcase)

indx = which(showcase==card)[1]
showcase[indx,]
showcase = hocus_pocus_round(indx, showcase)
indx = which(showcase==card)[1]
showcase[indx,]
showcase = hocus_pocus_round(indx, showcase)
indx = which(showcase==card)[1]
showcase[indx,]
showcase = hocus_pocus_round(indx, showcase)
indx = which(showcase==card)[1] ## Index = 10
if(indx == 10){
    print("hocus pocus")
    showcase[indx,]
}


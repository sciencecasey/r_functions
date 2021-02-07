die = c(1:6)
roll = function(){
  dice = sample(die, size = 2, replace = TRUE)
  sum(dice)
}
roll()
#test if the die is fair
rolls = replicate(10000, roll())
library(ggplot2)
qplot(rolls, binwidth = 1)

roll_weighted = function(){
  dice = sample(die, size = 2, replace = TRUE, 
                prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}
roll()
rolls = replicate(10000, roll_weighted())
qplot(rolls, binwidth = 1)
(rolls = expand.grid(die, die))
#add a sum row
rolls$value = rolls$Var1 + rolls $Var2

#add a probability vector
prob = c(rep(1/8, 5), 3/8) #weighted values
names(prob) = c(1:6) #name the values
attributes(prob)
#lookup the probabilities based on the names listed in Var1
prob[rolls$Var1]
rolls$prob1 = prob[rolls$Var1]
rolls$prob2 = prob[rolls$Var2]
rolls$Prob = rolls$prob1 * rolls$prob2

#calulate expected value
eWeightedDie = sum(rolls$value * rolls$Prob)

#same thing with the slot machine!
#create psuedo random probability dist for outputting symbols
get_symbols= function(){
  wheel = c("D","7","BBB","BB","B","C","0")
  #take a sample of the wheel sample using probability listed
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(.03, .03, .06, .1, .25, .01, .52))
}

wheel = c("D","7","BBB","BB","B","C","0")
prob = c(.03, .03, .06, .1, .25, .01, .52)
combos = expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos
names(prob) = wheel
prob
combos$prob1 = prob[combos$Var1]
combos$prob2 = prob[combos$Var2]
combos$prob3 = prob[combos$Var3]
combos$Prob = combos$prob1*combos$prob2*combos$prob3
head(combos)
sum(combos$Prob) #should be 1!!

#add a scores row
score <- function(symbols) {
  diamonds <- sum(symbols == "D")
  cherries <- sum(symbols == "C")
  
  # identify case
  # since diamonds are wild, only nondiamonds 
  # matter for three of a kind and all bars
  slots <- symbols[symbols != "D"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries
    # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  
  # double for each diamond
  prize * 2^diamonds
}

score(paste(unname(combos[1,1:3])))
combos$score = score(paste(unname(combos[,1:3])))  #doesn't work!
head(combos)
score(c(combos[1,1], combos[1,2], combos[1,3]))
combos$score = score(c(combos[,1], combos[,2], combos[,3])) #also doesn't work!
head(combos)

#Use a For loop!!
output <- vector(length = 4)
words <- c ("hello", "my", "old", "friend")
for(i in 1:4){
  output[i] <-words[i]
}
output

for(i in 1:nrow(combos)){
  grab = c(combos[i,1], combos[i,2], combos[i,3])
  combos$score[i] = score(grab)
}
head(combos)

#expected value 
sum(combos$score * combos$Prob)

plays_left = function(startingAmt){
  cash = startingAmt
  n = 0
  while(cash > 0){
    cash = cash - 5
    n = n+1
  }
  return(n)
}

plays_left(100)

#same but using a repeat
plays_left = function(startingAmt){
  cash = startingAmt
  n = 0
  repeat{
    cash = cash -5
    if(cash<=0){
      break
    }
  }
  return(n)
}



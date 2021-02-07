#From: https://rstudio-education.github.io/hopr/s3.html
#Machine Rules:: 
##win if gt 3 of same type (not zero)
##win if every item is a bar (any type) -- we use Bs for bars
##win if get one or more cherries- we use C for cherries
##Diamonds (D) are wild AND if it appears in a combo doubles the prize
##Jackpot for 3 diamonds 

#create psuedo random probability dist for outputting symbols
get_symbols= function(){
  wheel = c("D","7","BBB","BB","B","C","0")
  #take a sample of the wheel sample using probability listed
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(.03, .03, .06, .1, .25, .01, .52))
}

#create the play with prizes embedded using ouput of get_symbols

play = function(){
  roll = get_symbols()
  print(roll)
  score(roll)
}

score = function(symbols){
  same = all(symbols == symbols[1]) #boolean
  bars = all(symbols %in% c("B", "BB", "BBB")) #boolean
  wild = any(symbols == "D")
  wildBar = all(symbols %in% c("B", "BB", "BBB", "D"))
  
  #determine case
  if(same){
    #three of a kind
    payouts <- c("D" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize = unname(payouts[symbols[1]]) #lookup in a table
  }else if(length(unique(symbols)) == 2 && wild){
    #two symbols match with at least one diamond 
    kind = which(symbols!="D")
    payouts <- c("D" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize = unname(payouts[kind[1]]) #lookup in a table
  }else if(bars){
    #all bars
    prize = 5
  }else if(wildBar){
    #all bars including at least one wild
    prize = 5 #assign to wild (diamonds accounted for below)
  }else{
  #no matches
    cherries = (sum(symbols == "C"))
    payouts = c(0, 2, 5)
    prize = payouts[cherries + 1]
  }
  
  #adjust for diamonds
  diamonds = sum(symbols == "D")
  return(prize*2^diamonds) #return the prize
} #end of score function


#Adding Attributes/saving a game
one_play = play()
attributes(one_play) #starts out Null
#add symbols attr (making them up)
attr(one_play, "symbols") = c("B", "0", "B")
attr(one_play, "symbols") #checking that it worked
attributes(one_play)
one_play+1 
attributes(one_play) #unchanged even though value of one_play changed


#Add attribute to a function
play = function(){
  roll = get_symbols()
  prize = score(roll)
  attr(prize, "prize") = prize
  attr(prize, "symbols") = roll
  return(prize)
}
one_play = play()
one_play
two_play = play()
two_play
#using the structure function
play = function(){
  roll = get_symbols()
  prize = score(roll)
  structure(prize, "prize" = prize, "symbols" = roll)
}
three_play = play()
three_play

#using the attribute
slot_display = function(playObj){
  #look up symbols attribute
  symbols = attr(playObj, "symbols")
  prize = attr(playObj, "prize")
  #collapse into a string
  symbols = paste(symbols, collapse = " ")
  #combine symbol with prize as a sentence in a new line
  string = paste(symbols, prize, sep = "\n$")
  #read into console without quotation marks
  cat(string)
}

slot_display(three_play)
slot_display(two_play)
slot_display(one_play)
#to run
slot_display(play())

#or change the play() function directly
play = function(){
  roll = get_symbols()
  prize = score(roll)
  prizeObj = structure(prize, "prize" = prize, "symbols" = roll)
  slot_display(prizeObj)
}
play()


#Making Generic S3 method: 
#first make a class for our type of object
class(one_play) = "slots" #this makes a class type slots with the attr of one_play

#now create a generic function of print() to read this type
print.slots = function(x, ...){ #the function must look like the original generic
  slot_display(x)
}

play = function(){
  roll = get_symbols()
  prize = score(roll)
  prizeObj = structure(prize, "prize" = prize, "symbols" = roll, class = "slots")
  print(prizeObj)
}
play()
print(one_play)
print(two_play) #doesn't work for this as it's unclassed

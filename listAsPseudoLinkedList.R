#list as linked list doesn't actually work! 
#can't setNext without going down the whole list
link = list()
link[[1]] = "first"
link[[2]] = 2
link = list(1,2,3,4,5)
link = link[-2]
link[2] = 8 #overwrites the item

getNext = function(list, index){
  return(list[[index+1]])
}
getHead = function(list){
  return(list[[1]])
}
swapPoint = function(list, pointer1, pointer2){
  temp1 = list[[pointer1]]
  list[[pointer1]] = list[pointer2]
  list[[pointer2]] = temp1
  return(list)
}
delete = function(list, pointer){
  list = list(-pointer)
  return(list)
}
getTail = function(list){
  return(list[[length(list)]])
}
setTail = funtions(list, value){
  index = length(list)+1
  list[index] = value
  return(list)
}
setNext = function(list, value, after){
  c(list[1:(after)], value, list[after+1:(length(list)-after)])
}


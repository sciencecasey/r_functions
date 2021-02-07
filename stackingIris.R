# Stack functions using lists 
stack = function(){
  return(list())
}
push = function(stack, value){
  if(length(value)>1){
    stack = append(stack, value[1], after = 0)
    value = value[-1] #remove the pushed item
    return(push(stack, value)) #recursive call
  }else{
    append(stack, value, after = 0)
  }
}
pop = function(stack){
  if(length(stack)>0){
    stack[-1]
  }else{
    cat(paste("Stack underflow"))
  }
}
tos = function(stack){
  return(stack[1])
}
isEmpty = function(stack){
  if(length(stack) == 0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

sepals = stack()
#save desired items in a vector
sepalLength = iris$Sepal.Length
#push all onto stack
sepals = push(sepals, sepalLength)

#ensuring they were pushed correctly
tail(sepalLength)
head(sepals)
head(sepalLength)
tail(sepals)

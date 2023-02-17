hash = function(list, key, value){
  if (is.null(list[[key]])){
    #if no item at location
    list[[key]] = value
    cat(paste("inserted ", value, " at ", key))
    return(list)
  } else{
    #resolve collisions with chaining
    list[[key]] = c(list[[key]], value)
    cat(paste("appended ", value,  " at ", key))
    return(list)
  }
}

hashIris = vector("list", 150) #allocate the list we will hash into
for(item in iris$SepalLength){
  hashIris = hash(list = hashIris, key = item, value = iris$SepalLength[item])
  #item = item + 1
  cat(paste("pass number ", item))
}

# hashIris = hash(hashIris, 1, iris$Sepal.Length[1])
# iris$Sepal.Length[1]
# hashIris = hash(hashIris, 1, "test")
# hashIris[[1]]
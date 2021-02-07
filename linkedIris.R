library(R6)

# create a simple node class of data
node <- R6Class("node", 
                public = list(
                  data = NULL, nextNode = NULL,
                  initialize = function(data = NULL){
                    self$data <- data}
                )
)

# create a linked list of nodes
linkedList <- R6Class("linkedList", 
                      public = list(
                        head = NULL, tail = NULL, 
                        initialize = function(value = NULL){
                          if(!is.null(value)){
                            self$head <- node$new(data = value)
                            self$tail <- self$head
                          }
                        },
                        append = function(value){
                          if(is.null(self$head)){
                            #empty list
                            #append to front
                            self$head <- node$new(data = value)
                            self$tail <- self$head
                          }else if(is.null(self$head$nextNode)){
                            #head and tail the same
                            self$tail <- node$new(data = value)
                            self$head$nextNode <- self$tail
                          }else{
                            temp <- node$new(value)
                            self$tail$nextNode <- temp
                            self$tail <- temp
                          }
                        },
                        remove = function(value){
                          item = pointerTo(value)
                          if(is.null(item)){
                            #not in list
                            cat(paste("Item not in list"))
                          }else if(item == self$head){
                            #remove the head
                            if(is.null(self$head$nextNode)){
                              #head only thing in list
                              self$head = null
                              self$tail = null
                            }else{
                              self$head = self$head$nextNode
                            }
                          }else{
                            #not the head
                            prev = self$head
                            #get the node before deleted item
                            while(prev$nextNode != item){
                              prev = prev$nextNode
                            }
                            #remove pointer to item
                            prev$nextNode = item$nextNode
                            #remove item's next to detach from list fully
                            item$nextNode = NULL
                            if(is.null(prev$nextNode)){
                              #item was tail, reset tail pointer
                              self$tail = prev
                            }
                          }
                        }, 
                        pointerTo = function(value){
                          if(is.null(self$head)){
                            return(NULL)
                          }else if(self$head$data == value){
                            return(self$head)
                          }else{
                            temp <- self$head$next
                            while(temp != null){
                              if(temp$data == value){
                                break
                              }
                              temp = temp$nextNode
                            }
                            return(temp)
                          }
                        },
                        printList = function(){
                          if(is.null(self$head)){
                            cat(paste("List is empty"))
                          }else{
                            tempNode = self$head
                            while(!is.null(tempNode)){
                              cat(paste(tempNode$data, "\n"))
                              tempNode = tempNode$nextNode
                            }
                          }
                        }
                      ),
)

# read.csv(iris.csv) -- unnecessary, built-in dataset

# initialize empty list
linkedIris = linkedList$new()
#linkedIris$head #null value
for(i in iris$Sepal.Length){
  linkedIris$append(i)
}

#print to terminal if applicable
cat(paste("\nLinked list of all sepal lengths created\n"))

#ensure all data populated correctly
tempNode = linkedIris$head
i = 1
while(!is.null(tempNode)){
  if(tempNode$data != iris$Sepal.Length[i]){
    #print out any discrepancies between original data and list
    cat(paste("Didn't work \n"))
    print(tempNode$data)
    print(iris$Sepal.Length[i])
  }
  #move to next node and next item in iris$Sepal.Length
  tempNode = tempNode$nextNode
  i = i+1
}
#woohoo! it's the same!

# checking that it worked
# linkedIris
# linkedIris$head
# linkedIris$head$nextNode
# linkedIris$head$nextNode$nextNode
# linkedIris$head$nextNode$nextNode$nextNode
# linkedIris$tail
# #make sure they're the same
# iris[1,1]
# tail(iris[1])

# print the list
tempNode = linkedIris$head
cat(paste("\nListing all sepal lengths, in order: \n"))
total = 0
while(!is.null(tempNode)){
  cat(paste(tempNode$data, "\n"))
  tempNode = tempNode$nextNode
  total = total +1
}

cat(paste("\nTotal number of observations is", total, "\n"))

#playing around to see what I can do with them
node1 = node$new(1)
node2 = node$new(2)
node3 = node$new("hello")
nodeArray = c(node1, node2, node3) #can put into lists
nodeList = c(node3, node2, node1)
#nodeDF = data.frame(nodeArray, nodeList) #can't put into dataframe or matrix
rbind(nodeArray, c(7,8,9)) #you can create a list of lists 
library(tidyverse)
#nodeMat = matrix(nodeArray, nodeList) -- just creates another list of lists
nodeTib = tibble(nodeArray, nodeList)
#get information about the Tibble
str(nodeTib)
colnames(nodeTib)
rownames(nodeTib)
#ways to look up the self-created feature, data within a list or array
nodeTib$nodeArray[[3]]$data
nodeArray[[3]]$data
nodeList[[3]]$data



#trying another way using lists
linky = function(){
  return(list())
}
linkAppend = function(list, value){
  if(length(value)>1){
      list = append(list, value[1])
      value = value[-1] #drop the added item
      return(linkAppend(list, value)) #recursive call
  }else{
    append(list, value)
  }
}
linkPrepend = function(list, value){
  if(length(value)>1){
    list = append(list, value[1], after = 0)
    value = value[-1] #drop the added item
    return(linkPrepend(list, value)) #recursive call
  }else{
    append(list, value, after = 0)
  }
}
#using index-- doesn't work quite the same
setNext = function(list, value, afterIndex){
  append(list, value, after = afterIndex)
}
#using index doesn't work quite the same
getNext = function(list, afterIndex){
  list[afterIndex+1]
}
getHead = function(list){
  list[1]
}
getTail = function(list){
  list[length(list)]
}

linkySepal = linky()
linkySepal = linkAppend(linkySepal, iris$Sepal.Length)
#getHead(linkySepal)
#getTail(linkySepal)

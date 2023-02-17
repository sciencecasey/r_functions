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

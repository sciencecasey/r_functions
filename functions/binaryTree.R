#create a binary tree in which to place the evaluation values
library("R6")
binNode = R6Class("binNode", public = list(data = NULL, 
                                           parent = NULL,
                                           left = NULL,
                                           right = NULL,
                                           initialize = function(value){
                                             #' @param value a value to store in the current node
                                             #' creates a node without connecting it to a tree
                                             self$data = value
                                           },
                                           hasChild = function(){
                                             if(is.null(self$left)){
                                               if(is.null(self$right)){
                                                 return(FALSE)
                                               }
                                             }else{
                                               return(TRUE)
                                             }
                                           },
                                           hasLeft = function(){
                                             return(!is.null(self$left))
                                           },
                                           hasRight = function(){
                                             return(!is.null(self$right))
                                           }))

binTree = R6Class("binTree", public = list(root = NULL,
                                         initialize = function(value){
                                           #' create a binary tree from a single value or vector of values
                                           #' @param value a numeric value or vector of numeric types
                                           if(length(value) == 1){
                                             self$root = binNode$new(value)
                                           }else{
                                             i = 2
                                             self$root = binNode$new(value[1])
                                             while(i<=length(value)){
                                              self$insert(value[i])
                                              i = i+1
                                             }
                                           }
                                         },
                                         isEmpty = function(){
                                           #' @return boolean if the tree has any nodes
                                           if(is.null(self$root)){
                                             return(TRUE)
                                           }else{
                                             return(FALSE)
                                           }
                                           },
                                         inOrderWalk = function(start = self$root){
                                           #' recursive function that moves through the binary tree and 
                                           #' prints in order of value
                                           #' @param start default proceeds from root can be changed by user
                                           if(is.null(start)){
                                             return(start)
                                           }else {
                                             self$inOrderWalk(start$left) 
                                             cat(paste(start$data), "\t")
                                             self$inOrderWalk(start$right)
                                           }
                                         },
                                         search = function(fromNode = self$root, key){
                                           #' search for the value within the tree
                                           #' @param fromNode default begin from root but can be overridden by user
                                           #' @param key a numeric value to find within the tree
                                           #' @return the tree node containing the value or a NULL object if not in tree
                                           if(is.null(fromNode) || fromNode$data == key){
                                             return(fromNode)
                                           }else if(fromNode$data > key){
                                             # looking for a smaller value, move left
                                             fromNode = self$search(fromNode$left, key)
                                           }else{
                                             fromNode = self$search(fromNode$right, key)
                                           }
                                         },
                                         getMin = function(fromNode = self$root){
                                           #' @param fromNode the minimum value in subtree from the select node
                                           #' @return the smallest node within the subtree
                                           while(!is.null(fromNode$left)){
                                             fromNode = fromNode$left
                                           }
                                           return(fromNode)
                                         },
                                         getMax = function(fromNode = self$root){
                                           #' @param a node to use as root to find max within the subtree
                                           #' @return the node with the largest value in the subtree
                                           while(!is.null(fromNode$right)){
                                             fromNode = fromNode$right
                                           }
                                           return(fromNode)
                                         },
                                         getSuccessor = function(node){
                                           #' @param node the node for whom to find successor (next larger value)
                                           #' @return the node with next largest value if there is one
                                           if(!is.null(node$right)){
                                             # get the smallest value in the right subtree
                                             return(self$getMin(node$right))
                                           }else if(!is.null(node$parent) && identical(node, node$parent$left)){
                                             #a left child & leaf node, the parent is next larger value
                                             return(node$parent)
                                           }else{
                                             #check if max value
                                             maxie = self$getMax()
                                             if(identical(node, maxie)){
                                               #largest value in tree
                                               cat(paste("No successor found", "\n"))
                                               return(NULL)
                                             }
                                             while(!is.null(node$parent) && identical(node, node$parent$right)){
                                               # move up the parents who are right children
                                               node = node$parent
                                             }
                                             return(node)
                                           }
                                         },
                                         getPredecessor = function(fromNode){
                                           #' @param fromNode the node to get the predecessor of
                                           #' @return the node who is the predecessor (next smallest value)
                                           if(!is.null(fromNode$left)){
                                             fromNode = self$getMax(fromNode$left)
                                             return(fromNode)
                                           }else if(!is.null(fromNode$parent) && identical(fromNode, fromNode$parent$right)){
                                              #a right child and leaf node, parent is the predecessor 
                                              return(fromNode$parent)
                                           }else{
                                             #check if min value
                                             minnie = self$getMin()
                                             if(identical(fromNode, minnie)){
                                               #smallest value in tree
                                               cat(paste("No predecessor found", "\n"))
                                               return(NULL)
                                             }
                                             while(!is.null(fromNode$parent) && identical(fromNode,fromNode$parent$left)){
                                               # move up the parents who are left children
                                               fromNode = fromNode$parent
                                             }
                                             return(fromNode)
                                           }
                                         },
                                         insert = function(value){
                                           #' @param value a numeric value to add to the tree
                                           node = self$root
                                           parental = NULL
                                           while(!is.null(node)){
                                             parental = node
                                             if(value<node$data){
                                               node = node$left
                                             }else{
                                               node = node$right
                                             }
                                           }
                                           newb = binNode$new(value)
                                           #set parent pointer
                                           newb$parent = parental
                                           #set the new node as correct child
                                           if(is.null(parental)){
                                             #new node is the root
                                             self$root = newb
                                           }else if(value<parental$data){
                                             parental$left = newb
                                           } else{
                                             parental$right = newb
                                           }
                                         },
                                         delete = function(node){
                                           #' delete a select node from the tree
                                           #' @param node the node to delete
                                           if(!node$hasChild()){
                                             #direct delete, no children
                                             if(identical(node, self$root)){
                                               self$root = NULL
                                             } else if(identical(node, node$parent$left)){
                                               node$parent$left = NULL
                                               node$parent = NULL
                                             } else{
                                               node$parent$right = NULL
                                               node$parent = NULL
                                             }
                                           }else if(!node$hasLeft()){
                                             #only right child, switch them
                                             private$swap(node, node$right)
                                           }else if(!node$hasRight()){
                                             #only left child
                                             swap(node, node$left)
                                           }else{
                                             #2 children, get successor
                                             succ = self$getMin(node$right)
                                             if(!identical(succ$parent, node)){
                                               #not a direct child
                                               private$swap(succ, succ$right)
                                               succ$parent = node$parent
                                               #set right child
                                               succ$right = node$right
                                               succ$right$parent = succ
                                             }
                                             private$swap(node, succ)
                                             #set left child
                                             succ$left = node$left
                                             succ$left$parent = succ
                                           }
                                         }
                                         # print function doesn't work yet,
                                         # print = function(node = self$root){
                                         #   cat(paste(node$data))
                                         #   if(is.null(node$parent)){
                                         #     #print new line after
                                         #     cat(paste("\n"))
                                         #   }else{
                                         #     temp = node
                                         #     while(identical(temp, temp$parent$right)){
                                         #       temp = temp$parent
                                         #       if(identical(temp, self$root)){
                                         #         #end of full level when direct right side of root
                                         #         cat(paste("\n"))
                                         #         break
                                         #       }
                                         #     }
                                         #     #tabs between nodes
                                         #     cat(paste("\t\t"))
                                         #   }
                                         #   #print next row
                                         #   if(!is.null(node$left)){
                                         #     self$print(node$left)
                                         #   }
                                         #   if(!is.null(node$right)){
                                         #     self$print(node$right)
                                         #   }
                                         # }
                                         ), private = list(
                                           swap = function(u,v){
                                             #' an internal function to swap the values of v with u and leaves u unattached
                                             #' used within a delete method
                                             #' @param u a node to detach
                                             #' @param v a node to put into u's place
                                             temp = v
                                             if(identical(u, self$root)){
                                               self$root = v
                                             }else if(identical(u, u$parent$left)){
                                               #left child
                                               u$parent$left = v
                                             }else{
                                               u$parent$right = v
                                             }
                                             v$parent = u$parent
                                             #u is now unattached
                                             # #switch u into v's spot
                                             # if(temp = self$root){
                                             #   self$root = u
                                             # }else if(temp == temp$parent$left){
                                             #   temp$parent$left = u
                                             # }else{
                                             #   temp$parent$right = u
                                             # }
                                             # u$parent = temp$parent
                                           }
                                         )
                  )
# try = binTree$new(c(5,3,9,2,8,12,-1,3,18))
# try$root
# try$getMin()
# try$getMax()
# minnie = try$getMin()
# try$getSuccessor(minnie)
# try$getPredecessor(minnie) 
# try$getSuccessor(try$root) 
# try$getPredecessor(try$root)
# maxie = try$getMax()
# try$getPredecessor(maxie) #this works
# try$getSuccessor(maxie) #didn't work, attempt to apply non-function
# try$getSuccessor(maxie$parent)
# try$getPredecessor(maxie$parent)
# try$inOrderWalk()
# #try$print()
# found = try$search(key = 9)
# found
# try$delete(found)
# try$inOrderWalk()
# try$delete(try$root)
# try$inOrderWalk()
# try$root

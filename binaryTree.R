#create a binary tree in which to place the evaluation values
library("R6")
binNode = R6Class(binNode, public = list(data = NULL, 
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
                                                 return(TRUE)
                                               }
                                             }else{
                                               return(FALSE)
                                             }
                                           },
                                           hasLeft = function(){
                                             return(!is.null(self$left))
                                           },
                                           hasRight = function(){
                                             return(!is.null(self$right))
                                           }))

binTree = R6Class(binTree, public = list(root = NULL,
                                         initialize = function(value){
                                           #' create a binary tree from a single value or vector of values
                                           #' @param value a numeric value or vector of numeric types
                                           if(length(value) == 1){
                                             self$root = binNode$new(value)
                                           }else{
                                             i = 1
                                             self$root = binNode$new(value[1])
                                             while(i<=length(value)){
                                              self$insert(value[i])
                                              i = i+1
                                             }
                                           }
                                         },
                                         isEmpty = function(){
                                           #' @return boolean if the tree has any nodes
                                           return(!is.null(self$root))
                                           },
                                         inOrderWalk = function(start = self$root){
                                           #' recursive function that moves through the binary tree and 
                                           #' prints in order of value
                                           #' @param start default proceeds from root can be changed by user
                                           if(is.null(start)){
                                             return(start)
                                           }else {
                                             inOrderWalk(start$left) 
                                             cat(paste(start$data))
                                             inOrderWalk(start$right)
                                           }
                                         },
                                         search(fromNode = self$root, key){
                                           #' search for the value within the tree
                                           #' @param fromNode default begin from root but can be overridden by user
                                           #' @param key a numeric value to find within the tree
                                           #' @return the tree node containing the value or a NULL object if not in tree
                                           if(is.null(fromNode) || fromNode$data == key){
                                             return(fromNode)
                                           }else if(fromNode$data > key){
                                             # looking for a smaller value, move left
                                             return(fromNode$left, key)
                                           }else{
                                             return(fromNode$right, key)
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
                                         getMox = function(fromNode = self$root){
                                           #' @param a node to use as root to find max within the subtree
                                           #' @return the node with the largest value in the subtree
                                           while(!is.null(fromNode$right)){
                                             fromNode = fromNode$right
                                           }
                                           return(fromNode)
                                         },
                                         getSuccessor = function(fromNode){
                                           #' @param fromNode the node for whom to find successor (next larger value)
                                           #' @return the node with next largest value
                                           if(!is.null(fromNode$right)){
                                             # get the smallest value in the right subtree
                                             return(getMin(fromNode$right))
                                           }else{
                                             while(!is.null(fromNode$parent) && fromNode = fromNode$parent$right){
                                               # move up the parents who are right children
                                               fromNode = fromNode$parent
                                             }
                                             return(fromNode)
                                           }
                                         },
                                         getPredecessor = function(fromNode){
                                           #' @param fromNode the node to get the predecessor of
                                           #' @return the node who is the predecessor (next smallest value)
                                           if(!is.null(fromNode$left)){
                                             return(getMax(fromNode$left))
                                           }else{
                                             while(!is.null(fromNode$parent) && fromNode = fromNode$parent$left){
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
                                           if(parental = NULL){
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
                                             if(node == self$root){
                                               self$root = NULL
                                             } else if(node == node$parent$left){
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
                                             if(succ$parent != node){
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
                                         ), private = list(
                                           swap(u,v){
                                             #' an internal function to swap the values of v with u and leaves u unattached
                                             #' used within a delete method 
                                             #' @param u a node to detach
                                             #' @param v a node to put into u's place
                                             temp = v
                                             if(u = self$root){
                                               self$root = v
                                             }else if(u == u$parent$left){
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
                                         ))
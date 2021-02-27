#library(methods)
library("R6")
game_board = R6Class("game_board", 
                     #' create a game board for tix tac toe and initialize to empty (NA) values
                     #' has public methods take(), getBoard(), getEmpties(), and clearBoard()
                        public = list(
                          take = function(player, row, col){
                            #' the character string(player) to place into row and column on the board
                            #' @param player "X" or "O" for the player moving
                            #' @param row the index to move
                            #' @param col the index to move the player into
                            if(row>3 || row<0 || col>3 || col<0){
                              cat(paste("Invalid space, locations index only 1:3"))
                              self$getBoard()
                            }else if(private$empties[row,col]){
                              private$empties[row, col] = FALSE
                              private$locations[row, col] = player
                              self$getBoard()
                            }else{
                              cat(paste("Invalid space, already filled"))
                              self$getBoard()
                            }
                          },
                          getBoard = function(){
                            #' returns the board with current characters
                            return(private$locations)
                          },
                          getEmpties = function(){
                            #' return a boolean matrix with free spaces represented as TRUE
                            return(private$empties)
                          },
                          clearBoard = function(){
                            #' clear the characters "X" and "O" from the board opening up all positions
                            private$empties = matrix(data = rep(TRUE, 9),
                                                     nrow = 3, ncol = 3)
                            private$locations = matrix(nrow = 3, ncol = 3) 
                          }),
                        private = list(
                          locations = matrix(nrow = 3, ncol = 3), #the board itself
                          empties = matrix(data = rep(TRUE, 9), #a matrix keeping track of open spaces
                                           nrow = 3, ncol = 3)
                          )
)


tic_tac_toe = R6Class("ticTacToe", public = list(
                                      board = game_board$new(),
                                      initialize = function(){
                                        #'creates a new tic tac toe game with a public board and options to moveX(), moveO(), get winning(), or get winner()
                                        #'relies on the R6 class game_board
                                        self$board$clearBoard
                                      },
                                      moveX = function(row = NULL, col = NULL){
                                        #' either give a distinct location for X to move or allow it to move according to priority
                                        #' @param row an integer index for row to move
                                        #' @param col an integer index for col to move
                                        if(is.null(row)){
                                          spot = private$getPriority()
                                          print(spot)
                                          self$board$take("X", spot[1], spot[2])
                                        }
                                        else{
                                          self$board$take("X", row, col)
                                        }
                                      },
                                      moveO = function(row, col){
                                        #' @param row the row position to move as integer
                                        #' @param col the column position to move as integer
                                        self$board$take("O", row, col)
                                      },
                                      winning = function(){
                                        #' get the character in the lead and by how much
                                        #' @return the current score as integer
                                        score = private$getScore(self$board$getBoard())
                                        if(score == Inf || score == -Inf){
                                          winner(score)
                                          return #exit the function
                                        }
                                        if(score == 0){
                                          cat(paste("Tied!"))
                                        }else if(score>0){
                                          cat(paste("X is winning by ", score))
                                        }else{
                                          cat(paste("O is winning by ", -score))
                                        }
                                        return(score)
                                      },
                                      winner = function(score){
                                        #' a method that states the winner at current stat
                                        #' @param score the current score
                                        #' @return a string message of winner and current board
                                        if(score>0){
                                          cat(paste("X wins!"))
                                        }else{
                                          cat(paste("O wins!"))
                                        }
                                        cat(paste(self$board$getBoard()))
                                      },
                                      clear = function(){
                                        self$board$clearBoard()
                                        self$board$getBoard() #print the board
                                      }), 
                                  private = list(
                                      getPriority = function(){
                                          #' get the next most optimal position for X to move
                                          #' @return the index where X should move
                                          options = self$board$getEmpties()
                                          if(options[2,2]){
                                            #middle is highest priority
                                            return(c(2,2)) #return middle place
                                          }else{
                                            corners = list(c(1,1), c(1,3), c(3,1), c(3,3))
                                            for(item in  corners){
                                              if(options[item[1]][item[2]]){
                                                #take a corner space
                                                return(c(item[1], item[2])) #breaks out of method early
                                              }
                                            }
                                            #no corner space, take any available
                                            i = 1
                                            while(i<4){
                                              j = 1
                                              while(j<4){
                                                if(options[i,j]){
                                                  return(i, j)
                                                }
                                                j = j+1
                                              }
                                              i = i+1
                                            }
                                          }
                                          #should never get here
                                          cat(paste("No available space found"))
                                        },
                                      getScore = function(locations){
                                        #' an internal method to calculate the current score (called by winning function)
                                        #' @param locations a tic tac toe board
                                        #' @return a positive score if X is leading, negative score if O is leading, and 0 if it's a tie
                                        scores = c(0, 0)
                                        #calculate the score across each rows
                                        row <- 1
                                        while(row<=3){
                                          lineX = sum(locations[row,] == "X", na.rm = TRUE)
                                          lineO = sum(locations[row,] == "O", na.rm = TRUE)
                                          scores = private$sumLine(lineX, lineO, scores)
                                          row = row+1
                                        }
                                        
                                        #calculate score by columns
                                        col = 1
                                        while(col<=3){
                                          lineX = sum(locations[,col] == "X", na.rm = TRUE)
                                          lineO = sum(locations[,col] == "O", na.rm = TRUE)
                                          scores = private$sumLine(lineX, lineO, scores)
                                          col = col+1
                                        }
                                        
                                        #get diagonal scores
                                        diagX = c(locations[1,1] == "X", locations[2,2] == "X",
                                                  locations[3,3] == "X")
                                        diagO = c(locations[1,1] == "O", locations[2,2] == "O",
                                                  locations[3,3] == "O")
                                        lineX = sum(diagX, na.rm = TRUE)
                                        lineO = sum(diagO, na.rm = TRUE)
                                        scores = private$sumLine(lineX, lineO, scores)
                                        #second diagonal
                                        diagX = c(locations[1,3] == "X", locations[2,2] == "X",
                                                  locations[3,1] == "X")
                                        diagO = c(locations[1,3] == "O", locations[2,2] == "O",
                                                  locations[3,1] == "O")
                                        lineX = sum(diagX, na.rm = TRUE)
                                        lineO = sum(diagO, na.rm = TRUE)
                                        scores = private$sumLine(lineX, lineO, scores)
                                        
                                        #add in priorities
                                        #add one for each corner or center
                                        priorities = c(locations[1,1], locations[1,3],
                                                       locations[2,2], locations[3,1],
                                                       locations[3,3])
                                        scores[1] <- scores[1]+sum(priorities == "X", na.rm = TRUE)
                                        scores[2] <- scores[2]+sum(priorities == "O", na.rm = TRUE)
                                        #add another for center (best spot)
                                        if(is.na(locations[2,2])){
                                          #would throw an error if checked with X and O
                                        }else{
                                          if(locations[2,2] == "X"){
                                            scores[1] <- scores[1]+1
                                          } else if(locations[2,2] == "O"){
                                            scores[2] <- scores[2]+1
                                            cat(paste("O in center spot \n"))
                                          } 
                                        }
                                        return(scores[1] - scores[2])
                                      },
                                      sumLine = function(totX, totO, scores){
                                        #' internal method to add score for an individual line
                                        #' @param totX the number of Xs in a line
                                        #' @param totO the number of Ox in the same line
                                        #' @param scores- a vector at least 2 items long with x's current score in first position and y's current score in second location
                                        #' @return scores - a 2 item vector with x's updated score in first index and o's updated score in second position
                                        x <- scores[1]
                                        o <- scores[2]
                                        if(totX == 3){
                                          #x wins - 3 in a row
                                          x <- Inf
                                          return(c(x, o))
                                        }else if(totO == 3){
                                          #o wins, 3 in a row
                                          o <- Inf
                                          return(c(x, o))
                                        }else if(totX == 2 && totO == 0){
                                          #2 Xs alone in line, add 3
                                          x <- x+3
                                          #cat(paste("Two Xs alone \n"))
                                        }else if(totO == 2 && totX == 0){
                                          #2 o's alone in line, add 3
                                          o <- o+3
                                          #cat(paste("Two Os alone \n"))
                                        }else if(totX == 1 && totO == 0){
                                          #one X alone, add one
                                          x <- x+totX
                                          #cat(paste("One X alone \n"))
                                        }else if(totO ==1 && totX == 0){
                                          #one O alone, add 1
                                          o <- o+totO
                                          #cat(paste("One O alone \n"))
                                        }
                                        return(c(x,o))
                                      }
                                      )
                      )

#add a function to generate a unique value for each game state
unique_score = function(score, board){
  #' generate a unique score based on precedence given a game state and game state and current score
  #' @param score a numeric value of the current score 
  #' @param board a board state to evaluate against priority matrix
  #' @return the score after updating with a priority weight on each location
  priority = matrix(
    data = c(8, 4, 7, 
             3, 9, 1, 
             6, 2, 5), 
    nrow = 3, ncol = 3)
  i = 1
  while(i < 4){
    j = 1
    while(j < 4){
      if(!is.na(board[i,j])){
        #add for X and subtract for O
        if(board[i,j] == "X"){
          score = score + priority[i,j]
        }else{
          score = score - priority[i,j]
        }
      }
      j = j+1
    }
    i = i +1
  }
  return(score)
}


#Evaluate game states
#board1
game = tic_tac_toe$new()
game$board$getBoard()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(2,2)
evals = unique_score(game$winning(), game$board$getBoard())
#board2
game$clear()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(3,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board3
game$clear()
game$moveO(1,1)
game$moveX(1,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board4
game$clear()
game$moveO(2,2)
game$moveX()
game$moveO(2,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board5
game$clear()
game$moveO(2,2)
game$moveX(2,1)
game$moveO(3,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board6
game$clear()
game$moveO(3,1)
game$moveX(1,3)
game$moveO(2,2)
game$moveX(2,3)
game$moveO(3,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board7
game$clear()
game$moveO(2,2)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board8
game$clear()
game$moveO(3,1)
game$moveX(1,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board9
game$clear()
game$moveO(3,1)
game$moveX(1,3)
game$moveO(1,1)
game$moveX(3,2)
game$moveO(3,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board10
game$clear()
game$moveO(1,1)
game$moveO(3,1)
game$moveX(2,1)
game$moveX(2,2)
game$moveO(2,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board11
game$clear()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(2,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board12
game$clear()
game$moveO(1,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board13
game$clear()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(3,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
#board14
game$clear()
game$moveO(3,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
evals


#put this into a binary tree
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

ticTree = binTree$new(evals)
ticTree$root
ticTree$getMin()
ticTree$getMax()
evals

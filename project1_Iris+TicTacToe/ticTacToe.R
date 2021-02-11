library(methods)
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
                                        scores = c(x <- 0, o <- 0)
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
                                        priorities = c(locations[1,1], locations[1,3],
                                                       locations[2,2], locations[3,1],
                                                       locations[3,3])
                                        x <- x+sum(priorities == "X", na.rm = TRUE)
                                        o <- o+sum(priorities == "O", na.rm = TRUE)
                                        #add another for center (best spot)
                                        if(is.na(locations[2,2])){
                                          #would throw an error if checked with X and O
                                        }else{
                                          if(locations[2,2] == "X"){
                                            x <- x+1
                                          } else if(locations[2,2] == "O"){
                                            o <- o+1
                                          } 
                                        }
                                        return(x-o)
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
                                        }else if(totO ==3){
                                          #o wins, 3 in a row
                                          o <- Inf
                                          return(c(x, o))
                                        }else if(totX == 2 && totO == 0){
                                          #2 Xs alone in line, add 3
                                          x <- x+3
                                        }else if(totO == 2 && totX == 0){
                                          #2 o's alone in line, add 3
                                          o <- o+3
                                        }else if(totX == 1 && totO == 0){
                                          #one X alone, add one
                                          x <- x+totX
                                        }else if(totO ==1 && totX == 0){
                                          #one O alone, add 1
                                          o <- o+totO
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



game = tic_tac_toe$new()
game$board$getBoard()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(2,2)
game$winning()
evals = unique_score(game$winning(), game$board$getBoard())
game$clear()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(3,1)
game$winning()
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(1,1)
game$moveX(1,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(2,2)
game$moveX()
game$moveO(2,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(2,2)
game$moveX(2,1)
game$moveO(3,1)
evals = append(evals, game$winning())
game$clear()
game$moveO(3,1)
game$moveX(1,3)
game$moveO(2,2)
game$moveX(2,3)
game$moveO(3,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(2,2)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(3,1)
game$moveX(1,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(3,1)
game$moveX(1,3)
game$moveO(1,1)
game$moveX(3,2)
game$moveO(3,3)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(2,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(1,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(1,1)
game$moveX(1,3)
game$moveO(3,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
game$moveO(3,1)
evals = append(evals, unique_score(game$winning(), game$board$getBoard()))
game$clear()
evals




library(methods)

game_board = R6Class("game_board", 
                        public = list(
                          take = function(player, row, col){
                            if(row>3 || row<0 || col>3 || col<0){
                              cat(paste("Invalid space, locations index only 1:3"))
                              return(NULL)
                            }else if(private$empties[row,col]){
                              private$empties[row, col] = FALSE
                              private$locations[row, col] = player
                              getBoard()
                            }else{
                              cat(paste("Invalid space, already filled"))
                              return(NULL)
                            }
                          },
                          getBoard = function(){
                            return(private$locations)
                          },
                          getEmpties = function(){
                            return(private$empties)
                          }),
                        private = list(
                          locations = matrix(nrow = 3, ncol = 3), 
                          empties = matrix(data = rep(TRUE, 9),
                                           nrow = 3, ncol = 3)
                          )
)

board = game_board$new()
board
board$take("O", 2,2)
board$getBoard()
board$take("X", 4, 1)
board$take("X", 2, 1)
board$take("O", 2, 1)

stic_tac_toe = R6Class("ticTacToe", public = list(
                                    board,  
                                    initialize = function(){
                                      board = game_board$new
                                    },
                                    moveX = function(){
                                      spot = private$getPriority(board$getEmpties)
                                      board$take("X", spot[1], spot[2])
                                    },
                                    moveO = function(row, col){
                                      board$take("O", row, col)
                                    },
                                    winning = function(){
                                      score = private$getScore()
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
                                    },
                                    winner = function(score){
                                      if(score>0){
                                        cat(paste("X wins!"))
                                      }else{
                                        cat(paste("O wins!"))
                                      }
                                      cat(paste(board$getBoard()))
                                    },
                                    
                                    ), 
                      private = list(
                        priority = matrix(
                          data = c(1,0,1,0,1,0,1,0,1), 
                          nrow = 3, ncol = 3)
                        ),
                      getScore = function(){
                        x <- 0
                        o <- 0
                        
                        #calculate the score across each rows
                        row <- 1
                        while(row<=3){
                          lineX = sum(locations[row,] == "X", na.rm = TRUE)
                          lineO = sum(locations[row,] == "O", na.rm = TRUE)
                          if(lineX == 3){
                            #x wins - 3 in a row
                            x <- Inf
                            o<-0
                            break
                          }else if(lineO ==3){
                            #o wins, 3 in a row
                            x <- 0
                            o <- Inf
                          }else if(lineX == 2 && lineO == 0){
                            #2 Xs alone in line, add 3
                            x <- x+3
                          }else if(lineO == 2 && lineX == 0){
                            #2 o's alone in line, add 3
                            o <- o+3
                          }else{
                            x <- x+lineX
                            o <- o+lineO
                          }
                          row = row+1
                        }
                        
                        #calculate score by columns
                        col = 1
                        while(col<=3){
                          lineX = sum(locations[,col] == "X", na.rm = TRUE)
                          lineO = sum(locations[,col] == "O", na.rm = TRUE)
                          if(lineX == 3){
                            #x wins - 3 in a row
                            x <- Inf
                            o<-0
                            break
                          }else if(lineO ==3){
                            #o wins, 3 in a row
                            x <- 0
                            o <- Inf
                          }else if(lineX == 2 && lineO == 0){
                            #2 Xs alone in line, add 3
                            x <- x+3
                          }else if(lineO == 2 && lineX==0){
                            #2 o's alone in line, add 3
                            o <- o+3
                          }else{
                            x <- x+lineX
                            o <- o+lineO
                          }
                          col = col +1
                        }
                        
                        #get diagonal scores
                        diagX = c(locations[1,1] == "X", locations[2,2] == "X",
                          locations[3,3] == "X")
                        diagO = c(locations[1,1] == "O", locations[2,2] == "O",
                          locations[3,3] == "O")
                        lineX = sum(diagX, na.rm = TRUE)
                        lineO = sum(diagO, na.rm = TRUE)
                        if(lineX == 3){
                          #x wins - 3 in a row
                          x <- Inf
                          o<-0
                          break
                        }else if(lineO ==3){
                          #o wins, 3 in a row
                          x <- 0
                          o <- Inf
                        }else if(lineX == 2 && lineO == 0){
                          #2 Xs alone in line, add 3
                          x <- x+3
                        }else if(lineO == 2 && lineX==0){
                          #2 o's alone in line, add 3
                          o <- o+3
                        }else{
                          x <- x+lineX
                          o <- o+lineO
                        }
                        #second diagonal
                        diagX = c(locations[1,3] == "X", locations[2,2] == "X",
                                  locations[3,1] == "X")
                        diagO = c(locations[1,3] == "O", locations[2,2] == "O",
                                  locations[3,1] == "O")
                        lineX = sum(diagX, na.rm = TRUE)
                        lineO = sum(diagO, na.rm = TRUE)
                        if(lineX == 3){
                          #x wins - 3 in a row
                          x <- Inf
                          o<-0
                          break
                        }else if(lineO ==3){
                          #o wins, 3 in a row
                          x <- 0
                          o <- Inf
                        }else if(lineX == 2 && lineO == 0){
                          #2 Xs alone in line, add 3
                          x <- x+3
                        }else if(lineO == 2 && lineX==0){
                          #2 o's alone in line, add 3
                          o <- o+3
                        }else{
                          x <- x+lineX
                          o <- o+lineO
                        }
                        
                        #add in priorities
                        priorities = c(locations[1,1], locations[1,3], 
                                       locations[2,2], locations[3,1], 
                                       locations[3,3])
                        x <- x + sum(priorities == "X", na.rm = TRUE)
                        o <- o + sum(priorities == "O", na.rm = TRUE)
                        return(x-o)
                      }

)

library(R6)

priority_board_X = function(){
  prior = c(17, 13, 16, 
            12, 18, 11, 
            15, 10, 14)
}
priority_board_Y = funtion(){
  prior = c(8, 4, 7, 
            3, 9, 2, 
            6, 2, 5)
}
evaluate_board = function(board){
  hor1 = c(1,2,3)
  hor2 = c(4,5,6)
  hor3 = c(7,8,9)
  vert1 = c(1,4,7)
  vert2 = c(2,5,8)
  vert3 = c(3,6,9)
  diag1 = c(1,5,9)
  diag2 = c(3,5,7)
  lines = rbind(hor1, hor2, hor3, vert1, vert2, vert3, diag1, diag2)
  x2 = 
    sum(board[lines] == (c(X,X,E) || c(X,E,X) || c(E,X,X)))
  x1 =
    for(rows in lines){
      sum(all(board[lines[i,]] == c("X", " ", " ")))
    }
    sum(board[lines] == (c(X,E,E) || c(E,X,E) || c(E,E,X)))
  o2 = 
    sum(board[lines] == (c("X","X"," ") || c("O"," ","O") || c(" ","O","O")))
  if(board[1])
  comp = 3*x2+x1 -(3*o2+o1)
}

print_board = function(state = c(rep(" ", 9))){
  loc = 1:9
  hor1 = c(1,2,3)
  hor2 = c(4,5,6)
  hor3 = c(7,8,9)
  vert1 = c(1,4,7)
  vert2 = c(2,5,8)
  vert3 = c(3,6,9)
  diag1 = c(1,5,9)
  diag2 = c(3,5,7)
  cat(paste(state[1],"|", state[2],"|", state[3]))
  cat(paste("\n---------\n"))
  cat(paste(state[4],"|", state[5],"|", state[6]))
  cat(paste("\n---------\n"))
  cat(paste(state[7],"|", state[8],"|", state[9]))
}
print_board()

state1 = c("O"," ","X"," ","O"," "," "," "," ")
print_board(state1)
state2 = c(rep(" ", 9))
state2[1] = "O"
state2[7] = "O"
state2[3] = "X"
print_board(state2)
state2[lines] == (c("X","X"," "))
o2


#library(methods)
library("R6")
game_board = R6Class("game_board", 
                     #' create a game board for tic tac toe and initialize to empty (NA) values
                     #' has public methods take(), getBoard(), getEmpties(), and clearBoard()
                     #' @author Casey Jayne Richards
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
                             #' @return the board with current characters
                             return(private$locations)
                         },
                         getEmpties = function(){
                             #'@return a boolean matrix with free spaces represented as TRUE
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



tic_tac_toe = R6Class("ticTacToe", 
                      public = list(
                        #'@author Casey Jayne Richards
                        board = game_board$new(),
                        printboard = function(){
                          print(self$board$getBoard()) 
                        },
                        initialize = function(){
                            #'creates a new tic tac toe game with a public board and 
                            #'options to moveX(), moveO(), get winning(), or get winner()
                            #'relies on the R6 class game_board
                            self$board$clearBoard()
                            self$board$getBoard()
                        },
                        moveX = function(row = NULL, col = NULL){
                            #' either give a distinct location for X to move or allow it to move according to priority
                            #' @param row an integer index for row to move
                            #' @param col an integer index for col to move
                            if(is.null(row)){
                                spot = private$getPriority()
                                now = self$board$getEmpties()
                                self$board$take("X", spot[1], spot[2])
                            }
                            else{
                                self$board$take("X", row, col)
                            }
                            self$printboard()
                        },
                        # alternative, conditional best
                        reflex_AI_X = function(){
                            #'checks if X can win as first option, then if X can block O from winning
                            #'if neither available, moves according to priority
                            options = self$board$getEmpties()
                            # check if the X position can win
                            # start with rows
                            locations <- self$board$getBoard()
                            for(row in seq(1,3)){
                                lineX = sum(locations[row,] == "X", na.rm = TRUE)
                                lineO = sum(locations[row,] == "O", na.rm = TRUE)
                                if(lineX == 2 && lineO == 0){
                                    # x can win, see where the space is
                                    now = self$board$getEmpties()
                                    col = now[row,]
                                    col = match(TRUE, col)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard()) 
                                    }
                                }
                            }
                            # check columns
                            for (col in seq(1,3)){
                                lineX = sum(locations[,col] == "X", na.rm = TRUE)
                                lineO = sum(locations[,col] == "O", na.rm = TRUE)
                                if(lineX == 2 && lineO == 0){
                                    # x can win, see where the space is
                                    now = self$board$getEmpties()
                                    row = now[,col]
                                    row = match(TRUE, row)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard()) 
                                    }
                                }
                            }
                            # check diags
                            diagX = c(locations[1,1] == "X", locations[2,2] == "X",
                                      locations[3,3] == "X")
                            diagO = c(locations[1,1] == "O", locations[2,2] == "O",
                                      locations[3,3] == "O")
                            lineX = sum(diagX, na.rm = TRUE)
                            lineO = sum(diagO, na.rm = TRUE)
                            if(lineX == 2 && lineO == 0){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,1]){
                                    spot = c(1,1)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,3)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            #second diagonal
                            diagX = c(locations[1,3] == "X", locations[2,2] == "X",
                                      locations[3,1] == "X")
                            diagO = c(locations[1,3] == "O", locations[2,2] == "O",
                                      locations[3,1] == "O")
                            lineX = sum(diagX, na.rm = TRUE)
                            lineO = sum(diagO, na.rm = TRUE)
                            if(lineX == 2 && lineO == 0){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,3]){
                                    spot = c(1,3)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,1)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            # check if block available
                            # start with rows
                            locations <- self$board$getBoard()
                            for(row in seq(1,3)){
                                lineX = sum(locations[row,] == "X", na.rm = TRUE)
                                lineO = sum(locations[row,] == "O", na.rm = TRUE)
                                if(lineX == 0 && lineO == 2){
                                    # x can win, see where the space is
                                    now = self$board$getEmpties()
                                    col = now[row,]
                                    col = match(TRUE, col)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard())   
                                    }
                                }
                            }
                            # check columns
                            for (col in seq(1,3)){
                                lineX = sum(locations[,col] == "X", na.rm = TRUE)
                                lineO = sum(locations[,col] == "O", na.rm = TRUE)
                                if(lineX == 0 && lineO == 2){
                                    # x can win, see where the space is
                                    now = self$board$getEmpties()
                                    row = now[,col]
                                    row = match(TRUE, row)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard())   
                                    }
                                }
                            }
                            # check diags
                            diagX = c(locations[1,1] == "X", locations[2,2] == "X",
                                      locations[3,3] == "X")
                            diagO = c(locations[1,1] == "O", locations[2,2] == "O",
                                      locations[3,3] == "O")
                            lineX = sum(diagX, na.rm = TRUE)
                            lineO = sum(diagO, na.rm = TRUE)
                            if(lineX == 0 && lineO == 2){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,1]){
                                    spot = c(1,1)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,3)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            #second diagonal
                            diagX = c(locations[1,3] == "X", locations[2,2] == "X",
                                      locations[3,1] == "X")
                            diagO = c(locations[1,3] == "O", locations[2,2] == "O",
                                      locations[3,1] == "O")
                            lineX = sum(diagX, na.rm = TRUE)
                            lineO = sum(diagO, na.rm = TRUE)
                            if(lineX == 0 && lineO == 2){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,3]){
                                    spot = c(1,3)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,1)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            # if get to here, no block or win
                            # check the corners strategy
                            for(row in c(1,3)){
                                horzO = sum(locations[row,] == "O", na.rm = TRUE)
                                horzX = sum(locations[row,] == "X", na.rm = TRUE)
                                if(horzO == 1 && horzX == 0){
                                    # check adjacent columns
                                    for(col in c(1,3)){
                                        vertO = sum(locations[,col] == "O", na.rm = TRUE)
                                        vertX = sum(locations[,col] == "X", na.rm = TRUE)
                                        if(vertO == 1 && horzX == 0){
                                            # move into adjacent corner!
                                            if(row == 1 && col == 1){
                                                if(options[1,1]){
                                                    return(self$moveX(1,1))  
                                                }
                                            }else if(row == 1 && col == 3){
                                                if(options[1,3]){
                                                    return(self$moveX(1,3))    
                                                }
                                            }else if(col == 1){
                                                if(options[3,1]){
                                                    return(self$moveX(3,1))   
                                                }
                                            }else{
                                                if(options[3,3]){
                                                    return(self$moveX(3,3)) 
                                                }
                                            }
                                        }
                                    }       
                                }
                            }
                            # move on priority
                            return(self$moveX())
                        },
                        # alternative, goal-based states
                        goal_AI_X = function(){
                            #'checks if X can win as first option, then if X can block O from winning
                            #'if neither available, moves according to priority
                            options = self$board$getEmpties()
                            # check if the X position can win
                            # start with rows
                            locations <- self$board$getBoard()
                            
                            # Goal one check current win
                            win <- function(line){
                                # check for a win and take that space
                                lineX = sum(line == "X", na.rm = TRUE)
                                lineO = sum(line == "O", na.rm = TRUE)
                                if(lineX == 2 && lineO == 0){
                                    return(TRUE)
                                }else{
                                    return(FALSE)
                                }
                            }
                            # Goal 2, check current block
                            block <- function(line){
                                lineX = sum(line == "X", na.rm = TRUE)
                                lineO = sum(line == "O", na.rm = TRUE)
                                if(lineX == 0 && lineO == 2){
                                    return(TRUE)
                                }else{
                                    return(FALSE)
                                }
                            }
                            # Goal 3, check future win
                            future_win <- function(line){
                                lineX = sum(line == "X", na.rm = TRUE)
                                lineO = sum(line == "O", na.rm = TRUE)
                                if(lineX == 1 && lineO == 0){
                                    return(TRUE)   
                                }else{
                                    return(FALSE)
                                }
                            }
                            # Goal 4, check future block
                            future_block <- function(line){
                                lineX = sum(line == "X", na.rm = TRUE)
                                lineO = sum(line == "O", na.rm = TRUE)
                                if(lineX == 0 && lineO == 1){
                                    return(TRUE)   
                                }else{
                                    return(FALSE)
                                }
                            }
                            
                            # Check for win!
                            for(row in seq(1,3)){
                                line = locations[row,]
                                state <- win(line)
                                if(state){
                                    # x can win, see where the space is
                                    now = self$board$getEmpties()
                                    col = now[row,]
                                    col = match(TRUE, col)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard()) 
                                    }
                                }
                            }
                            # check columns
                            for (col in seq(1,3)){
                                line = locations[,col]
                                state <- win(line)
                                if(state){
                                    # x can win, see where the space is
                                    now = self$board$getEmpties()
                                    row = now[,col]
                                    row = match(TRUE, row)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard()) 
                                    }
                                }
                            }
                            # check diags
                            line = c(locations[1,1], locations[2,2],
                                     locations[3,3])
                            state <- win(line)
                            if(state){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,1]){
                                    spot = c(1,1)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,3)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            #second diagonal
                            line = c(locations[1,3], locations[2,2],
                                      locations[3,1])
                            state <- win(line)
                            if(state){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,3]){
                                    spot = c(1,3)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,1)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            
                            # check if block available
                            # start with rows
                            for(row in seq(1,3)){
                                line = locations[row,]
                                state <- block(line)
                                if(state){
                                    # x can block, see where the space is
                                    now = self$board$getEmpties()
                                    col = now[row,]
                                    col = match(TRUE, col)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard())   
                                    }
                                }
                            }
                            # check columns
                            for (col in seq(1,3)){
                                line = locations[,col]
                                state <- block(line)
                                if(state){
                                    # x can block, see where the space is
                                    now = self$board$getEmpties()
                                    row = now[,col]
                                    row = match(TRUE, row)
                                    if(options[row,col]){
                                        self$moveX(row, col)
                                        return(self$board$getBoard())   
                                    }
                                }
                            }
                            # check diags
                            line = c(locations[1,1], locations[2,2],
                                      locations[3,3])
                            state <- block(line)
                            if(state){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,1]){
                                    spot = c(1,1)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,3)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            #second diagonal
                            line = c(locations[1,3], locations[2,2],
                                      locations[3,1])
                            state <- block(line)
                            if(state){
                                # x can win, see where the space is
                                now = self$board$getEmpties()
                                if(now[1,3]){
                                    spot = c(1,3)
                                }else if(now[2,2]){
                                    spot = c(2, 2)
                                }else{
                                    spot = c(3,1)
                                }
                                if(options[spot[1],spot[2]]){
                                    self$moveX(row, col)
                                    return(self$board$getBoard()) 
                                }
                            }
                            
                            # if get to here, no block or win
                            # check the corners strategy for a win
                            for(row in c(1,3)){
                                line <- locations[row,]
                                state <- future_win(line)
                                if(state){
                                    # check adjacent columns
                                    for(col in c(1,3)){
                                        line <- locations[,col]
                                        state <- future_win(line)
                                        if(state){
                                            # move into adjacent corner!
                                            if(row == 1 && col == 1){
                                                if(options[1,1]){
                                                    return(self$moveX(1,1))  
                                                }
                                            }else if(row == 1 && col == 3){
                                                if(options[1,3]){
                                                    return(self$moveX(1,3))    
                                                }
                                            }else if(col == 1){
                                                if(options[3,1]){
                                                    return(self$moveX(3,1))   
                                                }
                                            }else{
                                                if(options[3,3]){
                                                    return(self$moveX(3,3)) 
                                                }
                                            }
                                        }
                                    }       
                                }
                            }
                            # check for future block!
                            ## check if can use offensive corner strategy
                            for(row in c(1,3)){
                                line = locations[row,]
                                state <- future_block(line)
                                if(state){
                                    # check adjacent columns
                                    for(col in c(1,3)){
                                        line = locations[,col]
                                        state <- future_block(line)
                                        if(state){
                                            # move into adjacent corner!
                                            if(row == 1 && col == 1){
                                                if(options[1,1]){
                                                    return(self$moveX(1,1))  
                                                }
                                            }else if(row == 1 && col == 3){
                                                if(options[1,3]){
                                                    return(self$moveX(1,3))    
                                                }
                                            }else if(col == 1){
                                                if(options[3,1]){
                                                    return(self$moveX(3,1))   
                                                }
                                            }else{
                                                if(options[3,3]){
                                                    return(self$moveX(3,3)) 
                                                }
                                            }
                                        }
                                    }       
                                }
                            }
                            # move on priority
                            return(self$moveX())
                        },
                        minimax = function(board = self$board, direction = "max"){
                            #'@param board a game board R6 object
                            #'@param direction the initial direction (setup to only move X player)
                            #' a minimax to move X position only
                            # locations <- board$clone
                            options <- board$getEmpties()
                            # check if the X position can win
                            # start with rows
                            if(sum(options) == 1){
                                # only one spot
                                return(self$moveX(options == T))
                            }else if(sum(options) == 9){
                                # fully empty board, start with utility
                                return(self$moveX())
                            }
                            possibilities <- rep(0, 9)
                            for(num in 1:length(options)){
                                # convert to indeces
                                col <- ifelse(num/3>2, 3, 0)
                                if(col == 0){
                                    col <- ifelse(num/3>1, 2, 1)
                                }
                                row <- num - (col-1)*3
                                if(options[num]){
                                    # empty spot
                                    tgame <- board$clone()
                                    if(direction == "max"){
                                        tgame$take("X", row, col)
                                    }else{ # move O
                                        tgame$take("O", row, col)
                                    }
                                    possibilities[num] <- private$getScore(tgame$getBoard()) # score if we move there
                                    # another option
                                    # possibilities[location] <- self$utility(game) # score if we move there
                                }else{
                                    possibilities[num] <- NA
                                }
                            }
                            if(direction == "max"){
                                if(any(possibilities == Inf, na.rm = TRUE)){
                                    # winning move - take move
                                    # convert to indeces
                                    num <- which(possibilities == Inf)[1] # only take first if tie
                                    col <- ifelse(num/3>2, 3, 0)
                                    if(col == 0){
                                        col <- ifelse(num/3>1, 2, 1)
                                    }
                                    row <- num - (col-1)*3
                                    return(self$moveX(row,col))    
                                }
                            }else{
                                if(any(possibilities == -Inf, na.rm = TRUE)){
                                    # O can win
                                    num <- which(possibilities == -Inf)[1] # only take first if tie
                                    col <- ifelse(num/3>2, 3, 0)
                                    if(col == 0){
                                        col <- ifelse(num/3>1, 2, 1)
                                    }
                                    row <- num - (col-1)*3
                                    return(self$moveX(row,col))    
                                }    
                            }
                            
                            # no win or loss
                            # go with max or min score
                            if(direction == "max"){
                                tgame <- board$clone()
                                num <- which.max(possibilities)[1]
                                col <- ifelse(num/3>2, 3, 0)
                                if(col == 0){
                                    col <- ifelse(num/3>1, 2, 1)
                                }
                                row <- num - (col-1)*3
                                tgame$take("X", row, col)
                                possibilities <- self$minimax(tgame, "min")    
                            }else{
                                tgame <- board$clone()
                                num <- which.min(possibilities)[1]
                                col <- ifelse(num/3>2, 3, 0)
                                if(col == 0){
                                    col <- ifelse(num/3>1, 2, 1)
                                }
                                row <- num - (col-1)*3
                                tgame$take("O", row, col)
                                possibilities <- self$minimax(tgame, "max")    
                            }   
                                     },
                        moveO = function(row, col){
                            #' @param row the row position to move as integer
                            #' @param col the column position to move as integer
                            self$board$take("O", row, col)
                            self$printboard()
                        },
                        winning = function(){
                            #' get the character in the lead and by how much
                            #' @return the current score as integer
                            score = private$getScore(self$board$getBoard())
                            if(score == Inf || score == -Inf){
                                private$winner(score)
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
                        clear = function(){
                            #' empties the current game
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
                                    if(options[item[1], item[2]]){
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
                            cat(paste("No available space found \n"))
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
                        },
                        winner = function(score){
                            #' a method that states the winner at current state
                            #' @param score the current score
                            #' @return a string message of winner and current board
                            if(score>0){
                                return(cat(paste("X wins!\n")))
                            }else{
                                return(cat(paste("O wins!\n")))
                            }
                            print(self$board$getBoard())
                        }
                    )
)

# tic_tac_toe$debug("minimax")
game <- tic_tac_toe$new()
game$moveO(1,1)
game$minimax()
game$moveO(1,3)
game$minimax()
game$moveO(3,1)
game$minimax()
game$moveO(3,3)
game$minimax()
game$winning()

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

# start game
game <- tic_tac_toe$new()
game$clear()
game$moveO(1,1)
game$minimax()
game$moveX()
game$moveX(1,3)
game$goal_AI_X()
game$winning()
game$clear()
game$moveO(1,1)
game$moveX()
game$moveX(1,2)
game$goal_AI_X()
game$clear()
game$moveO(1,1)
game$moveX()
game$moveX(2,1)
game$goal_AI_X()
game$winning()
# all the winning works, check blocking
game$clear()
game$moveO(1,1)
game$moveO(1,2)
game$goal_AI_X()
game$winning()
game$moveO(2,2)
game$goal_AI_X()
game$winning()
game$moveO(2,1)
game$goal_AI_X()
game$winning()
game$goal_AI_X()
game$winning()

# try to play X
game <- tic_tac_toe$new()
game$board$getBoard()
game$moveO(2,2)
game$goal_AI_X()
game$moveO(1,2)
game$goal_AI_X()
game$moveO(2,3)
game$goal_AI_X()
game$moveO(3,1)
game$goal_AI_X()
game$clear()
game$moveO(3,2)
game$moveX(1,2)
game$moveO(2,3)
game$moveX(2,1)
game$moveO(2,2)
game$goal_AI_X()
game$moveO(3,1) 
game$goal_AI_X()


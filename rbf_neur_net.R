rbf <- function(data, classList,  smoothing = .1, train_amt = .8, bias = FALSE, bias_weights = NULL, training_dat = NULL, train_classList = NULL, normalize = TRUE, weights = NULL, decision = "max"){
    #'@param data a numeric data frame with n observations as rows and D dimesions as columns
    #'@param classList a list of index ranges separating the classes of the input data
    #'@param smoothing the smoothing parameter for the weight parameters
    #'@param train_amt a numeric amount between 0 and 1 that corresponds to the random proportion to use as training data
    #'@param bias should we shift according to weighted bias, if TRUE and no weights, auto-weighted as 1
    #'@param bias_weights an optional shift the length of the test data of bias weights
    #'@param normalize should the data be normalized between 0 and 1 (set to FALSE if already normalized)
    #'@return a list of class models, training data, and testing data for each
    #'the weights in this case are equivalent to the training data
    #'the number of neurons is the same as number of training observations
    if(!is.list(classList)){
        errorCondition("classList needs to be in list form")
    }
    if(normalize){
      # normalize the data
      data = normalize(data)       # normally want to normalize against the training data, see the commented out code 
    }
    # add a column of classes
    class <- rep("unknown", length(data[,1]))
    for(i in seq(1, length(classList))){
        inds <- classList[[i]]
        class[inds] <- i # make every item equal to that number
    }
    data <- data.frame(data)
    data$class <- class
    
    train <- data.frame()
    test <- data.frame()
    if(is.null(training_dat)){
      # randomly extract the training data
      # segregate train and test data
      for(class in seq(1, length(classList))){
        # extract training for each class
        t <- extract_training(data[classList[[class]],], train_amt)
        train <- rbind(train, t[[1]])
        test <- rbind(test, t[[2]])
      }  
    }else{
      # add class to test data given test_classList
      class <- rep("unknown", length(training_dat[,1]))
      for(i in seq(1, length(train_classList))){
        inds <- train_classList[[i]]
        class[inds] <- i # make every item equal to that number
      }
      training_dat <- data.frame(training_dat)
      training_dat$class <- class
      train <- training_dat
      test <- data 
    }
  
    # save the index of class column
    cl_indx <- dim(data)[2]
    # if(normalize){ # the way we typically want to normalize, when not in this case with random 20% removed inside
    #     # normalize the data
    #     trainclass = train[,cl_indx]
    #     testclass = test[,cl_indx]
    #     train = normalize(train[, -cl_indx])
    #     test = normalize(test[,-cl_indx], normAgainst = train[-cl_indx])
    #     train$class <- trainclass
    #     test$class <- testclass
    # }

    # save the number testing
    n_test <- dim(test)[1]
    if(is.null(train_classList)){
      # save the training classList
      train_classList <- list()
      for (i in 1:length(classList)){ # for number of classes
        t = which(train[, cl_indx] == i)
        train_classList[[i]] <- t # create a classList of the training classes
      }  
    }
    
    # create a model for each class
    class_models <- list()
    classNum <- 1
    class_predict <- rep(0, dim(test)[1]) # overall prediction vector
    for(class in train_classList){ 
      # create a class vector to pass
      class_vec <- rep(-1, dim(train)[1])
      class_vec[class] <- 1 # set associated values to 1
      if(bias){
        train_model <- rbf_train(training = train[,-cl_indx], class_vector = class_vec, 
                                 smoothing = smoothing, bias = TRUE, bias_weights = bias_weights)
      }else{
        train_model <- rbf_train(training = train[,-cl_indx], class_vector = class_vec, smoothing = smoothing)
      }
      # test against model
      test_model <- rbf_classify(test_data = test[,-cl_indx], trained_model = train_model)  
      cl <- list(trained_model = train_model, tested = test_model) # nest the lists
      class_predict[cl$tested$prediction == 1] <- classNum # set to the current class
      class_models <- append(class_models, cl)
      classNum = classNum + 1 # increment to next class
    }
    
    # return all the models' accuracy
    return(list(prediction  = class_predict, models = class_models, training = train, testing = test))
}

rbf_accuracy <- function(all_output){
  #'@param all_output should be the output from the RBF class that includes class labels for input
  #'@return the accuracy as a percentage
  correct <- sum(all_output$prediction == as.numeric(all_output$testing$class))
  acc <- correct/length(all_output$prediction)
  return(acc)
}

rbf_train <- function(training, class_vector, smoothing, bias = FALSE, bias_weights = NULL){
    #'@param data: an n observations by dimension/feature (columns) data frame or matrix of training data
    #'@param class_vector a vector of classes [1,-1] that the training should be assigned to, with 1 noting: in the class and -1: out of the class
    #'@param bias should we associate weights to shit the matrix or not? 
    #'@param bias_weights: a neurons/clusters X class matrix or data frame of weights; if null but bias is true, the weights will equal the training data
    #'@return a named list with the prediction class vector, input data, smoothing factor, accuracy, bias status, bias weights, and incorrect observations
    # check that class_vector is only -1s and 1s
    test <- abs(class_vector)!=1
    if(TRUE %in% test){
      errorCondition("Invalid Class vector - only 1 and -1 allowed")
    }
    
    # calculate the H matrix
    require(plyr)
    observations = dim(training)[1]
    H = matrix(rep(0,  observations^2), nrow = observations, ncol = observations)
    for(obs in 1:observations){
        W = colwise(.fun = function(x) {x - x[obs]})(training) # get each observations' weight by subtracting every obs from it
        H[obs, ] = exp(-(rowSums(W*W)/(2*smoothing^2))) # set the H row to the exponential solution of the weights
    }
    
    if(!bias){
      # get the weights with pinv
      require(pracma)
      w_hat = pinv(t(H) %*% H) %*% t(H) %*% class_vector
      bias = 0
    }else{
      # use the bias version
      if(is.null(bias_weights)){
        bias_weights = rep(1, observations)
      }
      Htmp <- rbind(H, bias_weights)
      require(pracma)
      wtmp = mrdivide(class_vector, Htmp)
      w_hat <- wtmp[1:observations] # grab without the bias weights
      bias_weights <- wtmp[observations+1] # grab last for bias
    }
    
    # get prediction
    y_hat <- H %*% w_hat + bias_weights
    y_hat[y_hat<0] <- -1
    y_hat[y_hat>0] <- 1
    # check accuracy
    wrong <- y_hat!=class_vector
    accuracy <- (length(class_vector) - sum(wrong))/length(class_vector)
    # flag the incorrect
    wrong <- which(y_hat!=class_vector)
    return(list(prediction  = y_hat, 
                w_hat = w_hat,
                input = training, 
                smoothing = smoothing, 
                bias = bias, 
                bias_weights = bias_weights,
                accuracy = accuracy, 
                wrong = wrong))
}

rbf_classify <- function(test_data, trained_model){
  #'@param test_data a data frame of data to test without class labels
  #'@param trained_model a model that was output from rbf_train
  #'@return a list containing the smoothing parameter, bias status, bias weights, raw prediction, and class prediction
  # get the trained data
  model_nodes <- dim(trained_model$input)[1]
  if(is.null(dim(test_data))){
    # only one obs
    test_size <- 1
  }else{
    test_size <- dim(test_data)[1]
  }
  
  H = matrix(rep(0,  test_size*model_nodes), ncol = test_size, nrow = model_nodes)
  for(node in 1:model_nodes){
    single = matrix(rep(trained_model$input[node,], each = test_size), nrow = test_size) # repeat one value to subtract
    # M <- apply(test_data, 1, function(x)trained_model$input[node,]-x)
    M <- test_data - as.numeric(single)
    H[node,] = exp(-(rowSums(M*M)/(2*trained_model$smoothing^2))) # set the H row to the exponential solution of the model
  }
  if(!trained_model$bias){
    y_hat = t(H) %*% trained_model$w_hat
  }else{
    # b <-  as.numeric(trained_model$bias_weigths)
    y_hat = (t(H) %*% trained_model$w_hat) + trained_model$bias_weights
  }
  
  raw_yhat <- y_hat # save the unrounded values
  y_hat[y_hat<0] <- -1
  y_hat[y_hat>0] <- 1
  return(list(prediction  = y_hat, 
              raw_yhat  = raw_yhat,
              smoothing = trained_model$smoothing, 
              bias = trained_model$bias,
              bias_weights = trained_model$bias_weights))
}


# testing the H function works correctly (training)
getH <- function(training, smoothing){
    require(plyr)
    observations = dim(training)[1]
    H = matrix(rep(0,  observations^2), nrow = observations, ncol = observations)
    for(obs in 1:observations){
        W = colwise(.fun = function(x) {x - x[obs]})(training) # get each observations' weight by subtracting every obs from it
        H[obs, ] = exp(-(rowSums(W*W)/(2*smoothing^2))) # set the H row to the exponential solution of the weights
    }
    return(H)
}

normalize <- function(data, normAgainst = NULL){
    #'@param data a numeric data frame with dimenstions as cols and obs as rows
    #'@param normAgainst an optional data frame matching input data structurally to normalize by/against
    #'performs a max-min normalization to bring into 0-1 range
    #'@return the normalized data frame
    #'@author Casey Jayne
    if(is.null(normAgainst)){
        d <- dim(data)[2]
        mins <- apply(data, 2, min)
        maxs <- apply(data, 2, max)
        out <- t((t(data) - mins)/(maxs - mins))
        return(out)    
    }else{
      # here we want to make sure the max and min are within the range of our test data
        mins <- apply(normAgainst, 2, min)
        maxs <- apply(normAgainst, 2, max)
        out <- t((t(data) - mins)/(maxs - mins))
        return(out)    
    }
}

extract_training <- function(data, train_amount=.2){
    #'@param data a numeric data frame or matrix with n observations as rows and D dimensions as columns
    #'@param train_amount a numeric amount between 0 and 1 that corresponds to the random proportion to use as training data
    #'@return a list containing the original data separated into training_dat and test_dat
    if(train_amount > 1 | train_amount < 0){
        errorCondition("Training amount not between 0 and 1")
    }
    if(!is.numeric(data)){
        errorCondition("input data must be numeric")
    }
    N = dim(data)[1] # total number of rows
    select = sample(seq(1,N), N*train_amount)
    select = select[order(select)] # put back in order
    train = data[select, ]
    select = seq(1,N)[-select]
    test = data[select, ]
    return(list(training_dat = train, test_dat = test))
}

extract_training <- function(data, train_amount=.8, classList = NULL){
    #'@param data a numeric data frame or matrix with n observations as rows and D dimensions as columns
    #'@param train_amount a numeric amount between 0 and 1 that corresponds to the random proportion to use as training data
    #'@param classList an optional parameter of classes to extract from each evenly
    #'@return a list containing the original data separated into training_dat and test_dat'.  If a classList is passed will also have training_classList and testing_classList returned with lists of each index for each class passed in the original list
    #'@author Casey Jayne
    if(train_amount > 1 | train_amount < 0){
        errorCondition("Training amount not between 0 and 1")
    }
    if(!is.numeric(data)){
        errorCondition("input data must be numeric")
    }
    if(is.null(classList)){
        N = dim(data)[1] # total number of rows
        select = sample(seq(1,N), N*train_amount)
        select = select[order(select)] # put back in order
        train = data[select, ]
        select = seq(1,N)[-select]
        test = data[select, ]
        return(list(training_dat = train, test_dat = test))    
    }else{
        return(extract_training.equal_classes(data, train_amount, classList))
    }
    
}

extract_training.equal_classes <- function(data, train_amount, classList){
    if(!is.list(classList)){
        errorCondition("classList needs to be in list form")
    }
    # add a column of classes
    class <- rep("unknown", length(test_data[,1]))
    for(i in seq(1, length(classList))){
        inds <- classList[[i]]
        class[inds] <- i # make every item equal to that number
    }
    test_data <- data.frame(test_data)
    test_data$class <- class
    train <- data.frame()
    test <- data.frame()
    # segregate train and test
    for(class in seq(1, length(classList))){
        # extract training for each class
        t <- extract_training(test_data[classList[[class]],], train_amt)
        train = rbind(train, t[[1]])
        test = rbind(test, t[[2]])
    }
    # save the index of class
    cl_indx <- dim(test_data)[2]
    # make a list of class indexes for training and testing data
    train_classList <- list()
    test_classList <- list()
    for (i in 1:length(classList)){ # for number of classes
        t = which(train[, cl_indx] == i)
        train_classList[[i]] <- t # create a classList of the training classes
        t = which(test[, cl_indx] == i)
        test_classList[[i]] <- t
    }  
    
    return(training_dat = train, 
           testing_dat = test, 
           training_classList = train_classList, 
           testing_classList = testing_classList)
}
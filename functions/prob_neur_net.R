pnn <- function(data, classList, smoothing = .1, train_amt = .8, nodes = NULL, normalize = TRUE, weights = NULL, decision = "max"){
    #'@param data a numeric data frame with n observations as rows and D dimesions as columns
    #'@param classList a list of index ranges separating the classes of the input data
    #'@param smoothing the smoothing parameter for the weight parameters
    #'@param train_amt a numeric amount between 0 and 1 that corresponds to the random proportion to use as training data
    #'@param nodes the number of neural nodes per class, set to the number of training observations
    #'@param normalize should the data be normalized between 0 and 1 (set to FALSE if already normalized)
    #'the weights in this case are equivalent to the training data
    #'
    if(!is.list(classList)){
        errorCondition("classList needs to be in list form")
    }
    if(normalize){
        # normalize the data
        data = normalize(data)        
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
    # segregate train and test
    for(class in seq(1, length(classList))){
        # extract training for each class
        t <- extract_training(data[classList[[class]],], train_amt)
        train <- rbind(train, t[[1]])
        test <- rbind(test, t[[2]])
    }
    if(is.null(nodes)){
        nodes <- dim(train)[1] # same number of weights as training observations
    }
    # save the index of class column
    cl_indx <- dim(data)[2]
    # save the number training
    n_test <- dim(test)[1]
    # save the training classList
    train_classList <- list()
    for (i in 1:length(classList)){
        t = which(train[,cl_indx] == i)
        train_classList[[i]] <- t
    }
    
    accuracy <- n_test
    all_acc <- c()
    for(obs in 1:n_test){
        sums <- rep(0, length(classList))
        classNum <- 0
        for(class in train_classList){
            classSum <- 0
            classNum <- classNum + 1
            for(train_obs in class){
                temp <- t(test[obs, -cl_indx] - train[train_obs, -cl_indx])
                classSum <- classSum + exp(-(crossprod(temp)-1)/sigma^2)
            }
            sums[classNum] <- classSum/length(class)
        }
        assigned <- which(sums == max(sums)) # the assigned class
        if(assigned != test[obs,cl_indx]){
            accuracy = accuracy - 1
            all_acc = c(all_acc, 0)
        }else{
            all_acc = c(all_acc, 1)
        }
    }
    accuracy = accuracy/dim(test)[1] # convert accuracy to a percentage
    return(list(accuracy, all_acc))
}

pnn_train <- function(training, weights){
    #'@param data: an n observations by dimension/feature (columns) data frame or matrix of training data
    #'@param weights: a neurons/clusters X class matrix or data frame of weights
    classes <- dim(weights)[1]
    neurons <- dim(weights)[2]
    for(n in 1:neurons){
        temp <- t(training) - weights[n,]
        D <- weights
        diff_sum_squared <- t(crossprod(temp))
    }
    return(diff_sum_squared)
}


pnn_og <- function(data, classList, sigma, train_amt = .8, normalize = TRUE){
    #'@param data a numeric data frame with n observations as rows and D dimesions as columns
    #'@param classList a list of index ranges separating the classes of the input data
    #'@param sigma 
    #'@param train_amt a numeric amount between 0 and 1 that corresponds to the random proportion to use as training data
    #'@param nodes the number of neural nodes per class, set to the number of observations
    #'@param normalize should the data be normalized between 0 and 1 (set to FALSE if already normalized)
    #'@author Casey Jayne
    if(!is.list(classList)){
        errorCondition("classList needs to be in list form")
    }
    if(normalize){
        data = normalize(data)    
    }
    # add a column of classes
    class <- rep("unknown", length(data[,1]))
    for(i in seq(1, length(classList))){
        inds <- classList[[i]]
        class[inds] <- i # make every item equal to that number
    }
    data <- data.frame(data)
    data$class <- class
    # save the index of class column
    cl_indx <- dim(data)[2]
    # now check the data, 1 obs at a time against all others
    accuracy <- dim(data)[1]
    all_acc <- c()
    for(obs in 1:dim(data)[1]){
        sums <- rep(0, length(classList))
        classNum <-  0
        for(class in classList){
            # use the classes themselves as training norms
            classSum <- 0
            classNum <- classNum + 1
            for(train_obs in class){ # grab item from individual list
                temp <- t(data[obs, -cl_indx] - data[train_obs, -cl_indx])
                classSum <- classSum + exp(-(crossprod(temp)-1)/sigma^2)
            }
            sums[classNum] <- classSum/length(class)
        }
        assigned <- which(sums == max(sums)) # the assigned class
        if(assigned != data[obs,cl_indx]){
            accuracy = accuracy - 1
            all_acc = c(all_acc, 0)
        }else{
            all_acc = c(all_acc, 1)
        }
    }
    accuracy = accuracy/dim(data)[1] # convert accuracy to a percentage
    return(list(accuracy, all_acc))
}

normalize <- function(data){
    #'@param data a numeric data frame with dimenstions as cols and obs as rows
    #'performs a max-min normalization to bring into 0-1 range
    #'@return the normalized data frame
    #'@author Casey Jayne
    d <- dim(data)[2]
    mins <- apply(data, 2, min)
    maxs <- apply(data, 2, max)
    out <- t((t(data) - mins)/(maxs - mins))
    return(out)
}
extract_training <- function(data, train_amount=.2){
    #'@param data a numeric data frame or matrix with n observations as rows and D dimensions as columns
    #'@param train_amount a numeric amount between 0 and 1 that corresponds to the random proportion to use as training data
    #'@return a list containing the original data separated into training_dat and test_dat
    #'@author Casey Jayne
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

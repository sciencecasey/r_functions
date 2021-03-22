parz_window <- function(test_data, classList, spread, train_amt = .2){
    #'@param data a numeric data frame with n observations as rows and D dimesions as columns
    #'@param train_amount a numeric amount between 0 and 1 that corresponds to the random proportion to use as training data
    #'@param classList a list of index ranges separating the classes of the input data
    #'@param spread the size/spread of each kernel
    #'@return a dataframe with columns the test inputs and rows the summed gaus kernels across each training class
    # separate by classes
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
    kernels1 <- gaus_kernel(train[,-cl_indx], test[,-cl_indx], spread)
    # give classnames to kernels
    kernels1 <- data.frame(kernels1)
    colnames(kernels1) <- test$class
    kernels1$test_class <- train$class
    
    # take the sum across test classes
    p = data.frame()
    for(i in seq(1, length(classList))){
        class_sum <- colSums(kernels1[kernels1$test_class == as.character(i), -dim(kernels1)[2]])
        p <- rbind(p, class_sum)
    }
    # rename the cols to the test classes
    colnames(p) <- test$class
    return(p)
}

# 5 - fold cross validate to see if working correctly!
testing <- function(p, numClasses){
    classInd <- length(p[1,])/numClasses
    classed <- apply(p, 2, max)
    for(class in seq(1, numClasses)){
        for(i in seq(1, classInd)){
            if(names(classed[i]) != class){
                print("the {i} th item in class {class} is incorrectly valued.")
            }
        }    
    }
}

# exactly modeled off matlab
# THIS TEST AND TRAINING IS BACKWARDS
parz_window_og <- function(test_data, train_data, spread){
    #'@param test_data a numeric data frame or matrix with n observations as rows and D dimesions as columns
    #'@param train_data a numeric data frame or range to test the data against
    #'@param spread the size/spread of each kernel
    #'@return a matrix of parzen windows for each test data point
    # kernels1 <- gaus_kernel(train_data, test_data, spread)
    p = matrix(0, ncol = dim(train_data)[1])
    for(obs in seq(1, dim(test_data)[1])){
        kernel <- gaus_kernel(train = train_data, test = test_data[obs,], spread)
        p = p + kernel # this is correct for plotting but NOT for classification purposes
    }
    # p = colSums(p) # sum
    return(p)
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


gaus_kernel <- function(train, test, spread){
    #'@param test a matrix of test data with n obersvations as rows and D dimensions as columns
    #'@param train a matrix of training data with m obersvations as rows and D dimensions as columns
    #'@param spread the size/spread of each kernel
    #'@return a matrix of kernel of the size n (observation/rows train input) as rows X m (observations/rows test input) as columns
    n = dim(train)[1] # train observations/rows # the number of training data is what should be 
    D = dim(test)[2] # dimensions/features/columns
    K = matrix(0, nrow = dim(test)[1], ncol = n) # a test dim X training dim matrix
    for(i in seq(1, dim(train)[1])){ # across all training observations
        for(j in seq(1, dim(test)[1])){ 
            sub =  train[i,] - test[j,] 
            # subtract each test value from training value (can go either direction bc taking crossprod after)
            K[j,i] = (1/n)*(1/((sqrt(2*pi)*spread))^D)*
                exp(-.5*(tcrossprod(as.matrix(sub), as.matrix(sub)))/(spread^2)) 
        }
    }
    return(t(K))
}

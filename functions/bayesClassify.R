bayes_classifier.separated <- function(test, training, trainClassList, testClassList){
    #'@param test an observation X dimension (cols) data frame or matrix (fully numeric) to classify
    #'@param training an observation X dimension (cols) data frame or matrix (fully numeric) used to train the classifier
    #'@param trainClassList list of of indeces separating classes by row in the training data
    #'@param testClasslist an optional list of indeces separating classes by row (for checking accuracy)
    #'@return a list containing $assigned a vector of class assignments for inputs and $probabilitiesa matrix of posterior probabilities
    num_classes <- length(trainClassList)
    test_obs <- dim(test)[1]
    D <- dim(test)[2]
    
    # calculate posteriors for each test data
    posts <- matrix(rep(0, num_classes*test_obs), nrow = test_obs)
    require(pracma)
    classNum = 1
    for(class in trainClassList){
        #calculate the class priors
        meany = colMeans(training[class,]) #, na.rm = TRUE)
        covy = cov(training[class,]) #, use = "complete.obs")
        prior = length(class)/dim(training)[1]
        each_class = c() # will end up length of test items
        for(obs in 1:test_obs){
            # posteriors
            first = prior/((2*pi)^D*det(covy))
            secon = tcrossprod(as.numeric(test[obs,]-meany), pinv(covy))
            third = tcrossprod(secon, as.numeric(test[obs, ]-meany))
            P.Cj = first*exp(-.5*third)
            each_class = c(each_class, P.Cj)
        }
        posts[,classNum] = each_class
        classNum = classNum + 1
    }
    
    # add colnames for output clarity
    names = c()
    for(i in 1:num_classes) {
        name = paste("P.Class", i)
        names = c(names, name)
    }
    colnames(posts) = names
    assignment = apply(posts, 1, which.max)
    return(list(assigned = assignment, probabilities = posts, 
                tested = test, trained = training,
                train_classList = trainClassList,
                test_classList = testClassList))
}

bayes_classifier = function(data, classList, percentTrain = .8){
    #'@param data an obs X dimension dataframe or matrix (fully numeric)
    #'@param classList a list of indeces separating classes
    #'@param percentTrain the number of data to randomly subset as training and testing
    # Separate the training and testing
    
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
    train_classList <- list()
    test_classList <- list()
    cl_indx <- dim(data)[2] # save last row
    # randomly extract the training data
    # segregate train and test data
    for(class in seq(1, length(classList))){
        # extract training for each class
        t <- extract_training(data[classList[[class]],], percentTrain)
        train <- rbind(train, t[[1]])
        test <- rbind(test, t[[2]])
        # create classlists
        train_classList[[class]] <- which(train[, cl_indx] == class)
        test_classList[[class]] <- which(test[, cl_indx] == class)
    }  
    
    # pass to separated bayes
    return(bayes_classifier.separated(test = test[,-cl_indx], 
                                      training = train[,-cl_indx], 
                               trainClassList = train_classList,
                               testClassList = test_classList))
}

bayes_classifier.accuracy = function(classifier, test_vec = NULL, test_list = NULL){
    #'@param classifier a bayes classifier object
    #'@param test_vec a vector of true numeric classes
    #'@param test_list a list where each item contains the indeces for that class
    #'@return the percent correct as a decimal
    acc = dim(classifier$probabilities)[1]
    if(is.null(test_vec)){
        if(is.null(test_list)){
            errorCondition("Test list or test vector required")
        }else{
            test_vec <- rep(-1, length(classifier$assigned))
            for(i in 1:length(test_list)){
                test_vec[test_list[[i]]] = i # assign all indeces from the list to the class index
            }
        }
    }
    wrong = which(classifier$assigned != test_vec)
    acc = (acc - length(wrong))/acc
    return(acc)
}

extract_training <- function(data, train_amount =.8){
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

# testy <- rbind(iris[1:10, -5], iris[51:60, -5], iris[101:110, -5])
# indx <- as.integer(rownames(testy))
# trainy <- iris[-indx, -5]
# listy <- list(1:40, 41:80, 81:120)
# out <- bayes_classifier.separated(test = testy, training = trainy, trainClassList = listy)
# which(out$assigned != c(rep(1, 10), rep(2, 10), rep(3, 10))) # all accurately classified!




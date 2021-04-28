cross_validate <- function(classList, number_folds){
    #'@param classList a list of class indeces to sample from
    #'@param number_folds the number of cross validation separtions to create for the data
    #'@return a names list of matrices containing the indeces to use as test values for each pass through the validation, according to size of the data and the number of folds requested
    #'note that these are created without replacement so the size of the test set is equal to the length of the class*1/number_folds for each validation
    #'@author Casey Jayne
    cross_val_lists <- list()
    list_name = c()
    for(class_num in 1:length(classList)){
        class = classList[[class_num]]
        amount = 1/number_folds*(length(class))
        class_indxs <- matrix(data = rep(0, amount*number_folds), nrow = amount)
        cols = c()
        for(iteration in 1:number_folds){
            indx <- sample(class, amount)
            class_indxs[,iteration] = indx
            class = class[-indx] # remove the index until the last time
            cols = c(cols, paste0("validation ", iteration)) # create column name
        }
        colnames(class_indxs) = cols # add col names
        cross_val_lists[[class_num]] = class_indxs
        list_name = c(list_name, paste0("Class_", class_num)) # create matrix name
    }
    names(cross_val_lists) = list_name # add matrix names
    return(cross_val_lists)
}





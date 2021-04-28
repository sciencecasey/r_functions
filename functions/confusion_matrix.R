confusion_mat <- function(true_class, predicted_class){
    #'@param true_class a vector of true classes
    #'@param predicted_class a vector of predicted_classes to compare to true_class
    #'@return a list containing the confusion matrix, vectors counting false positive, false negative,
    #'true positive, true negative
    classes <- unique(true_class)
    num_classes <- length(classes)
    c_mat <- matrix(rep(0, num_classes^2), nrow = num_classes)
    col_names <- c()
    row_names <- c()
    for(i in 1:num_classes){
        TP <- predicted_class == classes[i] & true_class == classes[i]
        class_wrong <- rep(0, num_classes)
        j = 1
        while(j <= num_classes){
            if(j != i){
                class_wrong[j] = sum(predicted_class[true_class == classes[i]] == classes[j])
            }
            j = j+1
        }
        class_wrong[i] = sum(TP)
        c_mat[i,] = class_wrong # insert into the matrix
        col_names = c(col_names, paste(classes[i], "True Class"))
        row_names = c(row_names, paste(classes[i], "Predicted Class"))
    }
    colnames(c_mat) <- col_names
    rownames(c_mat) <- row_names
    tp <- diag(c_mat)
    fp <- colSums(c_mat) - tp
    fn <- rowSums(c_mat) - tp
    tn <- sum(c_mat)-tp-fp
    names(tp) <- classes
    names(fp) <- classes
    names(fn) <- classes
    names(tn) <- classes
    accuracy <- (tp + tn)/(tp+fp+tn+fn)
    precision <- tp/(tp+fp)
    recall <- tp/(tp+fn)
    F1 <- 2/((1/recall) + (1/precision))
    cs <- data.frame(accuracy, precision, recall, F1)
    cs <- colMeans(cs)
    return(list(confusion_matrx = c_mat,
           true_positive = tp,
           false_positive = fp,
           true_negative = tn,
           false_negative = fn,
           confusion_statistics  = cs))
}
# true <- c(rep(1,10), rep(2, 10), rep(3, 10))
# pred <- c(rep(1,9), 2, rep(2,9), 3, rep(3,9), 1)
# confusion_mat(true, true)
# confusion_mat(true, pred)
# pred <- c(rep(1,13), rep(3,4), rep(2,13))
# confusion_mat(true, pred)


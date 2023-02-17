ordered_cat <- function(category){
    #' use this with an apply function if you'd like the result across all columns: sapply(df,ordered_cat)
    #'@param category a factor vector where levels(category) returns the levels in ascending order
    #'@return a list including frequency=count per category, na_count=count of na's, median=middlest response when put in order by category level
    frq <- summary(category)
    least_frequent <- which(min(frq)==frq)
    least_frequent <- levels(category)[least_frequent]
    most_frequent <- which(max(frq)==frq)
    most_frequent <- levels(category)[most_frequent]
    sorted <- category[order(category)] # sort the values by ordered factor levels
    na_count <- sum(is.na(category))
    sorted <- sorted[-(which(is.na(category)))] # remove any na values
    mid <- floor(length(sorted)/2) # avg of 2 doesn't make sense for categories
    med <- sorted[mid]
    return(list(most_freq = most_frequent, least_freq = least_frequent,sorting = levels(category), na_count=na_count, median_response=med))    
}
checkdate <- function(date_vec, col_name=NULL){
    #' checks that there are 2 digits for month and day, if not adds a 0 padding, converts to date format to check that the date is valid (ie: no leap year), adds pad_month, pad_day, invalid_format items to log list with numerics for the location changed in vector
    #' @param date a vector of dates in character format m/d/y
    #' @return a list containing the changed vector and log of changes
    #' @author Casey Jayne

    log <- list()
    # 2-digit months
    mo <- grepl("^[0-9][0-9]/", date_vec)
    if(length(date_vec[!mo])>0){
        log$pad_month <- which(!mo)
        date_vec[!mo] <- paste0("0", date_vec[!mo])
        # Check it worked
        mo <- grepl("^[0-9][0-9]/", date_vec)
        if(length(date_vec[!mo])>0){
            errorCondition("Month padding error")
            return(list(new_dates=date_vec,
                        change_log=log,
                        known_errors_at=date_vec[!mo]))
        }
    }
    rm(mo) #cleanup

    # make day 2-digit
    day <- grepl("^[0-9][0-9]/[0-9][0-9]/", date_vec)
    if(length(date_vec[!day])>0){
        log$pad_day = which(!day)
        t <- sapply(date_vec[!day],
                    function(x) paste(read.fwf(textConnection(x),
                                               c(3, nchar(x)),
                                               as.is=T), collapse='0'))
        date_vec[!day] <- unname(t)
        rm(t)
        day <- grepl("^[0-9][0-9]/[0-9][0-9]/", date_vec)
        if(length(date_vec[!day])>0){
            errorCondition("Day padding error")
            return(list(new_dates=date_vec,
                        change_log=log,
                        known_errors_at=date_vec[!day]))
        }
    }
    rm(day)

    # make sure no 4-digit years
    longyear <- grepl("[0-9][0-9]/[0-9][0-9]/[0-9][0-9]$", date_vec)
    if(length(date_vec[!longyear])>0){
        # from inspection doesn't look like it
        warning("Year too long write this function")
    }

    # check that the dates are valid dates (months and days exist)
    is.Date <- function(x) { # will make invalid dates NA
        return(as.Date(x, format = '%m/%d/%y'))
    }
    if(length(date_vec[is.na(is.Date(date_vec))])>0){
        notdate <- date_vec[is.na(is.Date(date_vec))] # get locations of invalid dates
        date_vec[notdate] <- NA
        log$invalid_date <- which(!notdate)
        rm(notdate)
        rm(is.Date)
    }

    return(list(new_dates=date_vec,
                change_log=log))
}
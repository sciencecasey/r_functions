redcap_codebook <- function(dict, 
                            field.name_column = '?..Variable...Field.Name',
                            choices_column = 'Choices..Calculations..OR.Slider.Labels'){
    #'@param dict a dictionary generated from redcap 
    #'@param field.name_column the fully qualified name of the column containing field.name (automatic redcap label is default) or the field names as a list
    #'@param choices_column the fully qualified name of the column containing the choices or the choices column as a list
    
    these <- which(dict$Field.Type=='checkbox'|dict$Field.Type=='yesno'|dict$Field.Type=='radio')
    codes <- list()
    for(i in these){
        name <- field.name_column
        opts <- strsplit(choices_column[i], split = ' | ')
        breaks <- which(unlist(opts)=='|')
        breaks <- c(breaks, length(unlist(opts))+1)
        codename <- unlist(opts)[c(1,breaks+1)]
        codename <- codename[-length(codename)] # remove the NA at end
        codename <- sapply(codename, function(.) substr(., 1, nchar(.)-1)) # remove the commas
        codename <- as.numeric(codename) # change to numbers

        start <- 1
        t <- unlist(opts)
        meanings <- c()
        for(end in breaks){
            meaning <- t[(start+1):(end-1)]
            meaning <- paste(meaning, sep = " ", collapse = " ")
            start <- end + 1
            meanings <- append(meanings, meaning)
        }
        names(meanings) <- codename
        codes[[name]] <- meanings
    }
    return(codes)
}


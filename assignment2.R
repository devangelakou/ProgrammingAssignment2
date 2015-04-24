makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {  ## changes the vector stored in the main function 
                x <<- y  ## substitutes x with y in the main function 
    
                ## restores to null the mean of m, 
                ## because the old mean of the old vector is not needed anymore. 
                ## The new mean needs to be recalculated through the function cachemean.
                m <<- NULL  
        }
        get <- function() x  ## returns the vector x stored in the main function 
        
        ## store the value of the input in a variable m into the main function makeVector (setmean)
        ## and return it (getmean).
        setmean <- function(mean) m <<- mean ## m is substituted by mean globally - mean is passed as an arg  
        getmean <- function() m 
        
        ## create a list with all functions 
        ## that permits us to pass all functions to cachemean 
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean() 
        
        ## verify the value m, stored previously with getmean, exists and is not NULL 
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)  ## not necessarily the real mean value 
        }
        data <- x$get()  ## vector stored with makeVector 
        m <- mean(data, ...)  ## actually calculate the mean 
        x$setmean(m)  ## stores the result to m so that it is returned 
        m
}


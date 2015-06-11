## The is my solution to Programming Assignment 2.

## The pair of function here will attempt to
## reterive a previous cached inveresed matrix or
## calculate the inverse matrix if the cached is
## not available.

## This function will create a special matrix that
## all for get and set the original and inverse of
## the matrix.

makeCacheMatrix <- function(original_matrix = matrix()) {

        ## Set the cached matrix to null
        cached_matrix <- NULL
        
        ## Function to store the original matrix
        set <- function(orig_mat) 
        {
                original_matrix <<- orig_mat;
                cached_matrix <<- NULL;
        }
        
        ## Function to return the original matrix
        get <- function()
        {
                return(original_matrix);
        }
        
        ## Function to store the cached matrix
        setInverse <- function(inv_mat)
        {
                cached_matrix <<- inv_mat;
        } 
        
        ## Function to return the cached matrix
        getInverse <- function()
        {
                return(cached_matrix);
        } 
        
        ## List of available function
        return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## This function will check for the cached of the
## inverse matrix first.  If the cached exists,
## it will return the cached value, else it will
## calculate the inverse matrix and stored it in
## the cached.

cacheSolve <- function(original_matrix = matrix()) {
        
        ## Attempt to retrieve pervious cached of the inverse matrix
        cached_matrix <- original_matrix$getInverse()
        
        ## If cached is found, return it.
        if(!is.null(cached_matrix)) 
        {
                return(cached_matrix)
        }
        
        ## Calculate the inverse of the matrix 
        cached_matrix <- solve(original_matrix$get())
        
        ## store it into the cache for future use
        original_matrix$setInverse(cached_matrix)
        
        ## Return the inverse of the matrix
        return(cached_matrix)
}

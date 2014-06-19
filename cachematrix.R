## Assignment - Caching the inverse of a Matrix

## find the inverse of a matrix and load the inverse matrix from cache if it has already been calculated 
## otherwise calculate the inverse of the matrix

##Assumption: Matrix supplied is always invertible



## makeCacheMatrix will cache the matrix

makeCacheMatrix <- function(x = matrix()) {
        ##initialize inverse matrix with NULL
        i <- NULL
        ## set the matrix and initialize the inverse matrix to NULL
        set <- function(y) { 
                x <<- y
                i <<- NULL
        }
        ## return the matrix
        get <- function() x
        ## set the inverse of the matrix
        setinverse <- function(solve) i <<- solve
        ## return the inverse matrix
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve will compute the inverse of matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## call getinverse method to get catched inverse matrix 
        i <- x$getinverse()
        ## check if the catched result for inverse matrix exist then return that
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if catched inverse matrix does not exists i.e. inverse has not been calculated
        ## read the matrix and store in 'data'
        data <- x$get()
        ## call the solve function to get the inverse of the matrix and store the inverse in 'i'
        i <- solve(data)
        ## save the inverse matrix for future use (cache)
        x$setinverse(i)
        ## Return the inverse matrix 'i' of 'x'
        i

}

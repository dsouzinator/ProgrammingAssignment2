# creates matrix object that can cache inverse to memory
makeCacheMatrix <- function(x = matrix()) {
Inv <- NULL
        set <- function(mat) {
                x <<- mat 
                Inv <<- NULL
        }
        #get the matrix
        get <- function() x
        # set the inverse
        setinverse <- function(Imat) Inv <<- Imat
        #get the inverse
        getinverse <- function() Inv
        #return list of method
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#computes inverse of matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Imat <- x$getinverse()
        #return inverse if already set
        if(!is.null(Imat)) {
                message("getting cached data")
                return(Imat)
        }
        #get matrix from object
        data <- x$get()
        #calculates inverse of matrix
        Imat <- solve(data)
        x$setinverse(Imat)
        Imat
}

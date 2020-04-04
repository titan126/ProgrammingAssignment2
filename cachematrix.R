#two functions are included in this file
#makeCacheMatrix creates an object that stores a matrix and its inverse
#cacheSolve returns the inverse of a matrix through caching or via the
#solve function

#this function creates a special list with functions to get the value of
#a matrix, set the value of a matrix, set the inverse of the matrix, and
#get the inverse of the matrix

makeCacheMatrix <- function(x){
    #initialize matrixInverse to NULL to store the inverse of the matrix
    matrixInverse <- NULL
    
    #allows user to reset the matrix stored in x
    set <- function(y){
        x <<- y
        matrixInverse <<- NULL
    }
    
    #returns the matrix stored in x
    get <- function() x
    
    #sets the inverse matrix
    setInverse <- function(inverse) matrixInverse <<- inverse
    
    #returns the inverse matrix
    getInverse <- function() matrixInverse
    
    #return everything as a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#cacheSolve accepts an object x from makeCacheMatrix
#additional arguments can be passed to the solve function
cacheSolve <- function(x, ...){
    #store getInverse from x in matrixInverse
    matrixInverse <- x$getInverse()
    
    #if matrixInverse is not null, it means we've already calculated the inverse
    #in that case, this statement tells the user cached data is being returned
    #and prints matrixInverse
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    
    #if matrixInverse is not null, get the matrix, computed the inverse,
    #store the inverse via setInverse, and print the inverse matrix
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() 
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {           
        inv = x$getinv()
        
        #Check if the inverse if already calculated
        if (!is.null(inv)){
                message("retrieving cached inforamtion")
                return(inv)
        }
        
        # Else, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}

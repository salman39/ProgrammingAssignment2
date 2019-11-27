## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## OBJECTIVES
        ##              I. seting the matrix
        ##              II. getting the matrix
        ##              III. setting the inverse
        ##              IV. getting the inverse
        
        inv = NULL
        set = function(y) {
                
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        Now:
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}



## Write a short comment describing this function
Testing and explanation:

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
To test out these functions. I have written a function called assignment2(), 
 that brings in any invertible matrix, finds its inverse twice with the help of above functions,
 and prints out the number of times it takes for both runs. 
 The first run takes longer than the second run because it finds
 the inverse while the second run is only a look-up from the cache.

 assignment2 = function(lucky){
        ## N.B. lucky is an invertible matrix
        
        tait = makeCacheMatrix(lucky)
        
        start.time = Sys.time()
        cacheSolve(tait)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(tait)
        dur = Sys.time() - start.time
        print(dur)
}

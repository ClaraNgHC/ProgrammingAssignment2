## Put comments here that give an overall description of what your
## functions do

##creating function a creates a special "Vector" that contains a function
##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inverse <<- inverse
      getinv <- function() inverse
      list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Checking fucntion which check to see if the mean has already been calculated.
## If already check will skips the computation, else it calculates the mean of 
##the data and sets the value of the mean in the cache via the set mean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinv()
      if(!is.null(inverse)) {
            message("getting cached data.")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinv(inverse)
      inverse
}

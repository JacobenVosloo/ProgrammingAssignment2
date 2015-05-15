## This function enables the user to calculate and save the
## inverse of a invertable matrix and save in cache for later use
## This is a time saving alternative to calcualting the inverse
## everytime it is needed


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            #This function saves the inverse after it is calculated
            x <<- y
            inverse <<- NULL
      }
      
      get <- function() x
      setInv <- function(solve) inverse <<-solve
      getInv <- function() inverse
      
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      # Check if the inverse exists in the cahce, if it does return it
      inv <- x$getInv()
      if (!is.null(inv)){
            message("Getting the cached data")
            return(inv)
      }
      # if the matrix has not yet been inversed and saved to the cache
      # the first set the inverse and return the inverse
      data <- x$get()
      inv <- solve(data, ...) # Calculate the inverse of the matrix
      x$setInv(inv) # save the inverse that we just calculated
      inv
}

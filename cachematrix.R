## Put comments here that give an overall description of what your
## functions do
##cacheSolve actually computes an inverted matrix and stores it
##makeCacheMatrix enables that storage and retrieval

## Write a short comment describing this function
##makeCacheMatrix defines the functions that can put the
##inverse matrix into storage once calculated
##and retrieve it to save the time of calculating it again
makeCacheMatrix <- function(x = matrix()) {
  inv_mx <- NULL
  set <- function(y) {
    x <<- y
    inv_mx <<- NULL
  }
  get <- function() x
  setim <- function(inv_matrix) inv_mx <<- inv_matrix
  getim <- function() inv_mx
  list(set = set, get = get,
       setim = setim,
       getim = getim)
}


## Write a short comment describing this function
#cacheSolve looks to see if an inverse matrix solution is already stored
#--if so, it returns that info
#--if not, it calculates the inverse matrix 
#--and then stores it for future retrieval
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mx <- x$getim()
#--don't recalculate it if it isn't null now. just retrieve it.
  if(!is.null(inv_mx)) {
    message("getting cached inverse matrix")
    return(inv_mx)
  }
#--if we get here, no inverse matrix was found and it's time to calc it
  data <- x$get()
#--here's the line that performs the calculation
  inv_mx <- solve(x)
#--this line puts that result into storage
  x$setim(inv_mx)
  inv_mx
}

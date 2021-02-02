## Put comments here that give an overall description of what your
## functions do
##cacheSolve actually computes an inverted matrix and stores it (IF it
##doesn't find an inverse matrix that already exists)
##makeCacheMatrix enables the storage and retrieval so the inverted matrix
##doesn't have to be recalculated each time
#--Together, they demonstrate lexical scoping (If a variable is not defined
##HERE, then look in the parent environment for that variable)

## Write a short comment describing this function
##makeCacheMatrix defines the functions that can put the
##inverse matrix into storage once calculated
##and retrieve it to save the time of calculating it again
makeCacheMatrix <- function(x = matrix()) {
  inv_mx <- NULL
#--set is the reset function. It replaces the initial matrix
#--and nulls out the inverse matrix so it will recalculate
  set <- function(y) {
    x <<- y
    inv_mx <<- NULL
  }
#--set_im moves the inv_matrix input into the inv_mx in the parent env  
  set_im <- function(inv_matrix) inv_mx <<- inv_matrix
#--the gets return variables from the parent ENV  
  get <- function() x
  get_im <- function() inv_mx
  
#--this assigns names to the above functions so we can call them with $
  list(set = set, get = get,
       set_im = set_im,
       get_im = get_im)
}


## Write a short comment describing this function
#cacheSolve looks to see if an inverse matrix solution is already stored
#--if so, it returns that info
#--if not, it calculates the inverse matrix 
#--and then stores it for future retrieval
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mx <- x$get_im()
#--don't recalculate it if it isn't null now. just retrieve it.
  if(!is.null(inv_mx)) {
    message("getting cached inverse matrix")
    return(inv_mx)
  }
#--if we get here, no inverse matrix was found and it's time to calc it
  input_matrix <- x$get()
#--here's the line that performs the calculation
  inv_mx <- solve(input_matrix)
#--this line puts that result into storage
  x$set_im(inv_mx)
#--finally, output the inverse matrix for the user
  inv_mx
}

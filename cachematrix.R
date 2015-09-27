## There are two functions created in this file.
## They are both intended to be used to save memory 
## and time during computations with numerical matrices
## involving matrix inversion.





## This function may take a numeric matrix as an aregument
## to initialize the matrix cache(x).
## This function returns a list of functions connected to
## cached memory that the can all access. There are two
## memories that can be cached. One holding the original 
## matrix (x). The other is meant for holding either NULL
## or the invers of x (inv). The functions defined in   
## makeCacheMatrix are used as accessors or mutators of
## cached values. The mutator for the matrix cache(x)
## resets the inverse cache(inv) to NULL to show that
## the new inverse must be computed.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solved) inv <<- solved
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}





## This function takes as an argument a variable
## of the same type of the return value of 
## makeCacheMatrix. It returns the inverse of the
## matrix recorded in the matrix cache. This function
## first checks if there is already a computed inverse
## in the inverse cache($getinv()) to return it. If not, it 
## computes the inverse of the matrix cache($get()),
## stores it in the inverse cache($getinv()) for possible
## later use, and returns it.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}

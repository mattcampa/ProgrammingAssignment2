## I want to write two functions: one that should create an object, that is 
## a list of function to set and access(get) certain values that characterize a matrix,
## here we can set and get the matrix itself, plus we have a parameter inv that stores
## the inverse matrix, that is set NULL when the function is called, and so it will be unless we 
## calculate the inverse of the matrix and we store the result in that variable.
## The second function simply check if the variable inv has been set, if that is the case it will
## just return what was already in that variable, if not it will calculate the inverse, put its value in
## inv and return inv

## This function will just create an object that is a matrix that can store it's inverse if calculated

makeCacheMatrix <- function(x = matrix()) { inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL}
  
  getMatrix <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv, getInv = getInv)

}


## This function just wants to return the inverse of the special matrix created with the previous function
## it will first check if the inverse has already been calculated, if it is not, it will calculate it

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)}
  
  xx <- x$getMatrix()
  inv <- solve(xx, ...)
  x$setInv(inv)
  inv
  
}

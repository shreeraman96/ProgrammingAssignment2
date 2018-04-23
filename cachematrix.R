##the script contains two function makeCacheMatrix and cacheSolve. makeCacheMatrix can be used to create and cache a matrix
##cacheSolve can be used to find the inverse of the matrix and cache the same if not.


## makeCacheMatrix function returns a list with four functions. setMatrix will cache the matrix to be inversed
## getMatrix function will return the matrix that has been cached.
## setInv function will cache the inverse of the matrix that has been cached already
## getInv function will return the cached inverse of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  cacheInv <- NULL
  setMatrix <- function(mat2cache) {
    cacheMat <<- mat2cache
    cacheInv <<- NULL
  }
  getMatrix <- function() cacheMat
  setInv <- function(inv2Cache) cacheInv <<- inv2Cache
  getInv <- function() cacheInv
  list(setMatrix = setMatrix,getMatrix = getMatrix,setInv = setInv,getInv = getInv)
}


## cachesolve will return if the inverse is already cached. if not the function will solve for the inverse 
## and cache the inverse and then return the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)){
    message("Cached Inv being returned")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data,...)
  message("Inverse getting cached")
  x$setInv(inv)
  inv
}

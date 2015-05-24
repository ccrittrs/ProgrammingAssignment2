## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(orMat = matrix()) {
  # create the empty matrix to hold the inverse
  invMat <- matix()
  
  # set the initial matrix
  set <- function(inMat){
    orMat <- inMat
    invMat <- matix()
  }
  
  # return originl matrix
  get <- function() orMat
  
  # create the inverted matrix
  setInv <- function(solve){
    invMat <<- solve
  } 
  
  # return the inverted matrix (or empty matrix 
  # if inverted matrix is not present)
  getInv <- function() invMat
  list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(orMat, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- orMat$getInv()
  
  # if invMat is not empty, we need to retrieve cached
  if (!is.empty(invMat)){
    message("getting cached data")
    return(invMat)
  } # else - we have to calculate it 
  
  # create temporary matrix to hold original data
  tempMat <- orMat$get()
  invMat <- solve(tempMat, ...)
  orMat$setInv(invMat)
  invMat
}








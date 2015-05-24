## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  makeCacheMatrix takes a matrix as an argument and 
#  caches that matrix, as well as providing methods to 
#  set, store and return both the original matrix and its inverse

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
#  cacheSolve accepts a matrix as an argument and then 
#  uses makeCacheMatrix (above) to determine if the input
#  matrix's inverse has already been determined and if not
#  it calculates that inverse and then caches it in
#  makeCacheMatrix.

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








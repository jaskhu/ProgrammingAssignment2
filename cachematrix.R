## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# R code to compute inverse of matrix by caching the data
makeCacheMatrix <- function(gettingdat = matrix()) {
  lime <- NULL
  set <- function(y) {
    gettingdat <<- y			#stores data
    lime <<- NULL
  }
  get <- function() gettingdat
  setinverse <- function(inverse) lime <<- inverse	#stores inversion data
  getinverse <- function() lime
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(gettingdat, ...) {
  lime <- gettingdat$getinverse()	#fetch already evaluated data
  if (!is.null(lime)) {
    message("getting the cached data from prev")		#checkif already evaluated
    return(lime)	#return inversion from cached data
  }
  
  data <- gettingdat$get()	#get the input data if not already evaluated
  lime <- solve(data, ...)		#evaluate Matrix Inversion
  gettingdat$setinverse(lime)	
  lime				#return the inversion
}
#Create a Matrix as A and input as A1 <- makeCacheMatrix(A) and then run cacheSolve(A1) to get your inversion matrix from cached data


## Put comments here that give an overall description of what your
## functions do
# creating matrix to store cachemean that includes four functions: set, get, get mean, set mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Write a short comment describing this function
# cachemean is necessary to retrieve and populate the mean from the object makeCacheMatrix

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#Function to compute the inverse of 'x'

CacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}

aMatrix <- makeCacheMatrix(1:10)
aMatrix$get()

aMatrix$getmean()
#reset value with a new vector
aMatrix$set(30:50)
cachemean(aMatrix)
aMatrix$getmean()
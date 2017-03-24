## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  print(x)
  setinverse <- function(matrix) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m<-solve(data)
  x$setinverse(m)
  m
}

## This function 
tester <- function(){
  ## create the test matrix
  #mattest = matrix(1:4,2,2)
  mattest = matrix(2:5,2,2)
  
  print("create the matrix that can cache its inverse")
  v <- makeCacheMatrix(mattest)
  
  print("set the inverse of the matrix")
  v$setinverse()
  print(v$getinverse())
  
  print("call cacheSolve which will retrieve the cached version")
  cachedmat <- cacheSolve(v)
  print(cachedmat)
  
  print("call cacheSolve so it will have to create the inverse")
  createdmat <- cacheSolve(makeCacheMatrix(mattest))
  print(createdmat)
}
  
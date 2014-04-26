## These pair of functions create a "matrix" object that allows caching. What it 
## does is create a matrix object with a set of functions tied to that object for 
## caching. The cache solve function finds the inverse of the matrix if it is not
## cached, and retrieves the cached value if it has been calculated already

## This function creates a special "matrix" object that can cache its inverse
## List contains the set of functions that are used for caching

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
## Example below
#B = matrix( 
#     c(2, 3, 2, 2), 
#     nrow=2, 
#     ncol=2) 
# ------------Output --------------
#       [,1] [,2]
#[1,]    2    2
#[2,]    3    2


#Bcache = makeCacheMatrix(B)


#cacheSolve(Bcache)
#------------output----------------
#> cacheSolve(Bcache)
#       [,1] [,2]
#[1,] -1.0    1
#[2,]  1.5   -1


#cacheSolve(Bcache)
#------------output----------------
#> cacheSolve(Bcache)
# getting cached data
#       [,1] [,2]
#[1,] -1.0    1
#[2,]  1.5   -1

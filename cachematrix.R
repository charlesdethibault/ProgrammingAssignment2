# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.



## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. the function will "list":
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix using solve function
## 4. get the value of inverse of the matrix using solve function

makeCacheMatrix <- function(x = matrix()) {
    ## initialize cached value to NULL
  inv <- NULL
    ## create the matrix
    ## use `<<-` to assign a value to an object in an environment 
    ## different from the current environment.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    ## get matrix value
  get <- function() x
    ## set inverse matrix
  setinverse <- function(inverse) inv <<- inverse
    ## get inverted matrix
  getinverse <- function() inv
    ## return functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##  cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## This function computes the inverse of the special "matrix" using solve function 
        ##returned by makeCacheMatrix above.
  inv <- x$getinv()
        ## return inverted matrix from cache if it exists
        ## else create the matrix in working environment
        ## note : if you run it twice the second run will show matrix from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
        ## calcute the inverse 
  data <- x$get()
  inv <- solve(data, ...)
        ## set the value of the inverse function
  x$setinv(inv)
  inv
}

## Sample run to test functions
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
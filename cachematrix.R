## cachematrix.R
## R Programming Coursera
## Programming Assignment 2
## fxcebx
## 2014-11-21
##
## Computing the inverse of a matrix
## - the matrix supplied has to be invertible, this is not checked
## - the matrix has to be quadratic
##
## How to use the functions see bottom
## 
## To avoid unneccesary time-consuming recalculation the matrix and its inverse
## is cached in different environment from the current environment
## If the inverse has already been calculated and the matrix is unchanged, 
## then the inverse of the matrix is retrieved from cache.

#### makeCacheMatrix() --------------------------------------------------------
## makeCacheMatrix creates a list containing functions to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## returns a list of functions
  ##   set = set 
  ##   get = get
  ##   setInverse = setInverse
  ##   getInverse = getInverse
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix (the caching)
  set <- function(y) {
    x <<- y          # assign to different environment from current environment
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() {
    x
  }
  
  ## Method to set the inverse (the caching)
  setinverse <- function(inverse) {
    inv <<- inverse  # assign to different environment from current environment
  } 
  
  ## Method to get the inverse (cached)
  getinverse <- function() {
    inv
  }
  
  # return list
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

#### cacheSolve() -------------------------------------------------------------
## cacheSolve returns the inverse of a matrix x 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache with
## the setinverse function.
## The matrix given has to be invertible, thats not checked here.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  ## the matrix supplied has to be invertible, not checked

  inv <- x$getinverse()
  ## return the inverse if its already set
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## otherwise computation is needed
  ## Get the matrix from our object
  data <- x$get()
  
  ## compute the inverse
  inv <- solve(data)
  
  ## Set the inverse to the object (caching)
  x$setinverse(inv)
  
  ## return the inverse
  inv
}

#### Sample run ---------------------------------------------------------------
# > source("cachematrix.R")
# > x <- rbind(c(1, 1/3, 1/4), c(1/4, 1, -1/3), c(1/4, -1/3, 1))
# > solve(x)
# [,1]  [,2]  [,3]
# [1,]  1.28 -0.60 -0.52
# [2,] -0.48  1.35  0.57
# [3,] -0.48  0.60  1.32

# > m <- makeCacheMatrix(x)
## matrix x now in cache
# > m$get()
# [,1]       [,2]       [,3]
# [1,] 1.00  0.3333333  0.2500000
# [2,] 0.25  1.0000000 -0.3333333
# [3,] 0.25 -0.3333333  1.0000000

## inverse of x not yet cached
# > m$getinverse()
# NULL

## No cache:  1st run
# > cacheSolve(m)
# [,1]  [,2]  [,3]
# [1,]  1.28 -0.60 -0.52
# [2,] -0.48  1.35  0.57
# [3,] -0.48  0.60  1.32

## now already calculated and cached
# > m$getinverse()
# [,1]  [,2]  [,3]
# [1,]  1.28 -0.60 -0.52
# [2,] -0.48  1.35  0.57
# [3,] -0.48  0.60  1.32

## Retrieving from cache: 2nd run
# > cacheSolve(m)
# getting cached data.
# [,1]  [,2]  [,3]
# [1,]  1.28 -0.60 -0.52
# [2,] -0.48  1.35  0.57
# [3,] -0.48  0.60  1.32


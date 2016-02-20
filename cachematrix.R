## We are creating two functions in R.
## 1. makeCacheMatrix, 2. cacheSolve 
## Both these functions are used to get the inverse of the matrix and then to cache them for future use
## as matrix inversion is an expensive process.

## # 1. Set the value of the passed matrix
## # 2. Get the value of the passed matrix
## # 3. Set the inverse of the passed matrix
## # 4. Get the inverse of the passed matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  
  get <- function() x 
  
  setinverse <- function(inverse) inv <<- inverse 
  
  getinverse <- function() inv 
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() 
  if(!is.null(inv)) { 
    message("reading from cache...") 
    return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data) 
  x$setinverse(inv) 
  inv 
  
}

# inverse matrix results verified from http://www.purplemath.com/modules/mtrxinvr.htm
# s <- matrix(c(1,3,3,1,4,3,1,3,4), nrow=3, ncol=3,byrow = TRUE)
# s
# m = makeCacheMatrix(s) 
# m$get()
# 
# cacheSolve(m) 

## Test Data    
#     > s <- matrix(c(1,3,3,1,4,3,1,3,4), nrow=3, ncol=3,byrow = TRUE)
#     > s
#     [,1] [,2] [,3]
#     [1,]    1    3    3
#     [2,]    1    4    3
#     [3,]    1    3    4
#     > m = makeCacheMatrix(s)
#     > m$get()
#     [,1] [,2] [,3]
#     [1,]    1    3    3
#     [2,]    1    4    3
#     [3,]    1    3    4
#     > cacheSolve(m)
#     [,1] [,2] [,3]
#     [1,]    7   -3   -3
#     [2,]   -1    1    0
#     [3,]   -1    0    1
#     >
## Test Data    

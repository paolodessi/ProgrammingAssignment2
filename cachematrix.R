##The following functions are used to cache the inverse of a matrix.
## The first function creates an object(matrix) that can cache its inverse.


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

## 2nd function calculates the inverse of the matrix created previously. When the inverse has already been calculated
## the functin retrieve the inverse from the cache (message("ok; data from cache")).


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("ok; data from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

###test

#> source("chachematrix.R")
#> x <- matrix(1:4, 2, 2) ; print(x)

#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#> solv <- cacheSolve(m); print(solv)
   
#  [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#> solv2 <- cacheSolve(m); print(solv2)

#ok; data from cache
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5



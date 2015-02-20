## Compute the Inverse of a matrix, return computational results from cache
## if the cache contains the results, if not compute the result and store it
## in memory/cache

##@makeCacheMatrix: <args> Matrix
##@desc: Create a special list of functions  to get/set matrix
## to get/set inverse of the matrix
makeCacheMatrix<- function(x = matrix()) {
        matrixInverse <- NULL
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrixInverse <<- inverse
        getInverse <- function() matrixInverse 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##@cacheSolve: @ args : matrix
##@desc: Calculate inverse of the special list, get the computational result
## for cache hits else compute the result for cache miss and store in cache
 
cacheSolve<- function(x, ...) {
        matrixInverse <- x$getInverse() ## Get Inverse 
        if(!is.null(matrixInverse)) { ## check if Inverse exists
                message("getting matrix inverse from cache")
                return(matrixInverse) 
        }
        data <- x$get() ## Since Inverse is not in cache, get input matrix data
        matrixInverse <- solve(data, ...) ## Use Solve to compute Inverse
        x$setInverse(matrixInverse) ## Set Inverse in Cache
        matrixInverse ## Return Inverse of the matrix x
}

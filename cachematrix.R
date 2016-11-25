## This pair of functions cache the inverse of a matrix
## or solves it if it isn't cached.
## copyright Eric Chow 2016.

## this function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## returns a list of 4 functions that:
	## 1. set the value of the matrix
	## 2. get the value of the matrix
	## 3. set the value of the inverse
	## 4. get the value of the inverse
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## this function computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse
## has already been calculated (and the matrix has not changed)
#3 then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i	
}



# test code -------------

# an invertible matrix
A <- matrix(c(2,2,3,2),2,2)
solve(A)

# the identity matrix!
A %*% solve(A)

# test if caches or solves
matrix_A <- makeCacheMatrix(A)
cacheSolve(matrix_A)


#        ~ fin ~
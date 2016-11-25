## This pair of functions cache the inverse of a matrix
## or solves it if it isn't cached.
## copyright Eric Chow 2016.

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(i)
    m	
}



# test code -------------

# an invertible matrix
A <- matrix(c(2,2,3,2),2,2)
solve(A)

# the identity matrix!
A %*% solve(A)

# should return inverse of A from cache, or solve if not in cache
cacheSolve(makeCacheMatrix(A))
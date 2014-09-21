##  These functions assist in caching the inverse of a matrix (The format of these functions is based off the example mean functions given)

## The first function is geared to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## Initialize inverse property
x <- NULL
        ##Set matrix
		set <- function(matrix) {
                m <<- matrix
                x <<- NULL
        }
		##Get matrix
        get <- function() m
        
		## Set the inverse of the matrix
		setInverse <- function(inverse) x <<- Inverse
        
		## Get the inverse of the matrix
		getInverse <- function() x
        
		##List methods
		list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The second function is geared to compute the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInverse()
        
		## Return the inverse if its already already been calculated
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
		## Get the matrix
		data <- x$get()
        
		## Calculate the inverse
		m <- solve(data) %*% data
		
		## Set inverse
        x$setInverse(m)
		
		## Give resulting matrix
        m
}

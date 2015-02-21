    ##Assignment 2: Caching the Inverse of a Matrix
	
	#  This function creates a special "matrix" object
	#  that can cache its inverse.

	# The'makeCacheMatrix' creates a "matrix", and contains
	# a list of functions that will:

	# 1.  set the value of the matrix
	# 2.  get the value of the matrix
	# 3.  set the value of the inverse
	# 4.  get the value of the inverse


	makeCacheMatrix <- function(x = matrix()) {
				 inver <- NULL
				set <- function(y) {
						x <<- y
						inver <<- NULL
				}
				get <- function() x
				setinv <- function(solve) inver <<- solve
				getinv <- function() inver
				list(set = set, get = get,
					 setinv = setinv,
					 getinv = getinv)
	}


	# The 'cacheSolve' function will calculate the inverse of the matrix created by the 
	# 'makeCacheMatrix' function above. If the inverse has been calculated previously, it will extract or get
	# the inverse from the cache instead of re-computing it. If the inverse has not been calculated
	# previously, the function will calculate the inverse of the data and set the inverse value in the cache
	# using the 'setinver' function.

	cacheSolve <- function(x, ...) {
			## Return a matrix that is the inverse of 'x'
			inver <- x$getinv()
				if(!is.null(inver)) {
						message("getting cached data")
						return(inver)
				}
				data <- x$get()
				inver <- solve(data)
				x$setinv(inver)
				inver
	}

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y){
	x <<- y
  	m <<- NULL
	}
	get <- function() x
	setm_inverse <- function(solve) m<<- solve
	getm_inverse <- function() m
	list(set = set, get = get,
   		setm_inverse = setm_inverse,
   		getm_inverse = getm_inverse)
}

cacheSolve <- function(x = matrix(), ...) {
    	m <- x$getm_inverse()
    	if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    	}
    	matrix <- x$get() 
    	m <- solve(matrix, ...)
    	x$setm_inverse(m)
    	m
}

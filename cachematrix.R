#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
	cache <- NULL
	setMatrix <- function(newValue) 
	{
		x <<- newValue
		cache <<- NULL
	}
	getMatrix <- function() 
	{
		x
	}
	cacheInverse <- function(solve) 
	{
		cache <<- solve
	}
	getInverse <- function() 
	{
		cache
	}

	list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(y, ...) 
{
	inverse <- y$getInverse()
	if(!is.null(inverse)) 
	{
		message("getting cached data")
		return(inverse)
	}
	data <- y$getMatrix()
	inverse <- solve(data)
	y$cacheInverse(inverse)
	inverse
}

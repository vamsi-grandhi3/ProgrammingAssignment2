## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is to store in cache the matrix that is generated when running the cacheSolve function
## so, next time when cacheSolve function is run, it does not calculate the inverse, but fetches inverse from Cache

makeCacheMatrix <- function(x = matrix()) {
				m <- NULL
				set <- function(y) {
					x <<- y
					m <<- NULL
					}
				get <- function() x
				setinverse <- function(inverse) m <<- inverse
				getinverse <- function() m
				list(set=set,get=get,
					setinverse = setinverse,
					getinverse = getinverse)
}


## cacheSolve tries to fetch the inversematrix using getinverse, but if it receives a NULL, it computes the inverse
## if it was able to retrieve the inverse which is not NULL, it directly displays the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
			
			m <- x$getinverse()
			if(!is.null(m)) {
					print("getting cached data")
					return(m)
				}
			data <- x$get()
			m <- solve(data,...)
			x$setinverse(m)
			m
}

x <- matrix(c(1,1,4,0,3,1,4,4,0),3,3)
matrix <- makeCacheMatrix(x)
cacheSolve(my_matrix)
cacheSolve(my_matrix)	#you would observe that this time, the inverse is fetched directly from cache
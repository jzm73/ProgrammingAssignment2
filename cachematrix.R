## The function create a list with 4 elements, the first and second elements have the matrix parameter
## the third and four elements have the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- x
        get <- function() x 
	m<- solve(x)
        setinv <- m
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## call function makeCacheMatrix and assign list to variavel malist
malist<-makeCacheMatrix(x)

## The function has 03 paths: first validate if matrix parameter is the same as stored with last 
## call of makeCacheMatrix and if the value of the inverse was calculated, then return value from cache
## Second path calculate the value of the inverse
## Third path calculate a new matrix inverse, when the matrix parameter differ from matrix stored
## with makeCacheMatrix

cacheSolve <- function(x, ...) {
   	m <- malist$get()
        if(all.equal(m,x)==TRUE) {
        	m <- malist$getinv()
        	if(!is.null(m)) {
                	message("getting matrix inverse from cache")
                	return(m)
        	}
		message("calculate matrix inverse")
        	data <- malist$get()
        	m <- solve(data, ...)
        	x$setinv(m)
        	return(m)
	}
        message("matrix differ")
	minv<-solve(x)
        minv     ## Return a matrix that is the inverse of 'x'
}

## Assuming the matrix 'x' is always reversible,
## these functions would cache the inverse matrix to save double counting time.

## makeCacheMatrix() would create a special object 'CacheMatrix' that can cache the inverse of 'x'. 
## setsolve() would set the inverse to the special object.
## getsolve() would get the inverse from the special object.

makeCacheMatrix <- function(x = matrix()) {
	ans<-NULL
	set<-function(y){
		x<<-y
		ans<<-NULL
	}
	get<-function() x
	setsolve<-function(solve) ans<<-solve
	getsolve<-function() ans
	list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## cacheSolve() would determine whether the inverse of 'x' have been cached.
## If the inverse have been cached, the cached result would be returned.
## If the inverse have not been cached, the inverse would be calculated, and the result would be cached and returned.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	ans<-x$getsolve()
	if(!is.null(ans)){
		message("getting cached data")
		return(ans)
	}
	data<-x$get()
	ans<-solve(data, ...)
	x$setsolve(ans)
	ans
}

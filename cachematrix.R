## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. The two pairs of functions below are created to cache the inverse of a matrix.

## the first function makeCacheMatrix creates a "matrix object" to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv<-null
	set<-function(y){
		x<<-y
		inv<<-null
	}
    get<-function() x
    setInverse<-function(inverse) inv<<-inverse
    getInverse<-function() inv
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve function computes the inverse of the "matrix object" returned by makeCacheMatrix above. If the inverse has been calculated, the cacheSolve obtain the inverse from the Cache. Otherwise, the solve function is used to return its reverse and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        else
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}

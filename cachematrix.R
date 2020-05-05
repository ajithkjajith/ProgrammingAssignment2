## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y)
	{
		x<<-y
		inv <<-NULL
	}
	get<-function(){x}
	setinv<-function(inverse){inv<<-inverse}
	getinv<-function(){inv}

	list(set=set,get=get,
		setinv=setinv,
		getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getInvers()##invers
	if(!is.null(m))
	{
		message("cache data")
		return(m)
	}
	
	data<-x$get() ##matrix from our object

	m<-solve(data) %*% data ##inverse using matrix multiplication

	x$setInverse(m) ##settin inverse

	m

}

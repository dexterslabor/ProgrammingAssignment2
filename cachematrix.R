## Programming Assignment 2: Lexical Scoping
## for the "R Programming" lessons at coursera

## makeCacheMatrix creates a matrix object that can cache its inverse
## this function requires a matrix as input that is always invertible

makeCacheMatrix <- function(x = matrix()) {
	inv_m<-NULL  #variable for inverse matrix
		set<-function(y){ #modify existing matrix
			x <<- y
			inv_m<<-NULL
		}
		
		get <-function() x #returns matrix
		setinverse <- function(inverse_matrix) inv_m<<-inverse_matrix #save inverse matrix
		getinverse <- function() inv_m #return inverse matrix
		list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)	
}


## computes, caches and returns matrix inverse using the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   	inv_m <- x$getinverse() #get already computed inverse
    	 if(!is.null(inv_m)){ 
     		message("getting cached data")
	     	return(inv_m) #returns cached matrix inverse using previously computed matrix inverse
  		 }
	 	data <- x$get()   #cache matrix
     	inv_m <- solve(data,...) #computes inverse
	 	x$setinverse(inv_m) #saves inverse
     	inv_m #returns inverse matrix
}

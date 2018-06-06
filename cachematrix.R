# C2W3assignment
## The makeCacheAtrix function and the cachesolve function can only work by pair
## functions do

## this function aims at preparing the cacheSolve function to work.

## It is composed of 4 functions that can be called by name like 'x$getinverse'
# the set function is to reset the concerned matrix without recalling the function
# the get function is to return the concerned matrix
# the setinverse function will be use later to enter the inverse matrix once calculated (to cache it)
# the getinverse is to read the actual inverse if it has been calculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #once you call the matrix you don't know its inverse as you didn't calculated it 
  set <- function(y) {
    x <<- y
    i <<- NULL
    #of course if you reset the matrix, its inverse is not the same anymore
    
    # the <<- sign is to assign the value outside the environment of this subfunction
    # to the parent environment which is the makeCachematrix function.
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  #this writing maybe strange but the 2 objects at the different sides of the = sign are not the same :
  # name = function in order to call the function with $
}


## this function can only work with a matrix set by the makeCacheMatrix function.
#It will firs check if the inverse has already been calculated
#if yes it will return the value got from the makeCacheMatrix
#otherwise it will calculate the inverse, return it to the cache and print it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    #first the cachesolve function check wether the inverse is not in the cache memory already
    message("getting cached data")
    return(i)
    #here the inverse is already on the cache (in makeCacheMAtric function)
  }
  data <- x$get()
  i <- solve(data, ...)
  #at this step, as i was null, the Solve calculation has to be done
  x$setinverse(i)
  #the inverse is return to the makeCacheMatrix function :
  # the inverse is in the cache
  i
  #the value of the inverse is printed in the console
  
  
}

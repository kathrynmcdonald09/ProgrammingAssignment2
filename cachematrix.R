## Put comments here that give an overall description of what your
## functions do

##Background info:
##This is the makeVector function that was provided.
##it creates a special vector (or list containing a function).
> makeVector <- function(x = numeric()) {
    m <- NULL
   set <- function(y) {
         x <<- y
         m <<- NULL
 }
   get <- function() x
   setmean <- function(mean) m <<- mean
   getmean <- function() m
   list(set = set, get = get,
                setmean = setmean,
                getmean = getmean)
}

##This is the cachemean function that was provided.
##It calculates the mean of the special vector, "makeVector".
##It first checks if mean was previously calculated.
##If it wasn't, then it calculates the mean.
> cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
 }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
 }

##Question 1:
## Write a short comment describing this function
##This function builds a matrix that can cache its inverse (input).
##I used the '<<-' to assign values to x and inver.
makeCacheMatrix<-function(x=matrix()){
  inver<-NULL
  set<-function(y){
    x<<-y
    inver<<-NULL
  }
  get<-function()x
  setinver<-function(inver) inver <<-inver
  getinver<-function() inver
  list(set = set,
       get = get,
       setinver = setinver,
       getinver = getinver)
  }



##Question 2:
## Write a short comment describing this function
##This function should compute the inverse of the matrix creaded by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inver<-x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data<-x$get()
  inver<-solve(data, ...)
  x$setinver(inver)
  inver
}

##To test:
##I created a 2x2 matrix using 4 random numbers
m<-matrix(rnorm(4),2,2)
m
m1<-makeCacheMatrix(m)
##m1 should create a special matrix of m using the function above.
cacheSolve(m1)
##The casheSolve function returns a matrix which is the inverse of matrix 'm'.
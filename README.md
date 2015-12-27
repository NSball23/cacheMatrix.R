# cacheMatrix.R
eCacheMatrix <- function(X = matrix())
+ {
+ N <- NULL
+ set <- function(Z)
+ {
+ X <<- Z
+ N <<- NULL
+ }
+ get <- function() X
+ setinv <- function(Z) N <<- Z
+ getinv <- function() N
+ list(set = set, get = get, setinv = setinv, getinv = getinv)
+ }
> # This ought to return the inverse of the matrix of the first
> #function. I think if the inverse of N and X are NULL than
> # it will recalculate the inverse of X.
> cacheSolve <- function(X, ...)
+ {
+ N <- X$getinv()
+ if (is.null(N)) {
+ message('Computing inverse...')
+ date <- X$get()
+ N <- solve(date, ...)
+ X$setinv(N)
+ } else {
+ message('Returning cached inverse...')
+ }
+ return(N)
+ }
> X <- makeCacheMatrix()
> set.seed(1)
> X$set(matrix(runif(9, -1, 1), 3))
> cacheSolve(X)
Computing inverse...
           [,1]      [,2]      [,3]
[1,] -2.2708332  2.755019  4.389781
[2,]  0.6248449 -1.387424 -0.424108
[3,] -0.6466679  2.726451  2.704198
> cacheSolve(X)
Returning cached inverse...
           [,1]      [,2]      [,3]
[1,] -2.2708332  2.755019  4.389781
[2,]  0.6248449 -1.387424 -0.424108
[3,] -0.6466679  2.726451  2.704198
> X$set(matrix(runif(9, -1, 1, 3))
+ cacheSolve(X)
Error: unexpected symbol in:
"X$set(matrix(runif(9, -1, 1, 3))
cacheSolve"
> X$set(matrix(runif(9, -1, 1), 3))
> cacheSolve(X)
Computing inverse...
           [,1]       [,2]      [,3]
[1,] -0.8920343 -0.7138776 0.3116461
[2,]  0.5722648 -1.6672120 0.7402486
[3,] -0.9004629  0.4451743 0.8152981

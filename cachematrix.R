## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

+#validate matrix can be inversed
+testInverseWorks <- function() {
+    mm <- matrix(c(1,2,2,1), nrow=2, ncol=2, byrow=TRUE)
+    print("Original matrix:")
+    print(mm)
+    print("inverse:")
+    print(cacheSolve(makeCacheMatrix(mm)))
+}
 
-makeCacheMatrix <- function(x = matrix()) {
+#Validate cached value is returned on if inverse is called more than once
+testInverseIsCached <- function() {
+    mm <- matrix(c(1,2,2,1), nrow=2, ncol=2, byrow=TRUE)
+    print("Original matrix:")
+    print(mm)
+    print("inverse:")
+    mc <- makeCacheMatrix(mm)
+    print(cacheSolve(mc))
+    print(cacheSolve(mc))
+}
 
+#validate cache value is not returned for modified/new matrix
+testInverseNotCachedForModifiedMatrix <- function() {
+    mm <- matrix(c(1,2,2,1), nrow=2, ncol=2, byrow=TRUE)
+    mc <- makeCacheMatrix(mm)
+    inverse1 <- cacheSolve(mc)
+    mm <- matrix(c(1,3,3,1), nrow=2, ncol=2, byrow=TRUE)
+    mc <- makeCacheMatrix(mm)
+    inverse2 <- cacheSolve(mc)
+    print(paste("Identical matrix: ", identical(inverse1, inverse2), collapse=" "))
 }
 
+## function to create get and setter for matrix and its inverse
+makeCacheMatrix <- function(x = matrix()) {
+    inverse <- NULL
+    setMatrix <- function(y) {
+        x <<- y
+        inverse <<- NULL
+    }
+    getMatrix <- function() {
+        x
+    }
+    setInverse <- function(inverseM) {
+        inverse <<- inverseM
+    }
+    getInverse <- function() {
+        inverse
+    }
+    
+    list(setMatrix = setMatrix, getMatrix = getMatrix,
+         setInverse = setInverse, getInverse = getInverse)
+}
 
-## Write a short comment describing this function
 
+##function to cache inverse of matrix and saves the inverted matrix in a cache for further use.
 cacheSolve <- function(x, ...) {
-        ## Return a matrix that is the inverse of 'x'
+    ## Return a matrix that is the inverse of 'x'
+    inverse <- x$getInverse()
+    if(!is.null(inverse)) {
+        message("getting cached data")
+        return(inverse)
+    }
+    dataMatrix <- x$getMatrix()
+    inverse <- solve(dataMatrix)
+    x$setInverse(inverse)
+    inverse
 }
}

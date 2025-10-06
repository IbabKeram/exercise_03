#  Task 2 

IndexOfMin <- function(array, first, last) {
  index <- first
  for (k in (first + 1):last) {
    if (array[k] < array[index]) {
      index <- k
    }
  }
  return(index)
}

arr <- c(2,4,7,5,10,1)
print(IndexOfMin(arr, 1, 6))
# estimated number of operations is 2l + 3 or (worst case scenario) 4l+3

#Task 3

SelectionSort <- function (array, n) {
  for (i in 1:(n-1)) {
    j <- IndexOfMin(array, i, n)
    docas <- array[i]
    array[i] <- array[j]
    array[j] <- docas
  }
  return(array)
}

print(SelectionSort(arr,6))

# estimated number of operations is 6n-4
#the estimated O notation is O(n)


#Task 4 
RecursiveSelectionSort <- function(array, first, last) {
  if (first <last) {
    index <- IndexOfMin(array, first, last)
    docas <- array[index]
    array[index] <- array[first]
    array[first] <- docas
    array <- RecursiveSelectionSort(array, first + 1, last)
    
  }
  return(array)
}

print(RecursiveSelectionSort(arr, 1,6))

# the estimated number of operations is 5l or 0


#   Defunct as of version 1.0.3.9002 due to internal use of data.table
#   Handles recurrent events by appending a sequence number.
#   Example: A patient with two "SHK" events will get "SHK1" and "SHK2".
#   Optimization: Replaces the previous 'for loop' with a vectorized 'ave' approach.

nam <- function(X) {
  .Defunct()
}

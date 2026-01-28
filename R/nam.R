
#   Handles recurrent events by appending a sequence number.
#   Example: A patient with two "SHK" events will get "SHK1" and "SHK2".
#   Optimization: Replaces the previous 'for loop' with a vectorized 'ave' approach.

nam <- function(X) {
  # Generate sequence numbers by grouping identical events
  seq_nums <- ave(as.character(X), as.character(X), FUN = seq_along)
  paste0(X, seq_nums)
}

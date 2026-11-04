# Internal: validate a split wcep object and return its two groups in group_n order
.wcep_split_groups <- function(x) {

  if (!inherits(x, "wcep"))
    stop("`x` must be a 'wcep' object returned by wcep() with split = TRUE.", call. = FALSE)
  if (length(x) != 2L)
    stop("`x` must contain exactly two groups (was wcep() run with split = TRUE?).", call. = FALSE)

  nms <- names(x)
  gn <- vapply(seq_len(2), function(i) as.numeric(x[[i]]$group_n)[1], numeric(1))
  if (anyNA(gn) || !setequal(gn, c(1, 2)))
    stop("`x` must contain two groups with group_n 1 and 2 (was wcep() run with split = TRUE?).",
         call. = FALSE)

  i1 <- which(gn == 1)
  i2 <- which(gn == 2)
  list(
    g1     = x[[i1]],
    g2     = x[[i2]],
    labels = c("Comparison Group" = nms[i1], "Reference Group" = nms[i2])
  )
}

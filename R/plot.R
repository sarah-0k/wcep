#' wcep plot
#'
#' Create a plot of Kaplan-Meier-like curve(s) with confidence intervals.
#' @param x is an object of class "wcep"
#' @param main title of plot
#' @param type type of plot
#' @param lty line type
#' @param lwd line width
#' @param xlab first axis label
#' @param ylab second axis label
#' @param xlim first axis limits
#' @param ylim second axis limits
#' @param cex legend font size
#' @param linecol color or vector of two colors for plotting
#' @param ... other parameters passed to \code{plot}
#' @importFrom graphics plot grid polygon lines legend
#' @importFrom grDevices adjustcolor col2rgb
#' @export
plot.wcep <- function(x, main = " ", type = "n", lty = NULL, lwd = NULL,
                       xlab = " ", ylab = "Event Weighted Survival Probability",
                       xlim = NULL, ylim = NULL, cex = NULL, linecol = NULL, ...) {

  # --- Colour validation ---
  if (!is.null(linecol)) {
    if (!is.character(linecol) || anyNA(linecol))
      stop("Please supply non-NA colors for plot or set linecol = NULL for default colors.",
           call. = FALSE)
    ok <- tryCatch({ col2rgb(linecol); TRUE }, error = function(e) FALSE)
    if (!ok)
      stop("Please supply valid colors for plot or set linecol = NULL for default colors.",
           call. = FALSE)
  }

  # --- Split (two groups) or single result ---
  is_split <- length(x) == 2L          # non-split wcep objects have 4+ elements
  if (is_split) {
    sg   <- .wcep_split_groups(x)      # ordered by group_n: comparison first, reference second
    grps <- list(sg$g1, sg$g2)
    labs <- unname(sg$labels)
  } else {
    grps <- list(x)
  }
  n_g <- length(grps)

  need <- c("survival_probabilities", "lower", "upper")
  for (i in seq_len(n_g)) {
    if (!all(need %in% names(grps[[i]])))
      stop("`x` is missing required components: ",
           paste(setdiff(need, names(grps[[i]])), collapse = ", "), call. = FALSE)
  }

  # --- Colours, line settings ---
  cols <- if (is_split) c("orangered1", "dodgerblue4") else "dodgerblue4"
  if (!is.null(linecol)) {
    k <- seq_len(min(length(linecol), n_g))
    cols[k] <- linecol[k]
  }
  typ <- if (type == "n") "l" else type
  lt  <- if (is.null(lty)) 1 else lty
  lw  <- if (is.null(lwd)) 2 else lwd

  # --- Axis limits ---
  lens <- vapply(grps, function(g) length(g$survival_probabilities), integer(1))
  lows <- unlist(lapply(grps, `[[`, "lower"), use.names = FALSE)
  ymin <- round(max(min(lows, na.rm = TRUE) - 0.0499, 0) - 0.05, 1)
  yl   <- if (is.null(ylim)) c(ymin, 1) else ylim
  xl   <- if (is.null(xlim)) c(0, max(lens)) else xlim

  # --- Empty frame, then confidence bands, then lines ---
  plot(0, type = "n", main = main, xlab = xlab, ylab = ylab, xlim = xl, ylim = yl,
       panel.first = grid(col = "lightgray", lty = "solid"), ...)

  for (i in seq_len(n_g)) {
    g  <- grps[[i]]
    tt <- 0:lens[i]
    polygon(c(tt, rev(tt)),
            c(1, g$upper, rev(c(1, g$lower))),
            col = adjustcolor(cols[i], alpha.f = 0.2), border = NA)
  }
  for (i in seq_len(n_g)) {
    lines(0:lens[i], c(1, grps[[i]]$survival_probabilities),
          type = typ, lty = lt, lwd = lw, col = cols[i])
  }

  # --- Legend (split only) ---
  if (is_split) {
    cx <- if (is.null(cex)) 0.8 else cex
    legend("bottomleft", legend = labs, lty = lt, lwd = lw, col = cols, cex = cx)
  }

  invisible(x)
}

#' wcep plot
#'
#' Create a plot of Kaplan-Meier curve with its specified confidence interval
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
#' @param ... other parameters of generic "plot" have no use here
#' setOldClass("wcep")
#' @export


  plot.wcep <- function(x, main = " ", type = "n", lty = NULL,  lwd = NULL,
                        xlab = " ", ylab = "Event Weighted Survival Probability",
                        xlim = NULL, ylim = NULL, cex=NULL, linecol=NULL,
                        ...){

    if (length(linecol) > 0 & anyNA(linecol)) {
      return(noquote("Error: Please supply non-NA colors for plot or set linecol=NULL for default colors."))
    }

    if (length(linecol) > 0 & anyNA(linecol) == F) {
      color_exists <- linecol %in% colors()
      if (all(color_exists) == F){
        return(noquote("Error: Please supply valid colors for plot or set linecol=NULL for default colors."))
      }
    }

      if(length(x) > 2) {
        color1 <- if (is.null(linecol)) "dodgerblue4" else linecol[1]
        timelab <- 0:length(x$survival_probabilities)
        ymin <- round(max(min (x$lower)-0.0499,0)-0.05, 1)
        yl <- if (is.null(ylim)) c(ymin, 1) else ylim
        xmax <- length(x$survival_probabilities)
        xl <- if (is.null(xlim)) c(0, xmax) else xlim
        typ <- if (type == "n") "l" else type
        lt <- if (is.null(lty)) 1 else lty
        lw <- if (is.null(lwd)) 2 else lwd
        plot(timelab, c(1,x$survival_probabilities), type = typ, lwd = lw, col = color1,
             main = main, xlab = xlab, ylab = ylab, xlim = xl, ylim = yl,
             panel.first = grid(col = 'lightgray', lty = 'solid'),
             ...)
        b <- c(c(1, x$upper), rev(c(1, x$lower)))
        c <- c(timelab,rev(timelab))
        polygon(c, b, col = scales::alpha(color1, 0.2), border = NA)
        points(timelab, c(1,x$survival_probabilities), type = typ, lty = lt, lwd = lw,
               col = color1)
      } else {
        color1 <- if (is.null(linecol)) "dodgerblue4" else linecol[1]
        color2 <- if (length(linecol) <= 1) "orangered1" else linecol[2]
        timelab <- 0:max(length(x[[1]]$survival_probabilities),
                         length(x[[2]]$survival_probabilities))
        ymin <- round(max(min (c(x[[1]]$lower, x[[2]]$lower))-0.0499,0)-0.05, 1)
        yl <- if (is.null(ylim)) c(ymin,1) else ylim
        xl <- if (is.null(xlim)) c(0, length(x[[1]]$survival_probabilities)) else xlim
        typ <- if (type == "n") "l" else type
        lt <- if (is.null(lty)) 1 else lty
        lw <- if (is.null(lwd)) 2 else lwd
        plot(timelab, c(1,x[[1]]$survival_probabilities), type = typ, lwd = lw, col = color1,
             main = main, xlab = xlab, ylab = ylab, xlim = xl, ylim = yl, lty = lt,
             panel.first = grid(col = 'lightgray', lty = 'solid'),
             ...)
        b1 <- c(c(1, x[[1]]$upper), rev(c(1, x[[1]]$lower)))
        c1 <- c(timelab,rev(timelab))
        polygon(c1, b1, col = scales::alpha(color1, 0.2), border = NA)

        b2 <- c(c(1, x[[2]]$upper), rev(c(1, x[[2]]$lower)))
        c2 <- c(timelab,rev(timelab))
        polygon(c2, b2, col = scales::alpha(color2, 0.2), border = NA)
        points(timelab, c(1,x[[2]]$survival_probabilities), type = typ, lty = lt,
               lwd = lw, col = color2)
        cx <- if (is.null(cex)) 0.8 else cex
        legend("bottomleft", legend = names(x)[1:2], lty = rep(lt, 2), lwd = rep(lw, 2),
               col = c(color1, color2), cex=rep(cx,2))
      }
    }

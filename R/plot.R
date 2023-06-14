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
#' @param ... other parameters of generic "plot" have no use here
#' setOldClass("wcep")
#' @export


  plot.wcep <- function(x, main = " ", type = "n", lty = NULL,  lwd = NULL,
                          xlab = " ", ylab = "Survival Probability", xlim = NULL,
                          ylim = NULL, cex=NULL, ...){
      if(length(x) == 5) {
        timelab <- 0:dim(x$life_table)[2]
        ymin <- round(max(min (x$lower)-0.0499,0)-0.05, 1)
        yl <- if (is.null(ylim)) c(ymin, 1) else ylim
        xmax <- length(x$survival_probabilities)
        xl <- if (is.null(xlim)) c(0, xmax) else xlim
        typ <- if (type == "n") "l" else type
        lt <- if (is.null(lty)) 1 else lty
        lw <- if (is.null(lwd)) 2 else lwd
        plot(timelab, c(1,x$survival_probabilities), type = typ, lwd = lw, col = "hotpink2",
             main = main, xlab = xlab, ylab = ylab, xlim = xl, ylim = yl, ...)
        b <- c(c(1, x$upper), rev(c(1, x$lower)))
        c <- c(timelab,rev(timelab))
        polygon(c, b, col = "lavenderblush1", border = NA)
        points(timelab, c(1,x$survival_probabilities), type = typ, lty = lt, lwd = lw,
               col = "hotpink2")
      } else {
        timelab <- 0:dim(x[[1]]$life_table)[2]
        ymin <- round(max(min (c(x[[1]]$lower, x[[2]]$lower))-0.0499,0)-0.05, 1)
        yl <- if (is.null(ylim)) c(ymin,1) else ylim
        xl <- if (is.null(xlim)) c(0, length(x[[1]]$survival_probabilities)) else xlim
        typ <- if (type == "n") "l" else type
        lt <- if (is.null(lty)) 1 else lty
        lw <- if (is.null(lwd)) 2 else lwd
        plot(timelab, c(1,x[[1]]$survival_probabilities), type = typ, lwd = lw, col = "violetred4",
             main = main, xlab = xlab, ylab = ylab, xlim = xl, ylim = yl, lty = lt, ...)
        b1 <- c(c(1, x[[1]]$upper), rev(c(1, x[[1]]$lower)))
        c1 <- c(timelab,rev(timelab))
        polygon(c1, b1, col = rgb(0.92, 0.43, 0.63, 0.43), border = NA)

        b2 <- c(c(1, x[[2]]$upper), rev(c(1, x[[2]]$lower)))
        c2 <- c(timelab,rev(timelab))
        polygon(c2, b2, col = rgb(1,0.58,0.55,0.62), border = NA)
        points(timelab, c(1,x[[2]]$survival_probabilities), type = typ, lty = lt,
               lwd = lw, col = "orangered1")
        cx <- if (is.null(cex)) 0.8 else cex
        legend("bottomleft", legend = names(x)[1:2], lty = rep(lt, 2), lwd = rep(lw, 2),
               col = c("violetred4", "orangered1"), cex=rep(cx,2))
      }
    }

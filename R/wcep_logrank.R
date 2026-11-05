#' Weighted log-rank test comparing two groups
#'
#' @param x a `wcep` object from `wcep()` called with `split = TRUE`.
#' @param alt alternative hypothesis method for p-value calculation. Must be one
#' of "not.equal", "greater", or "less". Set "not.equal" for a two-sided test.
#' The one-sided options are "greater" for the h_a: Surv_comparison > Surv_reference,
#' and "less" for h_a: Surv_comparison < Surv_reference. Default is "not.equal".
#' @return A list with group labels, the null hypothesis, the test statistic,
#'   and the p-value.
#' @importFrom stats pnorm
#' @export
wcep_logrank <- function(x, alt = "not.equal") {

  if (!(alt %in% c("not.equal", "greater", "less")))
    stop("`alt` must be one of 'not.equal', 'greater', 'less'", call. = FALSE)

  grp <- .wcep_split_groups(x)
  g1  <- grp$g1
  g2  <- grp$g2

  # Per-group risk sets, with the 1e-6 guard reset to 0
  rs1 <- as.numeric(g1$n_riskset)
  rs2 <- as.numeric(g2$n_riskset)
  rs1[rs1 < 1] <- 0
  rs2[rs2 < 1] <- 0

  # First moment (stored uj) and second moment of the weights, per group
  uj1  <- as.vector(as.matrix(g1$pj) %*% g1$w)
  uj2  <- as.vector(as.matrix(g2$pj) %*% g2$w)
  m2_1 <- as.vector(as.matrix(g1$pj) %*% (g1$w^2))
  m2_2 <- as.vector(as.matrix(g2$pj) %*% (g2$w^2))

  # Only time points where both groups are at risk contribute. Beyond the
  # shorter group's follow-up, rs = 0 and every term is exactly 0.
  m   <- min(length(rs1), length(rs2))
  idx <- which(rs1[seq_len(m)] > 0 & rs2[seq_len(m)] > 0)   # implies rs_all >= 2

  z_lr <- NA_real_
  if (length(idx)) {
    r1 <- rs1[idx]
    r2 <- rs2[idx]
    rt <- r1 + r2

    mu  <- (r1 * uj1[idx]  + r2 * uj2[idx])  / rt   # pooled sum(w_k p_jk)
    mu2 <- (r1 * m2_1[idx] + r2 * m2_2[idx]) / rt   # pooled sum(w_k^2 p_jk)

    obs_minus_exp <- sum(r1 * (uj1[idx] - mu))      # sum(d_1j - n_1j * sum(w_k p_jk))
    var_lr        <- sum(r1 * r2 / (rt - 1) * (mu2 - mu^2))

    if (is.finite(var_lr) && var_lr > 0) {
      z_lr <- obs_minus_exp / sqrt(var_lr)
    }
  }
  if (is.na(z_lr))
    warning("Log-rank variance is zero or undefined; statistic and p-value set to NA.",
            call. = FALSE)

  lab1 <- grp$labels[["Comparison Group"]]
  lab2 <- grp$labels[["Reference Group"]]
  s1   <- paste0("Surv_", lab1)
  s2   <- paste0("Surv_", lab2)

  if (alt == "not.equal") {
    p_value <- 2 * pnorm(-abs(z_lr))
    hyp     <- paste(s1, "=", s2)
  } else if (alt == "greater") {
    p_value <- pnorm(z_lr)
    hyp     <- paste(s1, "\u2264", s2)    # <=, written as an escape for R CMD check
  } else {
    p_value <- pnorm(z_lr, lower.tail = FALSE)
    hyp     <- paste(s1, "\u2265", s2)    # >=
  }

  list(
    group_labels = grp$labels,
    H_0          = hyp,
    lr_statistic = z_lr,
    p_value      = p_value
  )
}


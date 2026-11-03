# Function: wcep_core
#
# Analyze Weighted Composite EndPoints for one data set and produce life table, survival
# probabilities, variances, and 95% C.I. by given alpha or by default alpha = 0.05.
#
# Authors: Majid Nabipoor, Jeff Bakal
# revised: Sep. 2019, Jan. 2020, Oct. 2020

 wcep_core <- function(x, ew, alpha, simplify) {

   out <- list(); class(out) <- "wcep"

   ID <- TIME <- EVENT <- event <- weight <- i_key <- evtp_h <- event_base <- evtm_h <- censor_time <- NULL

   setnames(x,  c("ID", "EVENT", "TIME"))
   setnames(ew, c("event", "weight"))
   x[, `:=`(EVENT = as.character(EVENT), TIME = as.numeric(TIME))]
   ew[, event := as.character(event)]



   # --- Event matching ---
   missing_evts <- setdiff(unique(x$EVENT), ew$event)
   if (length(missing_evts))
     stop("Error: Data contains events not in weight file: ",
          paste(missing_evts, collapse = ", "), call. = F)

   max_time <- max(x$TIME)

   term_events <- ew$event[which(ew$weight == 1)]
   cens_events <- ew$event[which(ew$weight == 0)]
   term_ind <- if (length(term_events)) term_events[1] else NA_character_
   cens_ind <- if (length(cens_events)) cens_events[1] else NA_character_

   # --- Consolidate events, integer key, sort ---
   x[EVENT %in% term_events, EVENT := term_ind]
   x[EVENT %in% cens_events, EVENT := cens_ind]
   x[, i_key := as.integer(frank(ID, ties.method = "dense"))]  # <-- changed
   setorder(x, i_key, TIME)
   n <- max(x$i_key)

   # first row per subject among the rows in `idx` (x is sorted by i_key, TIME)
   first_per_subject <- function(idx) idx[!duplicated(x$i_key[idx])]

   # --- Logic checks (no group_by) ---
   n_term <- tabulate(x$i_key[x$EVENT %in% term_ind], n)
   n_cens <- tabulate(x$i_key[x$EVENT %in% cens_ind], n)

   if (any(n_term > 1) || any(n_cens > 1) || any(n_term > 0 & n_cens > 0)) {
     fs <- first_per_subject(which(x$EVENT %in% c(term_ind, cens_ind)))
     stop_time <- rep(Inf, n)
     stop_time[x$i_key[fs]] <- x$TIME[fs]
     x <- x[TIME <= stop_time[i_key]]

     if (any(n_term > 1 & n_cens == 0))
       cat("Note: Multiple terminal events in at least 1 subject. First terminal event used.\n")
     if (any(n_cens > 1 & n_term == 0))
       cat("Note: Multiple censor times in at least 1 subject. First censoring event used.\n")
     if (any(n_term > 0 & n_cens > 0))
       cat("Note: At least 1 subject has both terminal and censor event. First terminal or censoring event used.\n")
   }

   # --- Event renaming: vectorized replacement for nam(), with "_" separator ---
   x[, evtp_h := paste0(EVENT, "_", rowid(i_key, EVENT))]

   # --- Drop events after censoring; bump censor at max_time ---
   x[, evtm_h := TIME]
   fc <- first_per_subject(which(x$EVENT %in% cens_ind))
   cens_time <- rep(NA_real_, n)
   cens_time[x$i_key[fc]] <- x$TIME[fc]
   x2 <- x[is.na(cens_time[i_key]) | TIME <= cens_time[i_key]]
   x2[EVENT %in% cens_ind & evtm_h == max_time, evtm_h := max_time + 1]

   # --- Time matrix: direct fill instead of pivot_wider ---
   cols <- unique(x2$evtp_h)
   cj   <- match(x2$evtp_h, cols)
   xx   <- matrix(max_time + 1, nrow = n, ncol = length(cols),
                  dimnames = list(NULL, cols))
   xx[cbind(x2$i_key, cj)] <- x2$evtm_h

   # weight per column, taken from the base event directly (no regex)
   col_weight <- ew$weight[match(x2$EVENT[match(cols, x2$evtp_h)], ew$event)]
   tmat <- cbind(t(xx), col_weight)

   # --- Risk set ---
   all_cens_times <- rep(max_time + 1, n)
   all_term_times <- rep(max_time + 1, n)
   fc <- first_per_subject(which(x$EVENT %in% cens_ind))
   ft <- first_per_subject(which(x$EVENT %in% term_ind))
   all_cens_times[x$i_key[fc]] <- x$TIME[fc] + 1
   all_term_times[x$i_key[ft]] <- x$TIME[ft] + 1

   n_riskset <- n - cumsum(tabulate(all_cens_times, max_time) +
                             tabulate(all_term_times, max_time))
   n_riskset[n_riskset <= 0] <- 1e-6

   # --- Variance parameters: one tabulate call instead of apply over columns ---
   valid_cols_idx <- which(!is.na(col_weight) & col_weight != 0)
   W_valid <- col_weight[valid_cols_idx]
   k <- length(valid_cols_idx)

   vcol <- match(cj, valid_cols_idx)
   keep <- !is.na(vcol) & x2$evtm_h >= 1 & x2$evtm_h <= max_time
   event_timepoints <- matrix(
     tabulate((vcol[keep] - 1L) * max_time + as.integer(x2$evtm_h[keep]),
              nbins = max_time * k),
     nrow = max_time, ncol = k)

   pj <- event_timepoints / n_riskset
   uj <- as.vector(pj %*% W_valid)

   # ============================================================================
   # CALL SuRV/VAR FuNCTIONS
   # ============================================================================

   if(simplify == FALSE){
     # 1. Compute Survival Table
     s_table <- survtab(tmat,
                        n,
                        max_time,
                        all_cens_times,
                        uj)

     # 2. Compute Variance (using Optimized C++ Function)
     surv_squared <- colMeans(s_table)^2
     v_vector <- varvec(
       W = W_valid,
       uj = uj,
       pj = pj,
       s_sq = surv_squared,
       censor_times = all_cens_times,
       n_riskset = n_riskset,
       n = n,
       max_time = max_time
     )

     out$life_table <- s_table
     out$survival_probabilities <- colMeans(s_table)
     out$variance <- v_vector
   }

   if(simplify == TRUE){
     # 1. Compute Survival Vector
     surv_simple = cumprod(1-uj)

     # 2. Compute Simplified Variance (using Optimized C++ Function)
     surv_squared = surv_simple^2
     v_vector = varvec_simple(
       W = W_valid,
       uj = uj,
       pj = pj,
       s_sq = surv_squared,
       n_riskset = n_riskset,
       max_time = max_time
     )

     out$survival_probabilities <- surv_simple
     out$variance <- v_vector
   }


   z <- qnorm(1 - alpha / 2)
   out$upper <- pmin(pmax(out$survival_probabilities + z * sqrt(v_vector), 0), 1)
   out$lower <- pmin(pmax(out$survival_probabilities - z * sqrt(v_vector), 0), 1)

   return(out)
 }

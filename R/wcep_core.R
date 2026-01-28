#' @include nam.R
#'
# Function: wcep_core
#
# Analyze Weighted Composite EndPoints for one data set and produce life table, survival
# probabilities, variances, and 95% C.I. by given alpha or by default alpha = 0.05.
#
# Authors: Majid Nabipoor, Jeff Bakal
# revised: Sep. 2019, Jan. 2020, Oct. 2020

 wcep_core <- function(x, ew, alpha) {

   out <- list()
   class(out) <- "wcep"

   ID <- TIME <- EVENT <- i_key <- evtp_h <- event_base <- evtm_h <- censor_time <- NULL

   # Standardize column names
   colnames(x) <- c("ID", "EVENT", "TIME")
   names(ew) <- c("event", "weight")

   # --- Error Handling: Event Matching ---
   if(!all(x$EVENT %in% ew$event)){
     missing_evts <- unique(x$EVENT[!x$EVENT %in% ew$event])
     stop(paste("Error: Data contains events not in weight file:", paste(missing_evts, collapse=", ")))
   }

   max_time <- max(as.numeric(x$TIME))

   # Identify key event types
   term_events <- ew$event[ew$weight == 1]
   term_ind <- if(length(term_events) > 0) term_events[1] else NA

   cens_events <- ew$event[ew$weight == 0]
   cens_ind <- if(length(cens_events) > 0) cens_events[1] else NA

   # --- Data Cleaning & Consolidation ---
   # Unify all terminal events to one label, and censoring events to one label
   x <- x |>
     ungroup() |>
     mutate(EVENT = case_when(
       EVENT %in% term_events ~ term_ind,
       EVENT %in% cens_events ~ cens_ind,
       TRUE ~ EVENT
     )) |>
     arrange(ID, TIME)

   # --- Error Handling: Logic Checks ---
   check_multiples <- x |>
     group_by(ID) |>
     summarise(
       n_term = sum(EVENT == term_ind, na.rm=TRUE),
       n_cens = sum(EVENT == cens_ind, na.rm=TRUE)
     )

   if(any(check_multiples$n_term > 1)) stop("Error: Multiple terminal events in at least 1 subject.")
   if(any(check_multiples$n_cens > 1)) stop("Error: Multiple censor times in at lesat 1 subject.")
   if(any(check_multiples$n_term > 0 & check_multiples$n_cens > 0)) stop("Error: At least 1 subject has both terminal and censor event.")

   # Create numerical ID map
   x <- x |> mutate(i_key = as.integer(as.factor(ID)))
   n <- max(x$i_key)

   # --- Event Renaming (Vectorized) ---
   x1 <- x |>
     group_by(i_key) |>
     mutate(evtp_h = nam(EVENT)) |>
     ungroup()

   # --- Advanced Filtering ---
   # Remove events occurring after censoring time
   x2 <- x1 |>
     mutate(evtm_h = as.numeric(TIME)) |>
     mutate(event_base = str_extract(evtp_h, "^[A-Za-z]+")) |>
     group_by(i_key) |>
     mutate(censor_time = ifelse(any(event_base == cens_ind),
                                 evtm_h[which(event_base == cens_ind)[1]],
                                 NA)) |>
     filter(is.na(censor_time) | evtm_h <= censor_time) |>
     # Edge case: If censor event is at max_time, bump to max_time + 1
     mutate(evtm_h = ifelse(!is.na(cens_ind) & event_base == cens_ind & evtm_h == max_time,
                            max_time + 1,
                            evtm_h)) |>
     select(i_key, evtp_h, evtm_h) |>
     ungroup()

   # Format to Wide Matrix (Time Matrix for C++)
   xx <- x2 |>
     pivot_wider(names_from = evtp_h, values_from = evtm_h, values_fill = max_time + 1) |>
     arrange(i_key) |>
     select(-i_key) |>
     as.matrix()

   # Map Weights to Matrix Columns
   col_events <- colnames(xx)
   col_base_events <- str_remove(col_events, "\\d+$")

   ew_map <- data.frame(event = col_base_events) |>
     left_join(ew, by = "event")

   # Final Time Matrix input for C++
   tmat <- cbind(t(xx), ew_map$weight)

   # --- Risk Set Calculation ---
   # Vector of censoring/terminal times for each patient
   censor_info <- x |> filter(EVENT == cens_ind) |> select(i_key, TIME) |> distinct()
   term_info <- x |> filter(EVENT == term_ind) |> select(i_key, TIME) |> distinct()

   all_cens_times <- rep(max_time + 1, n)
   all_term_times <- rep(max_time + 1, n)

   if(nrow(censor_info) > 0) all_cens_times[censor_info$i_key] <- censor_info$TIME + 1
   if(nrow(term_info) > 0) all_term_times[term_info$i_key] <- term_info$TIME + 1

   cens_counts <- tabulate(all_cens_times, nbins = max_time)
   term_counts <- tabulate(all_term_times, nbins = max_time)

   n_riskset <- n - cumsum(cens_counts + term_counts)
   n_riskset[n_riskset <= 0] <- 1e-6 # Prevent division by zero

   # --- Variance Parameters Preparation ---
   # Filter out events with NA or 0 weight
   valid_cols_idx <- which(!is.na(ew_map$weight) & ew_map$weight != 0)
   xx_valid <- xx[, valid_cols_idx, drop=FALSE]
   W_valid <- ew_map$weight[valid_cols_idx]

   # Calculate Event Rates (pj) and Weighted Sum (uj)
   event_timepoints <- apply(xx_valid, 2, function(col) tabulate(col, nbins = max_time))
   pj <- event_timepoints / n_riskset
   uj <- as.vector(pj %*% W_valid)

   # ============================================================================
   # CALL C++ FUNCTIONS
   # ============================================================================

   # 1. Compute Survival Table
   s_table <- survtab(tmat,
                      n,
                      max_time,
                      all_cens_times,
                      uj)

   # 2. Compute Variance (Using Optimized C++ Function)
   v_vector <- varvec(
     W = W_valid,
     uj = uj,
     pj = pj,
     s_table = s_table,
     censor_times = all_cens_times,
     n_riskset = n_riskset,
     n = n,
     max_time = max_time
   )

   # ============================================================================
   # OUTPUT FORMATTING
   # ============================================================================
   out$life_table <- s_table
   out$survival_probabilities <- colMeans(s_table)
   out$variance <- v_vector

   z <- qnorm(1 - alpha / 2)
   out$upper <- pmin(pmax(out$survival_probabilities + z * sqrt(v_vector), 0), 1)
   out$lower <- pmin(pmax(out$survival_probabilities - z * sqrt(v_vector), 0), 1)

   return(out)
 }

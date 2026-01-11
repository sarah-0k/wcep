#' @include nam.R
#'
# Function: wcep_core
#
# Analyze Weighted Composite EndPoints for one data set and produce life table, survival
# probabilities, variances, and 95% C.I. by given alpha or by default alpha = 0.05.
#
# Authors: Majid Nabipoor, Jeff Bakal
# revised: Sep. 2019, Jan. 2020, Oct. 2020

 wcep_core <- function(x, ew, alpha=0.05) {

   # list of output
   out <- list()
   class(out) <- "wcep"

   colnames(x) <- c("ID", "EVENT", "TIME")
   names(ew) <- c("event", "weight")

   ##############################################################################
   ##############################################################################
   # START OF ERROR HANDLING
   ##############################################################################
   #add check that (1) no events listed after terminal or (2) after censor
   if(!any(x$EVENT %in% ew$event)){
     stop("NO EVENTS IN DATA MATCH EVENT WEIGHTS -- fix data")
   }
   ##############################################################################
   # END OF ERROR HANDLING
   ##############################################################################
   ##############################################################################

   max_time <- max(as.numeric(x$TIME)) #take max before removing non-events

   term_events <- (ew |> dplyr::filter(weight == 1))[,1]
   term_ind <- term_events[1] #returns NA if term_events empty

   cens_events <- (ew |> dplyr::filter(weight == 0))[,1]
   cens_ind <- cens_events[1] #returns NA if cens_events empty

   #need to make sure ordering of censor/death times match dataframe
   x <- x |>
     ungroup() |> #just in case
     #filter(EVENT %in% ew$event) |> #remove any event not listed in weight dataframe
     mutate(EVENT = case_when(
       EVENT %in% term_events ~ term_ind,
       EVENT %in% cens_events ~ cens_ind,
       .default = EVENT)) |>
     arrange(ID, TIME)

   ##############################################################################
   ##############################################################################
   # START OF ERROR HANDLING
   ##############################################################################
   #add check that (1) no events listed after terminal or (2) after censor
   if(any((x |> dplyr::filter(EVENT == term_ind) |>
           group_by(ID) |> dplyr::summarise(n()))[,2] > 1)){
     stop("MULTIPLE TERMINAL EVENTS IN AT LEAST 1 SUBJECT -- fix data")
   }

   if(any((x |> dplyr::filter(EVENT == cens_ind) |>
           group_by(ID) |> dplyr::summarise(n()))[,2] > 1)){
     stop("MULTIPLE CENSORING EVENTS IN AT LEAST 1 SUBJECT -- fix data")
   }

   if(any((x |> dplyr::filter(EVENT == cens_ind | EVENT == term_ind) |>
           group_by(ID) |> dplyr::summarise(n()))[,2] > 1)){
     stop("AT LEAST 1 SUBJECT WITH BOTH TERMINAL AND CENSOR EVENT -- fix data")
   }
   ##############################################################################
   # END OF ERROR HANDLING
   ##############################################################################
   ##############################################################################

   # add a numerical patient ID column
   pt_h <- as.factor(x$ID)
   ptid_h <- 1:length(unique(pt_h))
   dd <- data.frame(pt_h = unique(pt_h), ptid_h)
   names(dd) <- c(names(x)[1], "i_key")
   x1 <- merge(x, dd, by = names(x)[1]) |>
     arrange(ID, TIME) # need sorted so second censoring time (i.e., EOS) is correctly removed


   # Numerate similar events for a patient; SHK SHK -> SHK1 SHK2
   evtp_h <- as.character(x1[, 2])
   evtm_h <- as.numeric(x1[, 3])
   invisible(sapply(ptid_h, function(i) {
     ind <- which(x1[, "i_key"] == i)

     evtp_h[ind] <<- nam(evtp_h[ind])
   }))

   x2 <- data.frame(i_key = x1[, "i_key"], evtp_h, evtm_h = x1[, "TIME"]) |>
     mutate(event_base = stringr::str_extract(evtp_h, "^[A-Za-z]+")) |>   # Extract event type without numbers
     # no longer need handling of multi-censor events, handled in
     # stop/error statements (below also only functions for 2 events)
     group_by(i_key) |>
     # remove events that occur after censoring (terminal non-issue as cont survival will be 0)
     mutate(censor_time = ifelse(any(evtp_h == paste(cens_ind, 1, sep = "")),
                                 evtm_h[evtp_h == paste(cens_ind, 1, sep = "")],
                                 NA)) |>
     ungroup() |>
     filter(is.na(censor_time) | evtm_h <= censor_time) |>  # Remove events after censoring
     mutate(evtm_h = ifelse(!is.na(cens_ind) & event_base == cens_ind & evtm_h == max_time,
                            max_time + 1,
                            evtm_h)) |>
     dplyr::select(-censor_time) |>  # Remove helper column
     dplyr::select(-event_base) |>
     mutate(evtm_h = as.numeric(evtm_h))


   # time matrix for each unique event of patients
   xx <- x2 %>% spread(evtp_h, evtm_h)
   xx[is.na(xx)] <- max_time + 1

   # life table and survival probabilities
   ew1 <- data.frame(event = ew[, 1], weight = ew[, 2])

   ew_h <- data.frame(cbind(colnames(xx[, -1]),
                            gsub("(?<! )\\d+", "",
                                 colnames(xx[, -1]), perl = TRUE)))


   colnames(ew_h) <- c("e_key", "event")
   ew_h1 <- left_join(ew_h, ew1, by = "event")[, c(1, 3)]

   xx <- xx[, -which(colnames(xx) %in% setdiff(colnames(xx), ew_h1[, 1]))]
   n <- length(ptid_h)

   # Count number of patients still in risk set at each timepoint -- to be used for censoring
   # Get vector of unique censoring/EOS times for all patients
   censor_info <- x |>
     # Select patients/rows with censoring event or death
     filter(EVENT == cens_ind) |>
     group_by(ID) |>
     # For subjects without a censor event, set censor_time to NA
     right_join(x |> distinct(ID), by = "ID") |>
     # Change NA censoring times to maxtime + 1
     mutate(censor_time = ifelse(is.na(TIME), max_time + 1, TIME + 1)) |>
     arrange(ID) |>
     #add ungroup and select to handle loss of summarize statement
     ungroup() |>
     dplyr::select(ID, censor_time)

   # create vector of death times and censor times
   censor_times <- as.vector(censor_info$censor_time)

   # SR - add terminal end times because removed from censor times for c++
   term_times <- as.vector(unlist(xx |> dplyr::select(contains(term_ind))))

   #n at risk at the start of each day (less N first day of full censor,
   #                                    less N first day after death)
   n_riskset = n - cumsum(tabulate(censor_times, nbins = max_time) +
                            tabulate(unlist(xx |> dplyr::select(contains(term_ind)))+1,
                                     nbins = max_time))

   event_cols <- setdiff(colnames(xx), paste0(cens_ind, "1"))

   event_timepoints <- sapply(event_cols, function(event) {
     tabulate(xx[[event]], nbins = max_time)
   })

   colnames(event_timepoints) <- paste(event_cols, "timepoints", sep = "_")


   ##########################################################################
   #c++ application



   if(any(c(which(is.na(ew_h1[,2]))))){

     tmat = as.matrix(cbind(t(xx)[-c(c(which(is.na(ew_h1[,2])))),],
                            ew_h1[-c(c(which(is.na(ew_h1[,2])))),2]))
     pj = event_timepoints[,-c(c(which(is.na(ew_h1[,2]))))] / n_riskset

   } else {
     tmat = as.matrix(cbind(t(xx)[,], ew_h1[,2]))
     pj = event_timepoints[,] / n_riskset

   }

   W = ew_h1$weight[!is.na(ew_h1$weight) & ew_h1$weight != 0]
   uj = rowSums(sweep(pj, 2, W, FUN = "*"))
   cens <- (censor_info[,2] |> as.matrix())

   cat("\n\n***********Progress Bar: Survival Estimates***********\n\n")
   s_table_c <- survtab(tmat, n, max_time, cens, uj)

   #########################################################################

   out$life_table <- s_table_c

   surv_probs <- apply(s_table_c, 2, mean)

   out$survival_probabilities <- surv_probs

   ########################################################################
   #NEW VAR FUNCTION

   #c++ application
   cat("\n\n***********Progress Bar: Variance Estimates***********\n\n")
   vars = varvec(W, uj, pj, s_table_c, censor_times, n_riskset,
                 n, max_time)


   out$variance = vars


   ########################################################################

   ########################################################################

   z <- qnorm(1 - alpha / 2)

   upper <- surv_probs + z * sqrt(vars)
   lower <- surv_probs - z * sqrt(vars)
   out$upper <- pmin(pmax(upper, 0), 1)
   out$lower <- pmin(pmax(lower, 0), 1)

   return(out)
 }

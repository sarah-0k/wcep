#' @include wcep_core.R
NULL
#'
#'  Analysis of weighted composite endpoints
#'
#'  Analyze given data frame and return Kaplan-Meier-like event weighted survival
#'  probabilities together with the specified confidence interval. \code{wcep} modifies
#'  a Kaplan-Meier curve by taking into account severity weights of different event.
#'  Alternative methods are Anderson Gill model and win ratio of composite outcomes.The function takes an
#'  event dataset and user-specified severity weights to generate a modified Kaplan-Meier curve and
#'  based on the weighted composite endpoint method. The user supplies the event data set, the weights,
#'  and optionally, a two-level group to split on. The package will generate the event weighted survival curve and
#'  confidence interval.
#'
#' @param data a data.frame, tibble, or data.table with at least 3 columns. The first column specifies ID
#' (character, factor, or numeric), the second column specifies event type (character, factor, or numeric),
#' and the third column specifies event time (numeric whole number or integer). The
#' maximum follow time for administrative censoring and plotting will be set to the max value of this column.
#' The fourth column is optional and specifies group level (character, factor, or numeric with exactly 2 unique levels).
#' The grouping column will only be used when \code{split = TRUE}.
#'
#' @param weights a data.frame, tibble, or data.table with exactly two columns. The first column
#' specifies a character vector of event types (character, factor, or numeric) and the second column
#' specifies event weights (numeric between 0 and 1). The naming of event types should match those
#' in the second column of the data object. Events with weight = 1 are terminal events;
#' events with weight = 0 are censoring events. IDs with multiple terminal events, censor events, or a combination
#' of terminal and censoring events will use the first observed event. IDs without either
#' a terminal or censor event listed will be censored at the maximum follow time. Setting all non-censor events
#' to have weight = 1 gives a time-to-first event result.
#'
#' @param alpha a numeric value in the interval (0,1) which specifies the confidence level.
#' Default is 0.05.
#'
#' @param split a logical value of T or F which allows to compare two groups.
#' Default is FALSE.
#'
#' @param simplify a logical value of T or F which determines whether to use the
#' simplified event-weighted survival and variance estimates (\code{simplify = TRUE}) or
#' the participant-weighted survival and variance estimates (\code{simplify = FALSE}).
#' Default is FALSE.
#'
#' @param ref_group (optional) a string value that can be used when \code{split = TRUE} to
#' specify the reference level group. If \code{split = FALSE} and no reference level is
#' provided, the package will default to the observed ordering of the group column.
#'
#' @return A wcep object (list). If (\code{simplify = TRUE}) this will include
#' survival probabilities, variance, and upper and lower CIs. If (\code{simplify = FALSE})
#' an ID x time life table will be output as well. If (\code{split = TRUE}), outputs
#' will be provided by group level, along with additional information required for
#' calculation of the log-rank statistic.
#'
#' @section References:
#' Bakal J., Westerhout C. M., Armstrong P. W. (2015) Impact of weighted composite
#' compared to traditional composite endpoints for the design of randomized
#' controlled trails. \var{Statistical Methods in Medicine Research}. \bold{24}(6) 980-988.
#'
#' Nabipoor M., Westerhout C. M., Rathwell S., Bakal J. (2023) The empirical
#' estimate of the survival and variance using a weighted composite endpoint,
#' \var{BMC Medical Research Methodology}. \bold{23}(35).
#'
#' @examples
#' data(toyexample)
#' #event weights
#' weights <- data.frame(event = c('CHF','DTH','SHK','REMI','N'), weight = c(0.3,1,0.5,0.2,0))
#' res1 <- wcep(data = toyexample[,1:3], weights = weights, split = FALSE, simplify = FALSE)
#' str(res1)
#' res1$survival_probabilities
#' plot(res1)
#' #comparing two genders
#' res2 <- wcep(data = toyexample, weights = weights, split = TRUE, simplify = FALSE, ref_group = "M")
#' plot(res2)
#' wcep_logrank(res2)

#' @author
#' Majid Nabipoor: nabipoor@@ualberta.ca,
#' Cynthia Westerhout: cindy.westerhout@@ualberta.ca,
#' Jeffrey Bakal: jbakal@@ualberta.ca,
#' Sarah Rathwell: srathwel@@ualberta.ca
#' @seealso \code{\link[survival:coxph]{coxph}} for Anderson Gill model
#' @importFrom stats qnorm
#' @importFrom graphics plot points polygon legend grid
#' @importFrom grDevices rgb colors
#' @importFrom Rcpp sourceCpp
#' @importFrom scales alpha
#' @useDynLib wcep
#' @import coin data.table RcppProgress
#' @export

 wcep <- function(data, weights, alpha = 0.05 , split = FALSE, simplify = FALSE, ref_group = NULL){

   # --- Argument checks ---
   if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1)
     stop("Value of alpha should be in (0, 1)", call. = F)
   if (!isTRUE(split) && !isFALSE(split))
     stop("Split argument must be TRUE or FALSE", call. = F)
   if (!isTRUE(simplify) && !isFALSE(simplify))
     stop("Simplify argument must be TRUE or FALSE", call. = F)


   x  <- if (is.data.table(data))  copy(data)  else as.data.table(data)
   ew <- if (is.data.table(weights)) copy(weights) else as.data.table(weights)

   if (split  && ncol(x) != 4) stop("data object should have 4 columns", call. = F)
   if (!split && ncol(x) < 3) stop("data object should have at least 3 columns", call. = F)
   if (ncol(ew) != 2)          stop("weights object should have 2 columns (event, weight)", call. = F)

   # --- NA / type checks (works for data.frame, tibble, data.table) ---
   if (anyNA(x[[1]])) stop("NAs in first (ID) column of the data object", call. = F)
   if (anyNA(x[[2]])) stop("NAs in second (event) column of the data object", call. = F)
   if (anyNA(x[[3]])) stop("NAs in third (time) column of the data object", call. = F)
   if (split && anyNA(x[[4]])) stop("NAs in fourth (group) column of the data object", call. = F)

   if (!is.numeric(x[[3]])) stop("The third (time) column of the data object should be numeric", call. = F)
   if (any(x[[3]] != floor(x[[3]]) | x[[3]] < 1))
     stop("The third (time) column of the data object must contain whole numbers of at least 1", call. = F)

   if (anyNA(ew[[1]])) stop("NAs in event column of the weights object", call. = F)
   if (anyNA(ew[[2]])) stop("NAs in weight column of the weights object", call. = F)
   if (anyDuplicated(ew[[1]])) stop("Duplicate events in the weights object", call. = F)

   if (!split && !is.null(ref_group)) cat("Note: ref_group supplied but split = F. Grouping values are ignored.\n")


   # --- Run ---
   if (!split) {
     return(wcep_core(x[, 1:3, with = FALSE], ew, alpha, simplify, group_n = NULL))
   }

   if (split) {
     grp <- as.character(x[[4]])
     groups <- unique(grp)
     if (length(groups) != 2) stop("The fourth (group) column of the data object should have two levels", call. = F)

     # --- Optional: choose group level order ---
     if (!is.null(ref_group)) {
       if (!is.character(ref_group) || length(ref_group) != 1 || is.na(ref_group))
         stop("ref_group must be a single string", call. = F)
       if (!ref_group %in% groups)
         stop("ref_group '", ref_group, "' is not a level of the group column. ",
              "Available levels: ", paste(groups, collapse = ", "), call. = F)
       groups <- c(setdiff(groups, ref_group), ref_group)
     }

     res <- structure(list(), class = "wcep")
     for (i in seq_along(groups)) {
       g <- groups[i]
       idx <- which(grp == g)
       res[[g]] <- wcep_core(x[idx, 1:3, with = FALSE], ew, alpha, simplify, group_n = i)
     }
     res
   }
 }

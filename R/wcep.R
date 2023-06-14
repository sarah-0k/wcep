#' @include nam.R wcep_core.R
NULL

#'  Analysis of weighted composite endpoints
#'
#'  Analyze given data frame and return Kaplan-Meier survival probabilities together with the specified confidence interval.
#'  \code{wcep} modifies Kaplan-Meier curve by taking into account severity weights of different event. Alternative methods are Anderson Gill model and win ratio of composite outcomes.The function takes event dataset and user-specified severity weights  to generate a modified Kaplan-Meier curve and comparison statistics based on the weighted composite endpoint method.  The user supplies the event data set, the weights, and the factor to split on . The package will generate the weighted survival curve, confidence interval and test the differences between the two groups.
#'
#' @param x This data frame usually has 3 columns. The first column specifies patient ID,
#' which is a character or numeric vector, the second column is a factor with character values
#' of event types. The third column is a numeric vector of event times. If split = TRUE,
#' then the forth column is a character vector of split groups of at most two groups,
#' like gender.
#'
#' @param EW This data frame has two columns. The first column
#' specifies a character vector of event types. The second column specify weights.
#' The naming of event types in x and EW should be exactly similar.
#'
#' @param alpha A numeric value between 0-1 which specifies the confidence level,
#' if it is not specified, by default is 0.05.
#'
#' @param split A logical value of T or F which allows to compare two groups.
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
#' EW <- data.frame(event = c('CHF','DTH','SHK','REMI'), weight = c(0.3,1,0.5,0.2))
#' res1 <- wcep(toyexample, EW)
#' str(res1)
#' res1$survival_probabilities
#' plot(res1)
#' #comparing two genders
#' res2 <- wcep(toyexample, EW, split=TRUE)
#' plot(res2)
#' #wilcox and t test
#' res2$Wilcoxontest
#' res2$t_test
#' @author
#' Majid Nabipoor: nabipoor@@ualberta.ca,
#' Cynthia Westerhout: cindy.westerhout@@ualberta.ca,
#' Jeffrey Bakal: jbakal@@ualberta.ca
#' @seealso \code{\link{coxph}} for Anderson Gill model
#' @importFrom stats qnorm t.test
#' @importFrom graphics plot points polygon legend
#' @importFrom grDevices rgb
#' @import coin dplyr progress tidyr
#' @export

 wcep <- function(x, EW, alpha = 0.05 , split = FALSE){

          if (dim(x)[2] < 3 | dim(x)[2] > 4) {
              return(noquote("Error: Data frame x should have 3 columns for one group or 4 columns for two groups comparison"))
          }
          if (alpha >= 1 | alpha <= 0) {
              return(noquote("Error: value of alpha should be between 0 and 1"))
          }
          if ( split == TRUE && dim(x)[2] != 4 ) {
              return(noquote("Error: Data frame x should have 4 columns" ))
          }
          if ( split == TRUE && length(unique(x[,4])) > 2 ) {
              return(noquote("Error: The last column should have two levels" ))
          }
          if ( is.factor(x[,2]) == FALSE ) {
              return(noquote("Error: The second column should be factor" ))
          }
          res <- structure(list(), class = "wcep")
          if(split == FALSE) {
            pb <- progress_bar$new(
              format = " work progress [:bar] :percent",
              total = NA, clear = FALSE, width= 80)
            res <- wcep_core(x[, 1:3], EW, alpha)
          } else {
            groups <- unique(x[, 4])
            for(i in 1:2) {
              pb <- progress_bar$new(
                format = " Progress [:bar] :percent",
                total = NA, clear = FALSE, width= 80)
              res[[paste0(" ", groups[i], sep="")]] <- wcep_core(x[which(x[, 4] == groups[i]), 1:3], EW,                                                                          alpha)
            }
            res$Wilcoxontest <- wilcoxsign_test((res[[paste0(" ", groups[1], sep="")]])$survival_probabilities ~
                                (res[[paste0(" ", groups[2], sep="")]])$survival_probabilities, zero.method = c("Pratt"))
            res$t_test <- (t.test((res[[paste0(" ", groups[1], sep="")]])$survival_probabilities,
                          (res[[paste0(" ", groups[2], sep="")]])$survival_probabilities))
          }
          res
 }


 # Take the events of a patient and return a numirated version
 # of events. If a patient has two SHKs function "evt" returns
 # SHK1 and SHK2.This provides a vector with unique events for
 # each patient's event to use in spread() within library(tidyr).

  nam <- function(X) {
                 rps <- NULL
                 out <- NULL
                 a <- length(which(table(X)[which(table(X)>0)] > 1))
                 if (a==0) {
                   out <- paste(X, 1, sep = "")
                 } else {
                   for (i in 1:length(X)) {
                     if ((X[i] %in% rps) == FALSE) {
                       rps <- c(rps, as.character(X[i]))
                       out <- c(out, paste(X[i], 1, sep=''))
                     } else {
                      rps <- c(rps, as.character(X[i]))
                      b <- length(which(rps == X[i]))
                      out <- c(out, paste(X[i], b, sep=''))
                     }
                   }
                 }
                 out
         }

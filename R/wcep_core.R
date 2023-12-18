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

              # list of output
              out <- list()
              class(out) <- "wcep"
              # add a numerical patient ID column
              pt_h <- as.factor(x[, 1])
              ptid_h <- 1:length(unique(pt_h))
              dd <- data.frame(pt_h = unique(pt_h), ptid_h)
              names(dd) <- c(names(x)[1], "c4")
              x1 <- merge(x, dd, by = names(x)[1])

              # Numerate similar events for a patient; SHK SHK -> SHK1 SHK2
              evtp_h <- as.character(x1[, 2])
              evtm_h <- as.numeric(x1[, 3])
              invisible(sapply(ptid_h, function(i) {ind <- which(x1[, 4] == i);
              evtp_h[ind] <<- nam(evtp_h[ind])}))
              x2 <- data.frame(c1 = x1[, 4], evtp_h, evtm_h = x1[, 3])

              # time matrix for each unique event of patients
              xx <- x2 %>% spread(evtp_h, evtm_h)
              maxtime <- max(x2[, 3])
              xx[is.na(xx)] <- maxtime+1

              # life table and survival probabilities
              names(ew)<-c("event","weight")
              ew1 <- data.frame(event = ew[, 1], weight=ew[, 2])
              ew_h <- data.frame(cbind(colnames(xx[, -1]), substr(colnames(xx[, -1]), 1,
                      nchar(colnames(xx[, -1])) - 1)))
              colnames(ew_h) <- c("c1", "event")
              ne_h <- setdiff(ew_h[, 2], ew[, 1])
              ew_h0<- droplevels(ew_h[-which(ew_h$event==ne_h),],exclude=ne_h)
              ew_h1 <- left_join(ew_h0, ew1, by="event")[, c(1, 3)]
              xx <- xx[,-which(colnames(xx) %in% setdiff(colnames(xx), ew_h1[,1]))]
              n <- length(ptid_h)
              lifemat <- matrix(1, nrow = n, ncol = maxtime+1)
              tot <- n + maxtime
              pb <- progress_bar$new(
                format = "  downloading :Progress [:bar] :percent",
                clear = FALSE, total = tot, width = 80)
              s_table <- sapply(1:n, function(i){
                pb$tick()
                sapply(2:(maxtime + 1), function(j){
                lifemat[i,j] <<- prod(apply(cbind(t(xx[i, ]), ew_h1)[, -2], 1, function(v){
                                 ifelse(v[1] <= j-1, 1-as.numeric(v[2]), 1)}))
                                 })})
             # lt <- dim(s_table)[1]
             # s_table <- s_table[-lt, ]
              out$life_table <- t(s_table)
              out$survival_probabilities <- apply(s_table, 1, mean)

              # variance
              s_table1 <- data.frame(1, t(s_table))
              Key <- data.frame(key = 1 - ew$weight, ev = ew$event)
              s <- cumsum(sapply(2:dim(s_table1)[2], function(j){
                pb$tick()
                drev <- data.frame( key = s_table1[, j] / s_table1[, j - 1])
                f<-table(dplyr::left_join(drev, Key, by = "key")[, 2])
                u=attr(f,"dimnames")[[1]]
                p<-data.frame(key=Key[,2],0)
                p[,2]<-sapply(1:dim(p)[1],function(i){ifelse(length(which(u==p[i,1]))!=0, f[which(u==p[i,1])], 0)})
                pj<-matrix(p[,2] / sum(s_table1[, j - 1]), ncol=1)
                uj<- matrix(t(ew[, 2]), nrow=1)%*%pj
                (1 - uj)^(-2)*(t(ew[,2])%*%(diag(as.vector(pj))- pj%*%t(pj))%*%ew[,2])
              }))*apply(t(s_table)^2, 2, sum) / (dim(s_table)[2])^2
              out$variance <- s
              upper <- out$survival_probabilities +  qnorm(1-alpha / 2) * sqrt(s)
              lower <- out$survival_probabilities -  qnorm(1-alpha / 2) * sqrt(s)
              out$upper <- sapply(1:length(upper), function(i) {
                ifelse(upper[i] > 1, upper[i] <- 1, upper[i])
                ifelse(upper[i] < 0, upper[i] <- 0, upper[i])
                })
              out$lower <- sapply(1:length(lower), function(i) {
                ifelse(lower[i] > 1, lower[i] <- 1, lower[i])
                ifelse(lower[i] < 0, lower[i] <- 0, lower[i])
              })
              out
         }

library(wcep)

test_that("testinputs",{
  #Data should have at least 3 columns
  expect_error(wcep(toyexample[,1:2],data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 0.05 , split = FALSE))
  #Value of alpha should be in (0, 1)
  expect_error(wcep(toyexample,data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 1.05 , split = FALSE))
  #Value of alpha should be in (0, 1)
  expect_error(wcep(toyexample,data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = -1.05 , split = FALSE))
  #Value of alpha should be in (0, 1)
  expect_error(wcep(toyexample,data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 0 , split = FALSE))
  #Data should have 4 columns
  expect_error(wcep(toyexample[,1:3],data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 0.05 , split = TRUE))
  #The fourth (group) column should have two levels
  expect_error(wcep(data.frame(toyexample[,1:3],sex=c(rep("M",40), rep("F",40), rep("G",24))),
                    data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 0.05 , split = TRUE))
  #Simplify argument must be TRUE or FALSE
  expect_error(wcep(toyexample[,1:3],data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 0.05 , simplify = "yes"))
  #Split argument must be TRUE or FALSE
  expect_error(wcep(toyexample[,1:4],data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 0.05 , simplify = T, split = 'no'))
  #NAs in event column of the weights file
  expect_error(wcep(toyexample[,1:4],data.frame(event=c('CHF',NA,'SHK','REMI','N'), weights = c(0.3,1,0.5,0.2,0)),
                    alpha = 0.05 , simplify = T, split = T))
  #NAs in weights column of the weights file
  expect_error(wcep(toyexample[,1:4],data.frame(event=c('CHF','DTH','SHK','REMI','N'), weights = c(0.3,1,NA,0.2,0)),
                    alpha = 0.05 , simplify = T, split = T))
  #Duplicate events in the weights file
  expect_error(wcep(toyexample[,1:4],data.frame(event=c('CHF','DTH','SHK','REMI','N', 'REMI'), weights = c(0.3,1,0.5,0.2,0,.8)),
                    alpha = 0.05 , simplify = T, split = T))

})
#

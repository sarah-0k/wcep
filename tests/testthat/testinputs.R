library(wcep)

test_that("testinputs",{

  expect_equal(wcep(data.frame(toyexample,1),data.frame(event=c('CHF','DTH','SHK','REMI'), weight = c(0.3,1,0.5,0.2)),
                    alpha = 0.05 , split = FALSE),
               noquote("Error: Data frame x should have 3 columns for one group or 4 columns for two groups comparison"))

  expect_equal(wcep(toyexample[,1:2],data.frame(event=c('CHF','DTH','SHK','REMI'), weight = c(0.3,1,0.5,0.2)),
                    alpha = 0.05 , split = FALSE),
               noquote("Error: Data frame x should have 3 columns for one group or 4 columns for two groups comparison"))

  expect_equal(wcep(toyexample,data.frame(event=c('CHF','DTH','SHK','REMI'),weight = c(0.3,1,0.5,0.2)), alpha = 1.05 , split = FALSE),
               noquote("Error: value of alpha should be between 0 and 1"))

  expect_equal(wcep(toyexample,data.frame(event=c('CHF','DTH','SHK','REMI'),weight = c(0.3,1,0.5,0.2)), alpha = -1.05 , split = FALS),
               noquote("Error: value of alpha should be between 0 and 1"))

  expect_equal(wcep(toyexample,data.frame(event=c('CHF','DTH','SHK','REMI'),weight = c(0.3,1,0.5,0.2)), alpha = 0 , split = FALSE),
               noquote("Error: value of alpha should be between 0 and 1"))

  expect_equal(wcep(toyexample[,1:3],data.frame(event=c('CHF','DTH','SHK','REMI'), weight = c(0.3,1,0.5,0.2)),
                    alpha = 0.05 , split = TRUE),
               noquote("Error: Data frame x should have 4 columns"))

  expect_equal(wcep(data.frame(toyexample[,1:3],sex=c(rep("M",40), rep("F",40), rep("G",24))),
                    data.frame(event=c('CHF','DTH','SHK','REMI'), weight = c(0.3,1,0.5,0.2)),
                    alpha = 0.05 , split = TRUE),
               noquote("Error: The last column should have two levels"))

  expect_equal(wcep(matrix(rep(1,40),ncol=4), data.frame(event=c('CHF','DTH','SHK','REMI'), weight = c(0.3,1,0.5,0.2)),
                    alpha = 0.05),
               noquote("Error: The second column should be factor"))
})
#

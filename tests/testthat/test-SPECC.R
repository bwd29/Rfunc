test_that("Correct return amount", {
  data<-as.numeric(c(1,2,3,4,1,2,3,4,5,6,1,2,3,4,5))
  cl <- SPECC(data, 3, 8)
  expect_equal(length(cl), length(data))
})

test_that("Correct return amount", {
  data<-as.numeric(c(1,2,3,4,1,2,3,4,5,6,1,2,3,4,5))
  cl<-HCLUST(data, 3)
  expect_equal(length(cl), length(data))
})

context("hello.R")

test_that("hello prints stuff", {
  input <- data.frame(sex=c("m", "f"), age=c(25, 32), occupation=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "gender")

  input <- data.frame(gender=c("m", "f"), weight=c(25, 32), occupation=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "age")

  input <- data.frame(gender=c("m", "f"), age=c(25, 32), occupation=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "bmi")

  input <- data.frame(gender=c("e", "f"), age=c(25, 32), bmi=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "gender")
})

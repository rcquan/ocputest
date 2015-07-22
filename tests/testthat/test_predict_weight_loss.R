context("predict_weight_loss.R")

test_that("predict_weight_loss throws error if correct fields not supplied", {
  input <- data.frame(sex=c("m", "f"), age=c(25, 32), occupation=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "gender")

  input <- data.frame(gender=c("m", "f"), weight=c(25, 32), occupation=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "age")

  input <- data.frame(gender=c("m", "f"), age=c(25, 32), occupation=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "bmi")

  input <- data.frame(gender=c("e", "f"), age=c(25, 32), bmi=c("doctor", "engineer"))
  expect_error(predict_weight_loss(input), "gender")
})

test_that("predict_weight_loss returns correct data.frame", {
  input <- data.frame(gender=c("m", "f"), age=c(25, 32), bmi=c(35, 40))
  result <- predict_weight_loss(input)

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 4)
  expect_is(result$week16_loss, "numeric")
  expect_is(result$week16_loss, "numeric")
})

test_that("linear_model is loaded in the environment", {
  expect_is(linear_model, "lm")
})

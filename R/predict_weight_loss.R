#' Predict Week 16 Weight Loss
#'
#' Simple model with three predictors \code{age}, \code{gender}, and \code{bmi}.
#'
#' @export
#' @importFrom stats predict.lm
#' @param input data passed on as \code{newdata} to \code{\link{predict}}
#' @examples mydata <- data.frame(
#'    age=c(24, 54, 32, 75),
#'    gender=c("m", "f", "f", "m"),
#'    bmi=c(30, 35, 40, 41)
#' )
#' predict_weight_loss(mydata)
predict_weight_loss <- function(input){
  #input can either be csv file or data
  newdata <- if (is.character(input) && file.exists(input)) {
    read.csv(input)
  } else {
    as.data.frame(input)
  }
  stopifnot("age" %in% names(newdata))
  stopifnot("gender" %in% names(newdata))
  stopifnot("bmi" %in% names(newdata))

  newdata$age <- as.numeric(newdata$age)
  newdata$bmi <- as.numeric(newdata$bmi)
  stopifnot(newdata$gender %in% c("m", "f"))

  #tv_model is included with the package
  newdata$week16_loss <- as.vector(predict.lm(linear_model, newdata = newdata))
  return(newdata)
}

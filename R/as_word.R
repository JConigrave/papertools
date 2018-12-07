#' as_word
#'
#' Takes a numeral and converts to a word with optional sentence case. Based off the English package.
#' @param x a numeric. Number to convert to english
#' @param sentencecase a character vector. The names to be saved and loaded
#' @export as_word
#' @importFrom english english

as_word = function(x = NULL,
                   sentencecase = T) {
  x = as.character(
    english::english(
      as.numeric
      (x)))

  if(sentencecase==T){
    substr(x,1,1) <- toupper(substr(x,1,1))
  }
  return(x)
}

#' remove_lead
#'
#' Takes in a numeral, removes leading zeros
#' @param x a numeric.
#' @export remove_lead

remove_lead = function(x) sub("^(-)?0[.]",
                              "\\1.", x)

#' logit2prob
#'
#' Converts logits to prob
#' @param logit a numeric. A logit.
#' @export logit2prob

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#' q_alpha
#'
#' Calculates and rounds alphas
#' @param x a dataframe
#' @param round a numeric. Result will be rounded to this number.
#' @export q_alpha
#' @importFrom psych alpha

q_alpha = function(x, round = 2){
  x = psych::alpha(x)$total$std.alpha
    return(round(x, round))
}

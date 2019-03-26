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

#' digits
#'
#' Allows the user to specify the exact number of digits
#' @param x a numeric
#' @param n a numeric. The number of digits to round to.
#' @export digits
digits = function(x, n = 2){
  x = round(x,n)
  trimws(format(round(x, n), nsmall=n))
}

#' round p
#'
#' Rounds p value to specified digits and uses less symbol if result it zero.
#' @param p a p value
#' @param n a numeric. The number of digits to round to.
#' @export round_p
round_p =  function(p, n = 2){
  rounded = digits(p,n)
  if(as.numeric(rounded) == 0){
    rounded = strsplit(rounded,split="")[[1]]
    rounded[length(rounded)] = 1
    rounded = paste(rounded,collapse = "")
  }
  if(p < as.numeric(rounded)){
    rounded = paste0("< ",rounded)
  }
  return(rounded)
}

#' multi_grepl_n
#'
#' Returns the number of times grepl matched a pattern; vectorised.
#' @param pattern vector of character string patterns
#' @param x a vector of strings to match with patterns
#' @param tolower a bool. Indicates whether or not to make each pattern and input vector lowercase
#' @return A vector containing the number of times each pattern was matched
#' @importFrom dplyr %>%
#' @export multi_grepl_n


multi_grepl_n = function(pattern, x, tolower = T) {
  if (tolower) {
    pattern = tolower(pattern)
    x = tolower(x)
  }
  out = lapply(pattern, function(p) {
    sum(grepl(p, x))
  }) %>% unlist
  return(out)
}

#' n_percent
#'
#' Returns the number than meet a condition, and the percentage in brackets
#' @param vector a vector to count within
#' @param x character, the object to count by
#' @param round scalar, the number of digits
#' @param na.rm bool, whether to remove NAs
#' @importFrom dplyr %>%
#' @return a character
#' @export n_percent

n_percent = function(vector, x, round = 2, na.rm=T){

  if(na.rm){
    vector = na.omit(vector)
  }

  n = vector[which(vector == x)] %>%
    length
  total_length = length(vector)
  percent = (n / total_length) %>%
    "*"(100) %>%
    digits(round)
  out = glue_bracket(as.character(n),percent, brackets = c("(","%)"))
  return(out)
}

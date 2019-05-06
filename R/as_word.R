#' as_word
#'
#' Takes a numeral and converts to a word with optional sentence case. Based off the English package.
#' @param x a numeric. Number to convert to english
#' @param sentencecase a Bool. If true, the first letter is capitalised
#' @param hyphenate a Bool. If true, compound numbers are hyphenated
#' @importFrom dplyr %>%
#' @export as_word
#' @importFrom english english

as_word = function(x = NULL,
                   sentencecase = F,
                   hyphenate = T) {
  x = as.character(english::english(as.numeric
                                    (x)))
  if (sentencecase == T) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  }

  if (hyphenate) {
    compounds = english::english(1:9) %>%
      paste("y", .)
    compounds_hyphen = english::english(1:9) %>%
      paste("y", ., sep = "-")

    for (n in seq_along(compounds)) {
      x <- gsub(compounds[n], compounds_hyphen[n], x)
    }

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
#' @importFrom stats na.omit
#' @return a character
#' @export n_percent

n_percent = function(vector, x, round = 2, na.rm=F){

  if(na.rm){
    vector = na.omit(vector)
  }

  n = vector[which(vector == x)] %>%
    length
  total_length = length(vector)
  percent = (n / total_length) %>%
    "*"(100) %>%
    digits(round)
  out = glue_bracket(as.character(n),percent, brackets = c("(","%)"), round = NULL)
  return(out)
}

#' hash_replace
#'
#' Finds are replaces hash codes, replaces with objects of the same name
#' @param string text to search in
#' @param sample default NULL. If provided, samples a vector
#' @param envir the environment to call replacements from. Defaults to parent frame
#' @export hash_replace

hash_replace = function(string,sample = NULL, envir = parent.frame()){
  while(grepl(".*##(.+)##.*",string)){
    hash = gsub(".*##(.+)##.*", "\\1", string)
    replace = eval(parse(text = hash), envir = envir)

    if(!is.null(sample)) {
      replace = sample(replace, sample)
    }

    string = gsub(paste0("##",hash,"##"),replace,string)
  }
  return(string)
}
